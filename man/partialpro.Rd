\name{partialpro}
\alias{partialpro}
\title{Partial Effects for Variable(s)}

\description{
 Obtain the partial effect of x-variables from a VarPro analysis.
}

\usage{
partialpro(object, xvar.names, nvar,
           target, learner, newdata, method = c("unsupv", "rnd", "auto"),
           verbose = FALSE, papply = mclapply, ...)
}

\arguments{

  \item{object}{\code{varpro} object returned from a previous call to \code{varpro}.}

  \item{xvar.names}{Names of the x-variables to use.}

  \item{nvar}{Number of variables to include. Defaults to all.}

  \item{target}{For classification, specifies the class for which the partial effect is computed. Can be an integer or character label. Defaults to the last class.}

  \item{learner}{Optional function specifying a user-defined prediction model. See \code{Details}.}

  \item{newdata}{Optional data frame containing test features. If not provided, the training data is used.}

  \item{method}{Isolation forest method used for Unlimited Virtual Twins (UVT). Options are \code{"unsupv"} (default), \code{"rnd"} (pure random splitting), and \code{"auto"} (autoencoder). See \code{isopro} for details.}

  \item{verbose}{Print verbose output?}

  \item{papply}{Parallel apply method; typically \code{mclapply} or \code{lapply}.}

  \item{...}{Additional hidden options: \code{"cut"}, \code{"nsmp"},
  \code{"nvirtual"}, \code{"nmin"}, \code{"alpha"}, \code{"df"},
  \code{"sampsize"}, \code{"ntree"}, \code{"nodesize"},
  \code{"mse.tolerance"}.}


}

\details{

Computes partial effects for selected variables based on a VarPro
analysis. If a variable was filtered out during VarPro (e.g., due to
noise), its partial effect cannot be computed.
  
 
Partial effects are derived using predictions from the forest built
during VarPro. These predictions are restricted using Unlimited Virtual
Twins (UVT), which apply an isolation forest criterion to filter
unlikely combinations of partial data. The filtering threshold is
governed by the internal \code{cut} parameter. Isolation forests are
constructed via \code{isopro}.

Interpretation of partial effects depends on the outcome type:
\itemize{
  \item For regression: effects are on the response scale.
  \item For survival: effects are either on mortality (default) or RMST (if specified in the original \code{varpro} call).
  \item For classification: effects are log-odds for the specified \code{target} class.
}

Partial effects are estimated locally using polynomial linear models fit
to the predicted values. The degrees of freedom for the local model are
controlled by the \code{df} option (default = 2, i.e., quadratic).


By default, predictions use the forest from the VarPro
object. Alternatively, users may supply a custom prediction function via
\code{learner}. This function should accept a data frame of features and
return:
\itemize{
  \item A numeric vector for regression or survival outcomes.
  \item A matrix of class probabilities (one column per class, in original class order) for classification.
  \item If \code{newdata} is missing, the function should return predictions on the original training data.
}

See the examples for use cases with external learners, including:
\enumerate{
  \item Random forest (external to VarPro),
  \item Gradient tree boosting,
  \item Bayesian Additive Regression Trees (BART).
}

}

\author{

  Min Lu and Hemant Ishwaran

}

\references{
  
  Ishwaran H. (2025).  Multivariate Statistics: Classical Foundations
  and Modern Machine Learning, CRC (Chapman and Hall), in press.

}

\seealso{
  \command{\link{varpro}}
  \command{\link{isopro}}
}

\examples{
##------------------------------------------------------------------
##
## Boston housing
##
##------------------------------------------------------------------

library(mlbench)
data(BostonHousing)
par(mfrow=c(2,3))
plot((oo.boston<-partialpro(varpro(medv~.,BostonHousing),nvar=6)))
\donttest{

##------------------------------------------------------------------
##
## Boston housing using newdata option
##
##
##------------------------------------------------------------------

library(mlbench)
data(BostonHousing)
o <- varpro(medv~.,BostonHousing)
par(mfrow=c(2,3))
plot(partialpro(o,nvar=3))
## same but using newdata (set to first 6 cases of the training data)
plot(partialpro(o,newdata=o$x[1:6,],nvar=3))

##------------------------------------------------------------------
##
## Boston housing with externally constructed rf learner
##
##------------------------------------------------------------------

## varpro analysis
library(mlbench)
data(BostonHousing)
o <- varpro(medv~.,BostonHousing)

## default partial pro call
pro <- partialpro(o, nvar=3)

## partial pro call using built in rf learner
mypro <- partialpro(o, nvar=3, learner=rf.learner(o))

## compare the two
par(mfrow=c(2,3))
plot(pro)
plot(mypro, ylab="external rf learner")

##------------------------------------------------------------------
##
## Boston housing:  tree gradient boosting learner, bart learner
##
##------------------------------------------------------------------

if (library("gbm", logical.return=TRUE) &&
    library("BART", logical.return=TRUE)) {

## varpro analysis
library(parallel)
library(mlbench)
data(BostonHousing)
o <- varpro(medv~.,BostonHousing)

## default partial pro call
pro <- partialpro(o, nvar=3)

## partial pro call using built in gradient boosting learner
## mypro <- partialpro(o, nvar=3, learner=gbm.learner(o, n.trees=1000, n.cores=detectCores()))

## The only way to pass check-as-cran
mypro <- partialpro(o, nvar=3, learner=gbm.learner(o, n.trees=1000, n.cores=2))

## partial pro call using built in bart learner
## mypro2 <- partialpro(o, nvar=3, learner=bart.learner(o, mc.cores=detectCores()))

## The only way to pass check-as-cran
mypro2 <- partialpro(o, nvar=3, learner=bart.learner(o, mc.cores=2))

## compare the learners
par(mfrow=c(3,3))
plot(pro)
plot(mypro, ylab="external boosting learner")
plot(mypro2, ylab="external bart learner")

}

##------------------------------------------------------------------
##
## peak vo2 with 5 year rmst
##
##------------------------------------------------------------------

data(peakVO2, package = "randomForestSRC")
par(mfrow=c(2,3))
plot((oo.peak<-partialpro(varpro(Surv(ttodead,died)~.,peakVO2,rmst=5),nvar=6)))

##------------------------------------------------------------------
##
## veteran data set with celltype as a factor
##
##------------------------------------------------------------------

data(veteran, package = "randomForestSRC")
dta <- veteran
dta$celltype <- factor(dta$celltype)
par(mfrow=c(2,3))
plot((oo.veteran<-partialpro(varpro(Surv(time, status)~., dta), nvar=6)))

##------------------------------------------------------------------
##
## iris: classification analysis showing partial effects for all classes
##
##------------------------------------------------------------------

o.iris <- varpro(Species~.,iris)
yl <- paste("log-odds", levels(iris$Species))
par(mfrow=c(3,2))
plot((oo.iris.1 <- partialpro(o.iris, target=1, nvar=2)),ylab=yl[1])
plot((oo.iris.2 <- partialpro(o.iris, target=2, nvar=2)),ylab=yl[2])
plot((oo.iris.3 <- partialpro(o.iris, target=3, nvar=2)),ylab=yl[3])


##------------------------------------------------------------------
##
## iowa housing data
##
##------------------------------------------------------------------

## quickly impute the data; log transform the outcome
data(housing, package = "randomForestSRC")
housing <- randomForestSRC::impute(SalePrice~., housing, splitrule="random", nimpute=1)
dta <- data.frame(data.matrix(housing))
dta$y <- log(housing$SalePrice)
dta$SalePrice <- NULL

## partial effects analysis
o.housing <- varpro(y~., dta, nvar=Inf)
oo.housing <- partialpro(o.housing,nvar=15)
par(mfrow=c(3,5))
plot(oo.housing)

}
}
\keyword{plot}
