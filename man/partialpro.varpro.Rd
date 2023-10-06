\name{partialpro.varpro}
\alias{partialpro.varpro}
\alias{partialpro}
\alias{gbm.learner}
\alias{rf.learner}
\alias{bart.learner}
\title{Partial Effects for Variable(s)}

\description{
 Obtain the partial effect of x-variables from a VarPro analysis.
}

\usage{\method{partialpro}{varpro}(object, xvar.names, nvar,
  target, learner, method = c("unsupv", "rnd", "auto"),
  verbose = FALSE, papply = mclapply, ...)
}

\arguments{
  
  \item{object}{VarPro object obtained from previous call to \code{varpro}.}

  \item{xvar.names}{Names of the x-variables to be used.}

  \item{nvar}{Number of variables to be used. Default is all.}
  
  \item{target}{For classification, an integer or character value
    specifying the class that the partial effect is computed for
    (defaults to the last class).}

  \item{learner}{Optional function for passing a user's personally
     constructed learner (see details below).}

   \item{method}{Isolation forest method used for unlimited virtual
    twins.  Choices are "unsupv" (unsupervised analysis, the default),
    "rnd" (pure random splitting) and "auto" (auto-encoder, a type of
    multivariate forest).  See \command{isopro} for details.}
  
  \item{verbose}{Print verbose output?}
  
  \item{papply}{Use mclapply or lapply.}
  
  \item{...}{Additional hidden options: "cut", "nsmp", "nvirtual",
    "nmin", "alpha", "df", "sampsize", "ntree", "nodesize",
    "mse.tolerance".}
  
}

\details{

  Obtain partial effects for requested variables from a VarPro analysis.
  Some variables will have been filtered in the VarPro analysis and in
  such cases their partial effects cannot be obtained (as filtered
  variables are considered to be noisy variables).

  Partial effects are obtained using predicted values from the random
  forest object built during the VarPro analysis.  Unlimited
  Virtual Twins (UVT) are used to restrict the prediction call to
  partial plot data (i.e. the artificially created data used for the
  partial effects estimation) that meet an isolation forest criteria
  specified by the internal option \code{cut}, a small percentile
  value which filters unlikely to occur partial data.  Isolation
  forests are constructed via the function \command{isopro}.
  
  For regression, the partial effect is on the scale of the response;
  for survival it is either mortality (default) or RMST (the latter when
  the user specifies this in the original \link{varpro} call: see
  examples below); for classification it is the log-odds for the
  specified target class label (see examples below).

  Partial effects are estimated locally using polynomial linear models
  fit to the predicted values from the learner.  Degrees of freedom of
  the local polynomial model is set by option \code{df} (the default is
  2 corresponding to a quadratic model).

  By default, the random forest built during the VarPro analysis is used
  for prediction, however users can provide their own predictor using
  option \code{learner}.  The user's learner should be trained using the
  training data used in the varpro analysis.  \code{learner} then takes
  the form of a function whose input is a data frame containing feature
  values and whose output is the predicted value from the user's
  learner.  For regression and survival, the output should be a vector.
  For classification, the output should be matrix whose columns are the
  predicted probability for each class label given in the order of the
  original class outcome. If the input is missing, the function should
  return the predicted values for the original training data.  See the
  examples below which illustrate using an external: (1) random forest
  learner (which can sometimes be better than the random forests learner built
  during the VarPro analysis); (2) gradient tree boosting learner; and
  (3) BART (Bayesian Adaptive Regression Trees).
  
}

\author{

  Min Lu and Hemant Ishwaran

}

\references{
  
  Lu M. and Ishwaran H. (2022).  Model-Independent Variable Selection
  via the Rule-based Variable Priority (VarPro).

   Lu M. and  Ishwaran H. (2023).  Unlimited Virtual Twins.

}

\seealso{
  \command{\link{varpro}}
  \command{\link{isopro.varpro}}
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
library(mlbench)
data(BostonHousing)
o <- varpro(medv~.,BostonHousing)

## default partial pro call
pro <- partialpro(o, nvar=3)

## partial pro call using built in gradient boosting learner
mypro <- partialpro(o, nvar=3, learner=gbm.learner(o, n.trees=1000))

## partial pro call using built in bart learner
mypro2 <- partialpro(o, nvar=3, learner=bart.learner(o))

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
housing <- impute(SalePrice~., housing, splitrule="random", nimpute=1)
dta <- housing
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
