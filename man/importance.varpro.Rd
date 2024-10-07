\name{importance.varpro}
\alias{importance.varpro}
\alias{importance}
\alias{beta.varpro}
\alias{get.vimp}
\alias{get.orgvimp}
\alias{get.topvars}
\title{Calculate VarPro Importance}

\description{
 Calculates variable importance using results from previous varpro call.
}

\usage{\method{importance}{varpro}(o, local.std = TRUE, y.external = NULL,
  cutoff = 0.79, trim = 0.1, plot.it = FALSE, conf = TRUE, sort = TRUE,
  ylab = if (conf) "Importance" else "Standardized Importance",
  max.rules.tree, max.tree,
  papply = mclapply,
  ...)
}

\arguments{
  
  \item{o}{varpro object obtained from previous call to \code{varpro}.}

  \item{local.std}{Use locally standardized importance?}

  \item{y.external}{User supplied external y value.  Must match the
  dimension and be appropriate for the family.}
  
  \item{cutoff}{Cut-off used to highlight significant variables in the
   importance graphical plot.  Only applies when option
   \code{plot.it='TRUE'}.}
    
  \item{trim}{Used for setting windsorized trim value to robustify mean
  and standard deviation values.}

  \item{plot.it}{Plot the importance values?}

  \item{conf}{Show importance values with standard errors as a boxplot
    (thus showing an informal confidence region)?  If \code{conf='FALSE'},
    plots standardized importance.}

  \item{sort}{Sort the results in decreasing order?}
  
  \item{ylab}{Label used for vertical axis.}
  
  \item{max.rules.tree}{Optional. Maximum number of rules per tree.
    If left unspecified, uses the value from the VarPro object.}

  \item{max.tree}{Maximum number of trees used for extracting rules.
    If left unspecified, uses the value from the VarPro object.}

  \item{papply}{Use mclapply or lapply.}
  
  \item{...}{Additional options.}
  
}

\details{

  Calculates split-sample mean and standard deviation importance values
  using the results from a previous varpro call and converts these to
  standardized importance values from which variables are selected.

}

\value{

  Invisibly, table summarizing the results.  Contains mean importance
  'mean', the standard deviation 'std', and standardized importance 'z'.

  For classification, conditional 'z' tables are additionally provided,
  where the $z$ standardized importance values are conditional on the
  class label.
  
  See \command{cv.varpro} for a data driven cross-validation method for
  selecting the cutoff value, \code{cutoff}.
  
}

\author{

  Min Lu and Hemant Ishwaran

}

\references{

  Lu, M. and Ishwaran, H., (2024). Model-independent variable selection
  via the rule-based variable priority. arXiv e-prints, pp.arXiv-2409.
  
   
}

\seealso{
  \command{\link{cv.varpro}}
  \command{\link{varpro}}
}

\examples{
\donttest{

## ------------------------------------------------------------
## regression example
## ------------------------------------------------------------

data(BostonHousing, package = "mlbench")

## call varpro
o <- varpro(medv~., BostonHousing)

## extract importance values
imp <- importance(o)
print(imp)

## plot the results
imp <- importance(o, plot.it = TRUE)
print(imp)


## ------------------------------------------------------------
## illustrates y-external: regression example
## ------------------------------------------------------------

## friedman1 - standard application of varpro
d <- data.frame(mlbench:::mlbench.friedman1(250),noise=matrix(runif(250*10,-1,1),250))
o <- varpro(y~.,d)
print(importance(o))

## importance using external rf predictor
print(importance(o,y.external=randomForestSRC::rfsrc(y~.,d)$predicted.oob))

## importance using external lm predictor
print(importance(o,y.external=lm(y~.,d)$fitted))

## importance using external randomized predictor
print(importance(o,y.external=runif(nrow(o$x),-1,1)))

## ------------------------------------------------------------
## illustrates y-external: classification example
## ------------------------------------------------------------

## iris - standard application of varpro
o <- varpro(Species~.,iris)
print(importance(o))

## importance using  external rf predictor
print(importance(o,y.external=randomForestSRC::rfsrc(Species~.,iris)$class.oob))

## importance using  external randomized predictor
print(importance(o,y.external=sample(o$y)))

}
}
\keyword{varpro}
