\name{predict.unsupv.varpro}
\alias{predict.unsupv.varpro}
\alias{predict.unsupv}
\title{Prediction on Test Data using Unsupervised VarPro}

\description{
 Obtain predicted values on test data for unsupervised forests.
}

\usage{\method{predict.unsupv}{varpro}(object, newdata, ...)}

\arguments{
  
  \item{object}{Unsupervised VarPro object from previous call to
    \code{unsupv}.  Only applies if \code{method}="auto" was used.}
  
  \item{newdata}{Test data. If not provided the training data
  is used.}

  \item{...}{Additional options.}

}

\details{

  Applies to unsupervised VarPro objects built using autoencoder
  (\code{method}="auto").  The unsupervised object contains a
  multivariate random forest which is used to predict on test data.

  Returns a matrix containing predicted values where each column is predicted
  value for a feature (hot-encoding is applied) and with attributes:

  \enumerate{
    
    \item \code{mse}: standardized mean-squared error averaged across
    feature.

    \item \code{mse.all}: standardized mean-squared error for each
    feature.

  }
  
}

\author{

  Min Lu and Hemant Ishwaran

}

\seealso{
  \command{\link{unsupv.varpro}}
}

\examples{
\donttest{
## ------------------------------------------------------------
##
## boston housing
## obtain predicted values for the training data
##
## ------------------------------------------------------------

## unsupervised varpro on boston housing
data(BostonHousing, package = "mlbench")
o <- unsupv(data=BostonHousing)

## predicted values for the training features
print(head(predict(o)))

## ------------------------------------------------------------
##
## mtcars
## obtain predicted values for test data
## also illustrates hot-encoding working on test data
##
## ------------------------------------------------------------

## mtcars with some factors
d <- data.frame(mpg=mtcars$mpg,lapply(mtcars[, c("cyl", "vs", "carb")], as.factor))

## training 
o <- unsupv(d[1:20,])

## predicted values on test data
print(predict(o, d[-(1:20),]))

## predicted values on bad test data with strange factor values 
dbad <- d[-(1:20),]
dbad$carb <- as.character(dbad$carb)
dbad$carb <-  sample(LETTERS, size = nrow(dbad))
print(predict(o, dbad))

}
}

\keyword{predict unsupervised}
