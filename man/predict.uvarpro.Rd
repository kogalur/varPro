\name{predict.uvarpro}
\alias{predict.uvarpro}
\title{Prediction on Test Data using Unsupervised VarPro}

\description{
 Obtain predicted values on test data for unsupervised forests.
}

\usage{\method{predict}{uvarpro}(object, newdata, ...)}

\arguments{

  \item{object}{Unsupervised VarPro object from a previous call to \code{uvarpro}. Only applies if \code{method = "auto"} was used.}

  \item{newdata}{Optional test data. If not provided, the training data is used.}

  \item{...}{Additional arguments passed to internal methods.}

}

\details{

Applies to unsupervised VarPro objects built using the autoencoder (\code{method = "auto"}). The object contains a multivariate random forest used to generate predictions for the test data.

Returns a matrix of predicted values, where each column corresponds to a feature (with one-hot encoding applied). The result includes the following attributes:

\enumerate{

  \item \code{mse}: Standardized mean squared error averaged across features.

  \item \code{mse.all}: Standardized mean squared error for each individual feature.

}

}



\author{

  Min Lu and Hemant Ishwaran

}

\seealso{
  \command{\link{uvarpro}}
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
o <- uvarpro(data=BostonHousing)

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
o <- uvarpro(d[1:20,])

## predicted values on test data
print(predict(o, d[-(1:20),]))

## predicted values on bad test data with strange factor values 
dbad <- d[-(1:20),]
dbad$carb <- as.character(dbad$carb)
dbad$carb <-  sample(LETTERS, size = nrow(dbad))
print(predict(o, dbad))

}
}

\keyword{predict uvarpro}
