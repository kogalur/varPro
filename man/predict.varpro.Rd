\name{predict.varpro}
\alias{predict.varpro}
\title{Prediction on Test Data using VarPro}

\description{
 Obtain predicted values on test data for VarPro object.
}

\usage{\method{predict}{varpro}(object, newdata, ...)}

\arguments{
  
  \item{object}{VarPro object obtained from previous call to \code{varpro}.}
  
  \item{newdata}{Test data. If not provided the training data is used.}

  \item{...}{Additional options.}

}

\details{

  
  The VarPro method uses the rules obtained from a random forest
  constructed by guided feature selection where variables are selected
  according to a split-weight obtained in a pre-processing step using
  lasso+trees.

  This function returns the predicted values for this random forest on
  the provided test data, \code{newdata} (if not provided, predicted
  values are the out-of-bag predicted values for the training data).
  
}

\author{

  Min Lu and Hemant Ishwaran

}

\references{

  Lu M. and Ishwaran H. (2022).  Model-Independent Variable Selection
  via the Rule-based Variable Priority (VarPro).


}

\seealso{
  \command{\link{varpro}}
}

\examples{
\donttest{
## ------------------------------------------------------------
##
## boston housing regression
## obtain predicted values for the training data
##
## ------------------------------------------------------------

## varpro applied to boston housing data
data(BostonHousing, package = "mlbench")
o <- varpro(medv~., BostonHousing)

## predicted values for the training features
print(head(predict(o)))

## ------------------------------------------------------------
##
## iris classification
## obtain predicted values for test data
##
## ------------------------------------------------------------

## varpro applied to iris data
trn <- sample(1:nrow(iris), size = 100, replace = FALSE)
o <- varpro(Species~., iris[trn,])

## predicted values on test data
print(data.frame(Species=iris[-trn, "Species"], predict(o, iris[-trn,])))

## ------------------------------------------------------------
##
## mtcars regression
## obtain predicted values for test data
## also illustrates hot-encoding working on test data
##
## ------------------------------------------------------------

## mtcars with some factors
d <- data.frame(mpg=mtcars$mpg,lapply(mtcars[, c("cyl", "vs", "carb")], as.factor))

## varpro on training data 
o <- varpro(mpg~., d[1:20,])

## predicted values on test data
print(predict(o, d[-(1:20),]))

## predicted values on bad test data with strange factor values 
dbad <- d[-(1:20),]
dbad$carb <- as.character(dbad$carb)
dbad$carb <-  sample(LETTERS, size = nrow(dbad))
print(predict(o, dbad))

}
}

\keyword{predict varpro}
