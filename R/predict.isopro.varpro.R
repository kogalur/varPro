predict.isopro.varpro <- function(object, newdata, quantiles = TRUE, ...) {
  ## must be an isopro object
  if (!inherits(object, "isopro", TRUE)) {
    stop("object must be an isopro object")
  }
  ## if test data missing revert to original data
  if (missing(newdata)) {
    newdata <- object$isoforest$xvar
  }
  ## test case depth values
  test.case.depth <- colMeans(predict.rfsrc(object$isoforest,
            newdata, case.depth = TRUE)$case.depth, na.rm = TRUE)
  ## return the howbad quantile
  if (quantiles) {
    object$cdf(test.case.depth)
  }
  else {
    test.case.depth
  }
}
predict.isopro <- predict.isopro.varpro
