predict.uvarpro <- function(object, newdata, ...) {
  ## check coherence: failure is fatal
  if (!inherits(object, "uvarpro")) {
    stop("object must be an 'uvarpro' varpro object")
  }
  if (object$rf$family != "regr+") {
    stop("only applies to unsupervised varpro objects using auto-encoder")
  }
  ## if test data missing revert to original data
  if (missing(newdata)) {
    newdata <- object$x
  }
  ## otherwise hot-encode it
  else {
    newdata <- get.hotencode.test(object$x, newdata)
  }
  ## predict on newdata (use training data otherwise)
  oo <- predict.rfsrc(object$rf, newdata)
  xhat <- get.mv.predicted(oo, oob = TRUE)
  colnames(xhat) <- oo$xvar.names
  ## standardized mse values
  mse.all <- colMeans((xhat - oo$xvar)^2, na.rm = TRUE) / apply(oo$xvar, 2, var, na.rm = TRUE)
  mse.all[is.infinite(mse.all)] <- NA
  mse <- mean(mse.all, na.rm = TRUE)
  ## return the goodies
  attr(xhat, "mse") <- mse
  attr(xhat, "mse.all") <- mse.all
  xhat
}
