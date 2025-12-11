predict.varpro <- function(object, newdata, ...) {

  ## check coherence: failure is fatal
  if (!inherits(object, "varpro")) {
    stop("object must be a varpro object")
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
  get.mv.predicted(predict.rfsrc(object$rf, newdata, ...), oob = TRUE)

}


