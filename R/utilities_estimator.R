varpro.estimator <- function(y,
                        idx1,
                        idx2,
                        var.y = NULL,
                        classObj = NULL,
                        delta = 1e-6)
{
  ## get sample size
  n <- length(y[idx1])
  ## y is real-valued ---> mse
  if (!is.factor(y) && !is.matrix(y)) {
    ## bail out if subsetted y has zero length
    if (n == 0) {
      return(list(est = NA, n = 0))
    }
    if (is.null(var.y)) {
      var.y <- 1
    }
    ## estimator calculation
    est <- abs(mean(y[idx2], na.rm = TRUE) - mean(y[idx1], na.rm = TRUE)) / sqrt(var.y)
    return(list(est = est, n = n))
  }
  ## y is a real-valued matrix ---> mv-mse
  if (!is.factor(y) && is.matrix(y)) {
    ## set dimensions
    n <- nrow(y)
    J <- ncol(y)
    ## bail out if subsetted y has zero length
    if (n == 0) {
      return(list(est = rep(NA, J), n = n))
    }
    if (is.null(var.y)) {
      var.y <- rep(1, J)
    }
    ## estimator calculation
    est <- sapply(1:J, function(j) {
      abs(mean(y[idx2, j], na.rm = TRUE) - mean(y[idx1, j], na.rm = TRUE)) / sqrt(var.y[j])
    })
    return(list(est = est, n = n))
  }
  ## y is a factor --> get "all" performance and J-class performance, a J+1 vector
  else {
    ## bail out if subsetted y has zero length
    if (n == 0) {
      return(list(est = rep(NA, 1 + classObj$J), n = rep(0, 1 + classObj$J))) 
    }
    ## sample size calculation
    y <- as.numeric(y)
    J <- classObj$J
    f1 <- tapply(y[idx1], y[idx1], length) 
    f2 <- tapply(y[idx2], y[idx2], length) 
    frq1 <- frq2 <- rep(0, J)
    names(frq1) <- names(frq2) <- 1:J
    frq1[names(f1)] <- f1 
    frq2[names(f2)] <- f2
    n <- c(n, pmax(frq1, frq2))
    ## data based (default) conditional probability calculations
    if (is.null(classObj$phat)) {
      prb1 <- prb2 <- rep(0, J)
      names(prb1) <- names(prb2) <- 1:J
      prb1[names(f1)] <- f1 / length(y[idx1])
      prb2[names(f2)] <- f2 / length(y[idx2])
    }
    ## model based conditional probability calculations
    if (!is.null(classObj$phat)) {
      prb1 <- colMeans(classObj$phat[idx1,, drop = FALSE], na.rm = TRUE)
      prb2 <- colMeans(classObj$phat[idx2,, drop = FALSE], na.rm = TRUE)
    }
    ## final metric calculation
    est.all <- mean(abs(prb2 - prb1), na.rm = TRUE)
    est.class <- abs(prb2 - prb1)
    majority.class <- resample(which(est.class == max(est.class)), 1)
    est.class[-majority.class] <- 0
    est <- c(est.all, est.class)
    return(list(est = est, n = n))
  }
}
