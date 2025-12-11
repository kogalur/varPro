## rmst
sIndex <- function(x,y) {sapply(1:length(y), function(j) {sum(x <= y[j])})}
get.rmst <- function(o, tau.horizon = NULL) {

  ## incoming parameter checks
  if (is.null(o)) {
    return(NULL)
  }
  if (o$family != "surv") {
    stop("this function only supports right-censored survival settings")
  }
  if (sum(inherits(o, c("rfsrc", "grow"), TRUE) == c(1, 2)) != 2 &
      sum(inherits(o, c("rfsrc", "predict"), TRUE) == c(1, 2)) != 2) {
    stop("This function only works for objects of class `(rfsrc, grow)' or '(rfsrc, predict)'")
  }

  ## extract time, survival (use OOB values if available)
  time <- o$time.interest
  if (is.null(o$survival.oob)) {
    surv <- o$survival.oob
  }
  else {
    surv <- o$survival
  }

  ## set the time horizon
  if (is.null(tau.horizon)) {
    tau.horizon <- max(time, na.rm = TRUE)
  }

  
  ## calculate the rmst
  rmst.lst <- lapply(tau.horizon, function(tau) {

    ## adjustment for when time doesn't include tau.horizon
    etime <- sort(unique(c(time, tau)))
    surv <- cbind(1, surv)[, 1 + sIndex(time, etime)]
    time <- etime

    ## restrict time to tau horizon
    time.pt <- time <= tau

    ## calculate rmst for the restricted time
    c(surv[, time.pt, drop = FALSE] %*% diff(c(0, time[time.pt])))

  })
  
  ## usual scenario is that tau horizon is length one
  if (length(rmst.lst) == 1) {
    return(rmst.lst[[1]])
  }
  ## tau horizon has length > 1: user wants multivariate regression forests
  else {
    return(do.call(cbind, rmst.lst))
  }
  

}
