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
  ## Use full-ensemble survival, as in VarPro's external-estimator step.
  ## An OOB-only object can supply the survival matrix when needed.
  time <- o$time.interest
  surv <- o[["survival"]]
  if (is.null(surv)) {
    surv <- o[["survival.oob"]]
  }
  if (is.null(surv)) {
    stop("object contains neither survival nor survival.oob estimates")
  }
  if (!is.numeric(time) || !length(time) || any(!is.finite(time)) ||
      any(time < 0) || is.unsorted(time)) {
    stop("time.interest must contain ordered, finite, nonnegative times")
  }
  if (!is.matrix(surv) || !is.numeric(surv) || nrow(surv) < 1L ||
      ncol(surv) != length(time)) {
    stop("survival estimates must be a numeric matrix with one column per time point")
  }
  ## set the time horizon
  if (is.null(tau.horizon)) {
    tau.horizon <- max(time)
  }
  if (!is.numeric(tau.horizon) || !length(tau.horizon) ||
      any(!is.finite(tau.horizon)) || any(tau.horizon < 0)) {
    stop("tau.horizon must contain finite, nonnegative numeric values")
  }
  tau.horizon <- as.numeric(tau.horizon)
  ## calculate the rmst, preserving the requested horizon order
  rmst.lst <- lapply(tau.horizon, function(tau) {
    ## Integrate each interval using survival at its left endpoint.
    breaks <- sort(unique(c(0, time[time < tau], tau)))
    left <- breaks[-length(breaks)]
    surv.left <- cbind(1, surv)[, 1L + findInterval(left, time), drop = FALSE]
    c(surv.left %*% diff(breaks))
  })
  ## Label responses where they are calculated. For a single horizon the
  ## numeric vector retains its shape; its response identity is an attribute.
  rmst.info <- data.frame(
    source.response = paste0("RMST(", tau.horizon, ")"),
    tau.horizon = tau.horizon,
    stringsAsFactors = FALSE)
  if (length(rmst.lst) == 1L) {
    out <- rmst.lst[[1L]]
  }
  else {
    out <- do.call(cbind, rmst.lst)
    colnames(out) <- rmst.info$source.response
  }
  attr(out, "rmst.info") <- rmst.info
  out
}
