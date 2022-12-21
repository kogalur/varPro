## custom sampler
customSampler <- function(n, x, uniform = TRUE, ndiscrete = 10) {
  ## if discrete or factor then uniform sampling is not appropriate
  if (is.factor(x) || length(unique(x)) <= ndiscrete) {
    uniform <- FALSE
  }
  ## uniform sampling
  if (uniform) {
    if (length(x) > 1) {
      runif(n, min = min(x, na.rm = TRUE), max = max(x, na.rm = TRUE)) 
    }
    else {
      x
    }
  }
  ## empirical distribution
  else {
    resample(x, size = n, replace = TRUE)
  }
}
## hot-encoding
get.hotencode <- function(x, papply = mclapply) {
  anyF <- sapply(x, is.factor)
  if (sum(anyF) > 0) {
    x.f <- do.call(cbind, papply(names(anyF[anyF]), function(nn) {
      xn <- x[, nn]
      ## one-level factors are converted to zero
      if (length(levels(xn)) == 1) {
        as.numeric(xn) - 1
      }
      ## two-level factors are converted to binary
      else if (length(levels(xn)) == 2) {
        xn <- data.frame(as.numeric(factor(xn, labels = c(0, 1))) - 1)
        colnames(xn) <- nn
        xn
      }
      else {
        f <- as.formula(paste0("~ -1 +", nn))
        xn <- data.frame(xn)
        colnames(xn) <- nn
        model.matrix(f, xn)
      }
    }))
    x <- data.frame(x[, !anyF, drop = FALSE], x.f)
  }
  x
}
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
## robust sample
resample <- function(x, ...) x[sample.int(length(x), ...)]
##  winsorized statistics
winsorize <- function (x, trim = 0.1, na.rm = TRUE) {
  if ((trim < 0) | (trim > 0.5)) 
    stop("trimming must be reasonable")
  qtrim <- quantile(x, c(trim, 0.5, 1 - trim), na.rm = na.rm)
  xbot <- qtrim[1]
  xtop <- qtrim[3]
  if (trim < 0.5) {
    x[x < xbot] <- xbot
    x[x > xtop] <- xtop
  }
  else {
    x[!is.na(x)] <- qtrim[2]
  }
  return(x)
}
winsorize.sd <- function (x, trim = 0.1, na.rm = TRUE) {
  if ((trim < 0) | (trim >= 0.5)) {
    stop("trimming must be reasonable")
  }
  sqrt(var(winsorize(x, trim = trim, na.rm = na.rm), na.rm = na.rm))
}
winsorize.mean <- function (x, trim = 0.1, na.rm = TRUE) {
  if ((trim < 0) | (trim > 0.5)) 
    stop("trimming must be reasonable")
  if (trim < 0.5) {
    return(mean(winsorize(x, trim = trim, na.rm = na.rm), na.rm = na.rm))
  }
  else {
    return(median(x, na.rm = na.rm))
  }
}
