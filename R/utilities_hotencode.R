## hot-encoding
get.hotencode <- function(x, papply = mclapply) {
  anyF <- sapply(x, is.factor) | sapply(x, is.character)
  ## factors are present -> hotencode them
  if (sum(anyF) > 0) {
    x.f <- do.call(cbind, papply(names(anyF[anyF==TRUE]), function(nn) {
      ## pull the feature: force to factor
      xn <- factor(x[, nn])
      ## one-level factors are converted to zero
      if (length(levels(xn)) == 1) {
        rep(0, length(xn))
      }
      ## two-level factors are converted to binary
      else if (length(levels(xn)) == 2) {
        #xn <- data.frame(as.numeric(factor(xn)) - 1)
        xn <- data.frame(as.numeric(xn) - 1)
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
    ## store useful information from original data
    xvar.names <- colnames(x)
    xlevels <- lapply(x[, anyF, drop = FALSE], function(x){levels(as.factor(x))})
    ## package up as data frame, store useful attributes
    x <- data.frame(x[, !anyF, drop = FALSE], x.f)
    attr(x, "hotencode") <- TRUE
    attr(x, "xvar.names") <- xvar.names
    attr(x, "levels") <- xlevels
  }
  ## no hotencoding performed
  else {
    attr(x, "hotencode") <- FALSE
    attr(x, "xvar.names") <- colnames(x)
  }
  x
}
## hot-encoding for test data
get.hotencode.test <- function(x, xtest, papply = mclapply, raw = FALSE) {
  ## pull the original variable names (which may not be the same as colnames of x)
  xvar.names <- attr(x, "xvar.names")
  ## confirm test data coherence
  if (length(intersect(xvar.names, names(xtest))) != length(xvar.names)) {
    stop("variable names from test data do not match training data\n")
  }
  ## restrict columns of test data to training data
  xtest <- xtest[, intersect(xvar.names, names(xtest)), drop = FALSE]
  ## nothing to do if hotencoding was not used
  if (attr(x, "hotencode") == FALSE) {
    attr(xtest, "hotencode") <- FALSE
    attr(xtest, "xvar.names") <- colnames(xtest)
    ## return as is
    if (raw) {
      return(xtest)
    }
    ## removes unncessary extra columns
    else {
      return(xtest[, colnames(x), drop = FALSE])
    }
  }
  ## pull the training levels
  xlevels <- attr(x, "levels")
  ## there are factors present in the test data: encode them
  x.f <- do.call(cbind, papply(names(xlevels), function(nn) {
    ## pull the test feature: convert to character 
    xn <- as.character(xtest[, nn])
    ## extract train/test set labels
    trn.labels <- xlevels[[nn]]
    tst.labels <- sort(unique(xn))
    ## convert the test feature to a factor
    ## - superimpose original levels first followed by levels in test data that differ
    ## - factors get coded as before -> with potentially additional columns
    ## - for binary factors -> makes a fake integer value > 1
    xn <- factor(xn, levels = c(trn.labels, setdiff(tst.labels, trn.labels)))
    ## one-level factors are converted to zero
    if (length(trn.labels) == 1) {
      rep(0, length(xn))
    }
    ## two-level factors are converted to binary
    else if (length(trn.labels) == 2) {
      #xn <- data.frame(as.numeric(factor(xn, levels = levels(xn))) - 1)
      xn <- data.frame(as.numeric(xn) - 1)
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
  ## package up as data frame, store useful attributes
  xtest <- data.frame(xtest[, setdiff(xvar.names, names(xlevels)), drop = FALSE], x.f)
  attr(xtest, "hotencode") <- TRUE
  attr(xtest, "levels") <- xlevels
  attr(xtest, "xvar.names") <- xvar.names
  ## return as is
  if (raw) {
    xtest
  }
  ## removes unncessary extra columns
  else {
    xtest[, colnames(x), drop = FALSE]
  }
}
