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
## make interaction data
make.vt <- function(f, data, subset = NULL, interact = 2, papply = mclapply) {
  ## run a stumpy tree as a quick way to extract  x, y
  ## this also cleans up missing data 
  stump <- rfsrc(f, data, mtry = 1, splitrule="random",
           nodedepth=0, perf.type = "none", save.memory = TRUE, ntree=1)
  y <- stump$yvar
  yvar.names <- stump$yvar.names
  x <- stump$xvar
  rm(stump)
  gc()
  ## hot-encode x
  x <- get.hotencode(x, papply)
  xvar.names <- colnames(x)
  ## columns to be used for interactions
  if (is.null(subset)) {
    subset <- 1:ncol(x)
  }
  if (is.character(subset)) {
    subset <- which(xvar.names %in% subset)
  }
  ## two-way interactions
  nm <- c(yvar.names, xvar.names)
  if (length(subset) >= 2) {
    x.two <- do.call(cbind, lapply(combn(subset, 2, simplify = FALSE), function(j) {
      nm <<- c(nm, paste0(xvar.names[j[1]], ".", xvar.names[j[2]]))
      x[, j[1]] * x[, j[2]] 
    }))
    datamod <- data.frame(y = y, x, x.two)
  }
  else if (length(subset) == 1) {
    x.two <- do.call(cbind, lapply(setdiff(1:ncol(x), subset), function(j) {
      nm <<- c(nm, paste0(xvar.names[subset], ".", xvar.names[j]))
      x[, subset] * x[, j] 
    }))
    datamod <- data.frame(y = y, x, x.two)    
  }
  else {
    datamod <- data.frame(y = y, x)
  }
  colnames(datamod) <- nm
  ## three-way interactions
  if (length(subset) >= 3 && interact > 2) {
    nm <- NULL
    x.three <- do.call(cbind, lapply(combn(subset, 3, simplify = FALSE), function(j) {
      nm <<- c(nm, paste0(xvar.names[j[1]], ".", xvar.names[j[2]], ".", xvar.names[j[3]]))
      x[, j[1]] * x[, j[2]] * x[, j[3]] 
    }))
    colnames(x.three) <- nm
    datamod <- data.frame(datamod, x.three)
  }
  datamod
}
## robust sample
resample <- function(x, ...) x[sample.int(length(x), ...)]
