## hot-encoding
get.hotencode <- function(x, papply = lapply) {
  anyF <- sapply(x, is.factor) | sapply(x, is.character)
  ## factors are present -> hotencode them
  if (sum(anyF) > 0) {
    factor.names <- names(anyF)[anyF]
    x.parts <- papply(factor.names, function(nn) {
      ## pull the feature: force to factor
      xn <- factor(x[, nn])
      ## one-level factors are converted to zero
      if (length(levels(xn)) == 1) {
        xn <- data.frame(rep(0, length(xn)))
        colnames(xn) <- nn
        xn
      }
      ## two-level factors are converted to binary
      else if (length(levels(xn)) == 2) {
        #xn <- data.frame(as.numeric(factor(xn)) - 1)
        xn <- data.frame(as.numeric(xn) - 1)
        colnames(xn) <- nn
        xn
      }
      else {
        f <- as.formula(call("~", call("+", 0, as.name(nn))))
        xn <- data.frame(xn)
        colnames(xn) <- nn
        model.matrix(f, xn)
      }
    })
    x.f <- do.call(cbind, x.parts)
    source.names <- c(colnames(x)[!anyF],
                      rep(factor.names, vapply(x.parts, ncol, integer(1))))
    ## store useful information from original data
    xvar.names <- colnames(x)
    xlevels <- lapply(x[, anyF, drop = FALSE], function(z) levels(factor(z)))
    ## package up as data frame, store useful attributes
    x <- data.frame(x[, !anyF, drop = FALSE], x.f)
    attr(x, "hotencode") <- TRUE
    attr(x, "xvar.names") <- xvar.names
    attr(x, "levels") <- xlevels
    attr(x, "xvar.map") <- setNames(source.names, colnames(x))
  }
  ## no hotencoding performed
  else {
    attr(x, "hotencode") <- FALSE
    attr(x, "xvar.names") <- colnames(x)
    attr(x, "xvar.map") <- setNames(colnames(x), colnames(x))
  }
  x
}
## hot-encoding for test data
get.hotencode.test <- function(x, xtest, papply = lapply, raw = FALSE) {
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
    attr(xtest, "xvar.map") <- setNames(colnames(xtest), colnames(xtest))
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
  x.parts <- papply(names(xlevels), function(nn) {
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
      xn <- data.frame(rep(0, length(xn)))
      colnames(xn) <- nn
      xn
    }
    ## two-level factors are converted to binary
    else if (length(trn.labels) == 2) {
      #xn <- data.frame(as.numeric(factor(xn, levels = levels(xn))) - 1)
      xn <- data.frame(as.numeric(xn) - 1)
      colnames(xn) <- nn
      xn
    }
    else {
      f <- as.formula(call("~", call("+", 0, as.name(nn))))
      xn <- data.frame(xn)
      colnames(xn) <- nn
      model.matrix(f, xn)
    }
  })
  x.f <- do.call(cbind, x.parts)
  source.names <- c(setdiff(xvar.names, names(xlevels)),
                    rep(names(xlevels), vapply(x.parts, ncol, integer(1))))
  ## package up as data frame, store useful attributes
  xtest <- data.frame(xtest[, setdiff(xvar.names, names(xlevels)), drop = FALSE], x.f)
  attr(xtest, "hotencode") <- TRUE
  attr(xtest, "levels") <- xlevels
  attr(xtest, "xvar.names") <- xvar.names
  attr(xtest, "xvar.map") <- setNames(source.names, colnames(xtest))
  ## return as is
  if (raw) {
    xtest
  }
  ## removes unncessary extra columns
  else {
    xtest[, colnames(x), drop = FALSE]
  }
}
## Exact encoded-column -> original-variable mapping.
## New objects carry this mapping directly. For existing objects, rebuild
## the names from their stored levels, using the same model-matrix naming.
.get.hotencode.map <- function(x) {
  encoded <- colnames(x)
  map <- attr(x, "xvar.map", exact = TRUE)
  if (!is.null(map)) {
    if (is.null(names(map)) || anyDuplicated(names(map)) ||
        anyNA(match(encoded, names(map)))) {
      stop("stored hot-encoding map does not match the predictor columns")
    }
    return(map[encoded])
  }
  original <- attr(x, "xvar.names", exact = TRUE)
  if (is.null(original)) original <- encoded
  if (!isTRUE(attr(x, "hotencode", exact = TRUE))) {
    if (!all(encoded %in% original)) {
      stop("predictor columns do not match the original variable names")
    }
    return(setNames(encoded, encoded))
  }
  xlevels <- attr(x, "levels", exact = TRUE)
  if (is.null(xlevels)) {
    stop("hot-encoding metadata is missing; recreate the VarPro object")
  }
  ## Reproduce the original column assembly, including name repair by
  ## cbind.data.frame/data.frame and the old unnamed single-level column.
  ## One prototype row suffices because the stored factor levels are kept.
  numeric.names <- setdiff(original, names(xlevels))
  numeric.prototype <- data.frame(row.names = "1")
  for (nn in numeric.names) numeric.prototype[[nn]] <- 0
  parts <- lapply(names(xlevels), function(nn) {
    lev <- xlevels[[nn]]
    if (length(lev) == 1L) return(0)
    if (length(lev) == 2L) return(setNames(data.frame(0), nn))
    if (!length(lev)) stop("stored factor has no levels")
    d <- setNames(data.frame(factor(lev[1L], levels = lev)), nn)
    f <- as.formula(call("~", call("+", 0, as.name(nn))))
    model.matrix(f, d)
  })
  x.f <- do.call(cbind, parts)
  prototype <- data.frame(numeric.prototype, x.f)
  source.names <- c(numeric.names,
                    rep(names(xlevels), vapply(parts, NCOL, integer(1))))
  map <- setNames(source.names, colnames(prototype))
  if (anyNA(match(encoded, names(map)))) {
    stop("cannot recover the hot-encoding map; recreate the VarPro object")
  }
  map[encoded]
}
