## hot-encoding
get.hotencode <- function(x, papply = mclapply) {
  anyF <- sapply(x, is.factor)
  ## factors are present -> hotencode them
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
    attr(x, "hotencode") <- TRUE
  }
  ## no hotencoding performed
  else {
    attr(x, "hotencode") <- FALSE
  }
  x
}
## make interaction data
make.vt <- function(f, data, subset = NULL, interact = 2, papply = mclapply) {
  ## run a stumpy tree as a quick way to extract  x, y
  ## this also cleans up missing data 
  stump <- rfsrc(f, data, mtry = 1, splitrule="random", nodedepth=0, perf.type = "none", save.memory = TRUE, ntree=1)
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
