## stumpy tree
get.stump <- function(f, data) {
  rfsrc(f, data, mtry=1, splitrule="random", nodedepth=0,
                      perf.type="none", save.memory=TRUE, ntree=1)
}
## make interaction data
make.vt <- function(f, data, subset = NULL, interact = 2, papply = mclapply) {
  ## run a stumpy tree as a quick way to extract  x, y
  ## this also cleans up missing data 
  stump <- get.stump(f, data)
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
