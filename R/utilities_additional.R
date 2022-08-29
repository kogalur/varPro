get.seed <- function (seed) {
  if ((is.null(seed)) || (abs(seed) < 1)) {
    seed <- runif(1,1,1e6)
  }
  seed <- -round(abs(seed))
  return (seed)
}
get.trace <- function (do.trace) {
  ## Convert trace into native code parameter.
  if (!is.logical(do.trace)) {
    if (do.trace >= 1) {
      do.trace <- round(do.trace)
    }
    else {
      do.trace <- 0
    }
  }
  else {
    do.trace <- 1 * do.trace
  }
  return (do.trace)
}
get.rf.cores <- function () {
  if (is.null(getOption("rf.cores"))) {
    if(!is.na(as.numeric(Sys.getenv("RF_CORES")))) {
      options(rf.cores = as.integer(Sys.getenv("RF_CORES")))
    }
  }
  return (getOption("rf.cores", -1L))
}
get.outcome.target <- function(family, yvar.names, outcome.target) {
  if (family == "regr" | family == "regr+" | family == "class" | family == "class+" | family == "mix+") {
    if (is.null(outcome.target)) {
      outcome.target <- yvar.names
    }
    ## Map target names to outcome names and ensure coherency.
    outcome.target <- unique(outcome.target)
    outcome.target <- intersect(outcome.target, yvar.names)
    if (length(outcome.target) == 0) {
      stop("yvar target names do not match object yvar names")
    }
    outcome.target <- match(outcome.target, yvar.names)
  }
    else {
      ## This is surv or surv-CR
      outcome.target <- 0
    }
}
get.cr.bits <- function (fmly) {
  if (fmly == "surv-CR") {
    return(2^21)
  } else {
    return(0)
  }
}
get.bootstrap.bits <- function (bootstrap) {
  if (bootstrap == "by.root") {
    bootstrap <- 0
  }
  else if (bootstrap == "none") {
    bootstrap <- 2^20
  }
  else if (bootstrap == "by.user") {
    bootstrap <- 2^19 + 2^20
  }
  else {
    stop("Invalid choice for 'bootstrap' option:  ", bootstrap)
  }
  return (bootstrap)
}
## convert samptype option into native code parameter.
get.samptype.bits <- function (samptype) {
  if (samptype == "swr") {
    bits <- 0
  }
  else if (samptype == "swor") {
    bits <- 2^12
  }
  else {
    stop("Invalid choice for 'samptype' option:  ", samptype)
  }
  return (bits)
}
get.terminal.qualts.restore.bits <- function(incoming.flag) {
  bits <- 2^16
  if (!is.null(incoming.flag)) {
    if (incoming.flag == TRUE) {
      bits <- bits + 2^17
    }
  }
  else {
    stop("Invalid choice for 'incoming.flag':  ", incoming.flag)
  }
  return(bits)
}
get.terminal.quants.restore.bits <- function(incoming.flag) {
  bits <- 2^18
  if (!is.null(incoming.flag)) {
    if (incoming.flag == TRUE) {
      bits <- bits + 2^19
    }
  }
  else {
    stop("Invalid choice for 'incoming.flag':  ", incoming.flag)
  }
  return(bits)
}
