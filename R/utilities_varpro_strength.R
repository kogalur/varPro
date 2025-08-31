get.bootstrap.bits <- function (bootstrap) {
  if (bootstrap == "none") {
    bootstrap <- 0
  }
  else if (bootstrap == "by.root") {
    bootstrap <- 2^19
  }
  else if (bootstrap == "by.user") {
    bootstrap <- 2^20
  }
  else {
    stop("Invalid choice for 'bootstrap' option:  ", bootstrap)
  }
  return (bootstrap)
}
get.cr.bits <- function (fmly) {
  if (fmly == "surv-CR") {
    return(2^21)
  } else {
    return(0)
  }
}
get.data.pass.bits <- function (data.pass) {
  if (!is.null(data.pass)) {
    if (data.pass == TRUE) {
      data.pass <- 2^15
    }
    else if (data.pass == FALSE) {
      data.pass <- 0
    }
    else {
      stop("Invalid choice for 'data.pass' option:  ", data.pass)
    }
  }
  else {
    stop("Invalid choice for 'data.pass' option:  ", data.pass)
  }
  return (data.pass)
}
get.varpro.strength.bits  <- function(oob.bits, restore.mode) {
  ## 0 --> request OOB = 0 , 1 --> request INBAG = 2^2
  if (!is.null(oob.bits)) {
    if (oob.bits == 1) {
      bits  <- 2^2
    }
    else {
      bits  <- 0
    }
  }
  else {
    if (restore.mode) {
      bits  <- 0
    }
    else {
      bits <- 2^2
    }
  }
  return (bits)
}
get.freq.table.bits  <- function(freq.table.flag, restore.mode) {
  ## 0 --> no table --> bit = 0, 1 --> request table --> bit = 2^3
  if (!is.null(freq.table.flag)) {
      if (restore.mode) {
          bits  <- 0
      }
      else {
          if (freq.table.flag == 0) {
              bits  <- 0
          }
          else {
              bits  <- 2^3
          }
      }
  }
  else {
      bits  <- 0
  }
  return (bits)
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
get.rf.cores <- function () {
  if (is.null(getOption("rf.cores"))) {
    if (!is.na(as.numeric(Sys.getenv("RF_CORES")))) {
      options(rf.cores = as.integer(Sys.getenv("RF_CORES")))
    }
  }
  return (getOption("rf.cores", -1L))
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
get.seed <- function (seed) {
  if ((is.null(seed)) || (abs(seed) < 1)) {
    seed <- runif(1,1,1e6)
  }
  seed <- -round(abs(seed))
  return (seed)
}
get.stat.bits <- function (stat) {
  if (stat == "importance") {
    stat <- 2^0 + 2^1
  }
  else if (stat == "complement") {
    stat <- 2^0
  }
  else if (stat == "oob") {
    stat <- 2^1
  }
  else if (stat == "none") {
      stat  <- 0
  }
  else {
    stop("Invalid choice for 'stat' option:  ", stat)
  }
  return (stat)
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
get.tree.index <- function(get.tree, ntree) {
  ## NULL --> default setting
  if (is.null(get.tree)) {
    rep(1, ntree)
  }
  ## the user has specified a subset of trees
  else {
    pt <- get.tree >=1 & get.tree <= ntree
    if (sum(pt) > 0) {
      get.tree <- get.tree[pt]
      get.tree.temp <- rep(0, ntree)
      get.tree.temp[get.tree] <- 1
      get.tree.temp
    }
    else {
      rep(1, ntree)
    }
  }
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
get.varpro.strengthArray <- function(var.strength, family, y) {
  ## regression (survival) case
  if (family == "regr" || family == "surv") {
    ## assign new column names
    colnames(var.strength) <-  c("tree",
                                 "branch",
                                 "variable",
                                 "n.oobCT",
                                 "n.oob",
                                 "imp")
    ## standardize importance by sqrt(variance)
    var.strength$imp <- var.strength$imp / sqrt(var(y))
  }
  ## mv-regression
  else if (family == "regr+") {
    imp.names <- paste0("imp.", 1:ncol(y))
    colnames(var.strength) <- c("tree",
                                "branch",
                                "variable",
                                "n.oobCT",
                                "n.oob",
                                imp.names)
    ## standardize importance by sqrt(variance)
    var.y <- as.numeric(diag(var(y, na.rm = TRUE)))
    var.strength[, imp.names] <- var.strength[, imp.names] / sqrt(var.y)
  }
  ## classification
  else if (family == "class") {
    J <- length(levels(y))
    colnames(var.strength) <- c("tree",
                                "branch",
                                "variable",
                                "n.oobCT",
                                c("n.oob", paste0("n.oob.", 1:J)),
                                c("imp", paste0("imp.", 1:J)))
  }
  ## unsupervised
  else if (family == "unsupv") {
    ## add fake column for vimp
    colnames(var.strength) <-  c("tree",
                                 "branch",
                                 "variable",
                                 "n.oobCT",
                                 "n.oob")
    var.strength$imp <- NA 
  }
  ## something's wrong
  else {
    stop("family not supported")
  }
  ## remove complementary sample size from array
  var.strength$n.oobCT <- NULL
  ## return the strength array
  var.strength
}
## extracts varpro strength array for both rfsrc and varpro objects
get.varpro.strength <- function(object,
                                m.target = NULL,
                                max.rules.tree = 150,
                                max.tree = 150,
                                membership = FALSE,
                                y.external = NULL,
                                seed = NULL)
{
  ## ------------------------------------------------------------------------
  ##
  ## incoming object must be an rfsrc or varpro object
  ## 
  ## ------------------------------------------------------------------------
  if (!inherits(object, "varpro")) {
    if (sum(inherits(object, c("rfsrc", "grow"), TRUE) == c(1, 2)) != 2) {
      stop("This function only works for objects of class 'varpro' or `(rfsrc, grow)'")
    }
    ## this is a random forest object, need to process y according to family
    else {
      o <- object
      x <- object$xvar
      attr(x, "hotencode") <- FALSE
      o$x <- x
      o$xvar.names <- object$xvar.names
      ## allowed supervised families
      if (o$family == "regr" || o$family == "surv" || o$family == "class" || o$family == "regr+") {
        if (is.null(y.external)) {
          y <- object$yvar
        }
        else {
          y <- y.external
        }
      }
      ## unsupervised
      else if (o$family == "unsupv") {
        ## nothing to do
      }
      ## something's wrong
      else  {
        stop("family currently not supported")
      }
    }
  }
  ## this is a varpro object, extract the random forest object and x, y
  else {
    o <- object$rf
    o$x <- object$x
    o$xvar.names <- object$xvar.names
    y <- object$y
  }
  ## ------------------------------------------------------------------------
  ##
  ## obtain varpro strength values
  ##
  ## ------------------------------------------------------------------------
  vp.strength.o <- varpro.strength(object = o,
                                  m.target = m.target,
                                  max.rules.tree = max.rules.tree,
                                  max.tree = max.tree,
                                  membership = membership,
                                  seed = seed)
  ## ------------------------------------------------------------------------
  ##
  ## ## over-ride original object with updated information and return
  ##
  ## ------------------------------------------------------------------------
  o$max.rules.tree <- max.rules.tree
  o$max.tree <- max.tree
  o$strengthArray <- vp.strength.o$strengthArray
  o$results <- get.varpro.strengthArray(vp.strength.o$strengthArray, o$family, y)
  o$oobMembership <- vp.strength.o$oobMembership
  o$compMembership <- vp.strength.o$compMembership
  class(o) <- "varpro"
  o
}
