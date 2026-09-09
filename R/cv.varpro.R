## The OOB selector is unchanged when outer assessment is not requested.
cv.varpro <- function(formula, data, nvar = 30, ntree = 150,
                      local.std = TRUE, zcut = seq(0.1, 2, length = 50), nblocks = 10,
                      split.weight = TRUE, split.weight.method = NULL, sparse = TRUE,
                      nodesize = NULL, max.rules.tree = 150, max.tree = min(150, ntree),
                      verbose = FALSE, seed = NULL,
                      fast = FALSE, crps = FALSE,
                      cv.folds = 0, foldid = NULL,
                      ...)
{
  if (length(cv.folds) != 1L || !is.numeric(cv.folds) ||
      !is.finite(cv.folds) || cv.folds < 0 ||
      cv.folds != floor(cv.folds) || cv.folds == 1) {
    stop("cv.folds must be zero or an integer of at least two")
  }
  outer <- cv.folds > 0 || !is.null(foldid)
  args <- c(list(formula = formula, data = data, nvar = nvar, ntree = ntree,
                 local.std = local.std, zcut = zcut, nblocks = nblocks,
                 split.weight = split.weight,
                 split.weight.method = split.weight.method, sparse = sparse,
                 nodesize = nodesize, max.rules.tree = max.rules.tree,
                 max.tree = max.tree, verbose = verbose, seed = seed,
                 fast = fast, crps = crps), list(...))
  if (!outer) return(do.call(.cv.varpro.select, args))
  if (!is.null(foldid) &&
      (!is.numeric(foldid) || length(foldid) != NROW(data))) {
    stop("foldid must be a numeric vector with one entry per input data row")
  }
  ## Run the ordinary full-data analysis first. Outer CV is then performed
  ## in a saved RNG context, so it changes neither this analysis nor the
  ## random-number state left by an ordinary call with the same inputs.
  outer.seed <- if (is.null(seed)) {
    .cv.varpro.with.seed(NULL, sample.int(.Machine$integer.max, 1L))
  } else seed
  args$.cv.keep <- TRUE
  full <- do.call(.cv.varpro.select, args)
  info <- attr(full, ".cv.info")
  attr(full, ".cv.info") <- NULL
  .cv.varpro.with.seed(NULL, {
    .cv.varpro.outer(full, info, args, cv.folds, foldid, NROW(data), outer.seed)
  })
}
.cv.varpro.select <- function(formula, data, nvar = 30, ntree = 150,
                      local.std = TRUE, zcut = seq(0.1, 2, length = 50), nblocks = 10,
                      split.weight = TRUE, split.weight.method = NULL, sparse = TRUE,
                      nodesize = NULL, max.rules.tree = 150, max.tree = min(150, ntree),
                      verbose = FALSE, seed = NULL,
                      fast = FALSE, crps = FALSE, .cv.keep = FALSE,
                      ...)
{		   
  ## Validate the search before fitting any forests.
  if (!is.numeric(zcut) || !length(zcut) || any(!is.finite(zcut)) ||
      any(zcut <= 0)) {
    stop("zcut must be a nonempty numeric vector of finite positive cutoffs")
  }
  zcut <- sort(unique(zcut))
  if (length(nblocks) != 1L || !is.numeric(nblocks) ||
      !is.finite(nblocks) || nblocks < 1 || nblocks != floor(nblocks)) {
    stop("nblocks must be a positive integer")
  }
  if (length(ntree) != 1L || !is.numeric(ntree) ||
      !is.finite(ntree) || ntree < 1 || ntree != floor(ntree)) {
    stop("ntree must be a positive integer")
  }
  ##--------------------------------------------------------------
  ##
  ## to avoid forking issues we run everything in serial in R
  ##
  ##--------------------------------------------------------------
  papply <- base::lapply
  ##--------------------------------------------------------------
  ##
  ## extract original yvalue names
  ## re-define the original data in case there are missing values
  ##
  ##--------------------------------------------------------------
  if (.cv.keep) input.data <- as.data.frame(data)
  stump <- get.stump(formula, data)
  n <- stump$n
  p <- length(stump$xvar.names)
  yvar.names <- stump$yvar.names
  data <- data.frame(stump$yvar, stump$xvar)
  colnames(data)[1:length(yvar.names)] <- yvar.names
  family <- stump$family
  rm(stump)
  ##--------------------------------------------------------------
  ##
  ## extract additional options specified by user
  ##
  ##--------------------------------------------------------------
  dots <- list(...)
  ## set nodesize
  nodesize <- set.cv.nodesize(n, p, nodesize)
  dots$nodesize.reduce <- set.nodesize(n, p, dots$nodesize.reduce)
  dots$nodedepth.reduce <- set.nodedepth.reduce(n, p, dots$nodedepth.reduce)
  if (is.null(dots$sampsize)) {
    dots$nodesize.external <- set.nodesize(n, p, dots$nodesize.external)
  }
  else {
    if (is.function(dots$sampsize)) {
      dots$nodesize.external <- set.nodesize(dots$sampsize(n), p, dots$nodesize.external)
    }
    else {
      dots$nodesize.external <- set.nodesize(dots$sampsize, p, dots$nodesize.external)
    }
  }
  ## set rfq parameters for class imbalanced scenario
  hidden <- get.varpro.hidden(dots, ntree)
  use.rfq <- hidden$use.rfq
  iratio.threshold <- hidden$iratio.threshold
  ##--------------------------------------------------------------
  ##
  ## default settings
  ##
  ##--------------------------------------------------------------
  trn <- 1:n
  newdata <- splitrule <- rfq <- imbalanced.obj <- cens.dist <- NULL
  ##--------------------------------------------------------------
  ##
  ## set the type of sampling, define train/test (fast=TRUE)
  ##
  ##--------------------------------------------------------------
  ## use same inbag/oob members to reduce MC error
  if (!fast) {
    if (is.null(dots$sampsize)) {##default sample size function used by rfsrc.fast
      ssize <- n * .632
    }
    else {
      ssize <- eval(dots$sampsize)
    }
    if (is.function(ssize)) {##user has specified a function
      ssize <- ssize(n)
    }
  }
  ## subsampling is in effect when fast = TRUE
  else {
    ## obtain the requested sample size
    if (is.null(dots$sampsize)) {##default sample size function used by rfsrc.fast
      ssize <- eval(formals(randomForestSRC::rfsrc.fast)$sampsize)
    }
    else {
      ssize <- eval(dots$sampsize)
    }
    if (is.function(ssize)) {##user has specified a function
      ssize <- ssize(n)
    }
    ## now hold out a test data set equal to the tree sample size (if possible)
    if (n > (2 * ssize))  {
      tst <- sample(1:n, size = ssize, replace = FALSE)
      trn <- setdiff(1:n, tst)
      newdata <- data[tst,, drop = FALSE]
    }
  }
  ## custom sample array
  samp <- randomForestSRC:::make.sample(ntree, length(trn), ssize)
  ## pass the sample size to varpro as a hidden option
  dots$sampsize <- ssize
  ##--------------------------------------------------------------
  ##
  ## varpro call
  ##
  ##--------------------------------------------------------------
  o <- do.call("varpro", c(list(formula = formula, data = data, nvar = nvar, ntree = ntree,
                  split.weight = split.weight, split.weight.method = split.weight.method, sparse = sparse,
                  nodesize = nodesize, max.rules.tree = max.rules.tree, max.tree = max.tree,
                  verbose = verbose, seed = seed), dots))
  ##--------------------------------------------------------------
  ##
  ## extract importance values
  ## map importance values which are hot-encoded back to original data 
  ##
  ##--------------------------------------------------------------
  ## compute importance once and reuse it (avoid recomputation)
  vmp <- importance(o, local.std = local.std)
  vorg <- get.orgvimp(o, local.std = local.std, vmp = vmp)
  xvar.names <- vorg$variable
  imp <- vorg$z
  imp[is.na(imp)] <- 0
  ##--------------------------------------------------------------
  ##
  ## remove zcut values that lead to duplicated models
  ##
  ##--------------------------------------------------------------
  zcut.models <- do.call(rbind, lapply(zcut, function(zz) {
    1 * (imp >= zz)
  }))
  zcut <- zcut[!duplicated(zcut.models)]
  ##--------------------------------------------------------------
  ##
  ## rfq details: only applies to two class imbalanced scenarios
  ##
  ##--------------------------------------------------------------
  if (family == "class" && length(levels(data[, yvar.names])) == 2 && use.rfq) {
    ## calculate imblanced ratio
    y.frq <- table(data[, yvar.names])
    class.labels <- names(y.frq)
    iratio <- max(y.frq, na.rm = TRUE) / min(y.frq, na.rm = TRUE)
    ## check if this is imbalanced using default threshold setting
    if (iratio > iratio.threshold) {
      rfq <- TRUE
      splitrule <- "auc"
      imbalanced.obj <- list(perf.type = "gmean",
                             iratio = iratio,
                             iratio.threshold = iratio.threshold)
    }
  }  
  ##--------------------------------------------------------------
  ##
  ## censoring distribution: only applies to survival families
  ##
  ##--------------------------------------------------------------
  if (family == "surv" && crps) {
    cens.dist <- get.cens.dist(data[trn, c(yvar.names, xvar.names), drop = FALSE],
                        ntree, nodesize, ssize,
                        newdata = if (is.null(newdata)) NULL else
                          newdata[, c(yvar.names, xvar.names), drop = FALSE])
  }  
  ##--------------------------------------------------------------
  ##
  ## select zcut using out-of-sample performance
  ##
  ##--------------------------------------------------------------
  ## set the seed
  seed <- get.seed(seed)
  ## loop over zcut sequence and acquire OOB error rate
  err <- do.call(rbind, lapply(zcut, function(zz) {
    pt <- imp >= zz
    if (sum(pt) > 0) {
      if (!fast) {
        err.zz <- get.sderr(rfsrc(formula,
                                  data = data[trn, c(yvar.names, xvar.names[pt]), drop = FALSE],
                                  nodesize = nodesize,
                                  ntree = ntree,
                                  rfq = rfq,
                                  splitrule = splitrule,
                                  perf.type = "none",
                                  bootstrap = "by.user",
                                  samp = samp,
                                  seed = seed),
                            nblocks = nblocks,
                            crps = crps,
                            papply = papply,
                            imbalanced.obj = imbalanced.obj,
                            cens.dist = cens.dist)
      }
      else {
        ## nodesize is not deployed because fast subsampling is in play
        err.zz <- get.sderr(randomForestSRC::rfsrc.fast(formula,
                            data = data[trn, c(yvar.names, xvar.names[pt]), drop = FALSE],
                            ntree = ntree,
                            rfq = rfq,
                            splitrule = splitrule,
                            perf.type = "none",
                            forest = TRUE,
                            bootstrap = "by.user",
                            samp = samp,
                            seed = seed),
                         nblocks = nblocks,
                         crps = crps,
                         papply = papply,
                         newdata = newdata,
                         imbalanced.obj = imbalanced.obj,
                         cens.dist = cens.dist)
      }
    }
    else {
      err.zz <- c(NA, NA) 
    }
    if (verbose) {
      cat("zcut value", zz,
          "number variables", sum(pt),
          "error", err.zz[1],
          "sd", err.zz[2], "\n")
    }
    c(zz, sum(pt), err.zz)
  }))
  colnames(err) <- c("zcut", "nvar", "err", "sd")
  ##--------------------------------------------------------------
  ##
  ## return the importance values after filtering 
  ##
  ##--------------------------------------------------------------
  ## Retain the existing unfiltered fallback when no model can be scored.
  vmin <- v1sd.conserve <- v1sd.liberal <- vorg
  zcut.min <- zcut.1sd <- zcut.liberal <- 0
  valid <- which(is.finite(err[, "err"]))
  if (length(valid)) {
    idx.opt <- valid[which.min(err[valid, "err"])]
    zcut.min <- zcut[idx.opt]
    vmin <- vorg[imp >= zcut.min, , drop = FALSE]
    deviations <- err[valid, "sd"]
    deviations <- deviations[is.finite(deviations)]
    serr <- if (length(deviations)) mean(deviations) else 0
    eligible <- valid[err[valid, "err"] <= err[idx.opt, "err"] + serr]
    ## Increasing cutoffs make the last eligible model the smallest.
    conservative <- eligible[err[eligible, "err"] < 1]
    if (length(conservative)) {
      zcut.1sd <- zcut[max(conservative)]
      v1sd.conserve <- vorg[imp >= zcut.1sd, , drop = FALSE]
    } else {
      v1sd.conserve <- NULL
    }
    zcut.liberal <- zcut[min(eligible)]
    v1sd.liberal <- vorg[imp >= zcut.liberal, , drop = FALSE]
    if (verbose) {
      cat("optimal cutoff value", zcut.min, "\n")
      cat("optimal conservative cutoff value", zcut.1sd, "\n")
      cat("optimal liberal cutoff value", zcut.liberal, "\n")
    }
  } else {
    warning("no candidate model has a finite prediction error; returning the unfiltered importance ranking")
  }
  rO <- list(imp = vmin,
             imp.conserve = v1sd.conserve,
             imp.liberal = v1sd.liberal,
             err = err,
             zcut = zcut.min,
             zcut.conserve = zcut.1sd,
             zcut.liberal = zcut.liberal)
  class(rO) <- "cv.varpro"
  ## append some useful information as attributes
  attr(rO, "imp.org") <- vmp
  attr(rO, "xvar.names") <- o$xvar.names
  attr(rO, "xvar.org.names") <- o$xvar.org.names
  attr(rO, "family") <- o$family
  ## Used only while assembling outer CV; removed before public return.
  if (.cv.keep) {
    rows <- .cv.varpro.analysis.rows(input.data, data)
    attr(rO, ".cv.info") <- list(
      data = input.data[rows, colnames(data), drop = FALSE],
      rows = rows, family = family, yvar.names = yvar.names,
      importance = vorg, xvar.map = .get.hotencode.map(o$x),
      ntree = ntree, nodesize = nodesize, sampsize = ssize, fast = fast,
      rfq = rfq, splitrule = splitrule,
      perf.type = if (family == "class") {
        if (is.null(imbalanced.obj)) "brier" else imbalanced.obj$perf.type
      } else if (family == "surv" && crps) "none" else "default")
  }
  return(rO)
}
## Keep the usual list display, followed by a compact assessment summary.
print.cv.varpro <- function(x, ...) {
  rules <- c("imp", "imp.conserve", "imp.liberal")
  meta <- lapply(x[rules], function(tab) attr(tab, "cv"))
  out <- x
  attributes(out) <- list(names = names(x))
  for (nm in rules) {
    if (!is.null(out[[nm]])) attr(out[[nm]], "cv") <- NULL
  }
  print(out, ...)
  if (!is.null(attr(x, "cv"))) {
    cat("\nOuter cross-validation (", attr(x, "cv")$folds,
        " folds)\n", sep = "")
    summary <- do.call(rbind, lapply(seq_along(rules), function(j) {
      m <- meta[[j]]
      data.frame(rule = rules[j], perf.type = m$perf.type,
                 err = m$err, sd = m$sd,
                 mean.nvar = mean(m$folds$nvar),
                 fallbacks = sum(m$folds$fallback),
                 row.names = NULL)
    }))
    print(summary, row.names = FALSE)
  }
  invisible(x)
}
print.cv <- print.cv.varpro
