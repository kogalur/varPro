cv.varpro <- function(f, data, ntree = 150,
                      zcut = seq(0.1, 2, length = 50),
                      nblocks = 10,
                      split.weight = TRUE,
                      nodesize = NULL, max.rules.tree = 150, max.tree = min(150, ntree),
                      papply = mclapply, verbose = FALSE, seed = NULL,
                      fast = FALSE, crps = FALSE,
                      ...)
{		   
  ##--------------------------------------------------------------
  ##
  ## extract additional options specified by user
  ##
  ##--------------------------------------------------------------
  dots <- list(...)
  ##--------------------------------------------------------------
  ##
  ## extract original yvalue names
  ## re-define the original data in case there are missing values
  ##
  ##--------------------------------------------------------------
  stump <- rfsrc(f, data, mtry = 1, nodedepth = 0, perf.type = "none", save.memory = TRUE,
                    ntree = 1, splitrule = "random")
  n <- stump$n
  p <- length(stump$xvar.names)
  yvar.names <- stump$yvar.names
  data <- data.frame(stump$yvar, stump$xvar)
  colnames(data)[1:length(yvar.names)] <- yvar.names
  family <- stump$family
  rm(stump)
  ##--------------------------------------------------------------
  ##
  ## set nodesize
  ##
  ##--------------------------------------------------------------
  nodesize <- set.cv.nodesize(n, p, nodesize)
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
  ##--------------------------------------------------------------
  ##
  ## set the type of sampling, define train/test (fast=TRUE)
  ##
  ##--------------------------------------------------------------
  ## default settings
  trn <- 1:n
  newdata <- cens.dist <- NULL
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
    if ((2 * ssize)  < n)  {
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
  o <- do.call("varpro", c(list(f = f, data = data,
                  ntree = ntree, split.weight = split.weight,
                  nodesize = nodesize, max.rules.tree = max.rules.tree, max.tree = max.tree,
		  papply = papply, verbose = verbose, seed = seed), dots))
  ##--------------------------------------------------------------
  ##
  ## extract importance values
  ##
  ##--------------------------------------------------------------
  v <- importance(o)
  if (o$family == "class") {
    v <- v$unconditional
  }
  if (o$family == "regr+") {
    v <- v[[1]]
  }
  xvar.names <- rownames(v)
  imp <- v$z
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
  ## censoring distribution: only applies to survival families
  ##
  ##--------------------------------------------------------------
  if (family == "surv" && crps) {
    cens.dist <- get.cens.dist(data[trn, c(yvar.names, xvar.names), drop = FALSE],
                        ntree, nodesize, ssize)
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
        err.zz <- get.sderr(rfsrc(f, data[trn, c(yvar.names, xvar.names[pt]), drop = FALSE],
                            nodesize = nodesize, ntree = ntree, perf.type = "none",
                            bootstrap = "by.user", samp = samp, seed = seed),
                            nblocks = nblocks, crps = crps, papply = papply, cens.dist = cens.dist)
      }
      else {
        ## nodesize is not deployed because fast subsampling is in play
        err.zz <- get.sderr(randomForestSRC::rfsrc.fast(f, data[trn, c(yvar.names, xvar.names[pt]), drop = FALSE],
                            #nodesize = nodesize,
                            ntree = ntree, perf.type = "none",
                            forest = TRUE, bootstrap = "by.user", samp = samp, seed = seed),
                            nblocks = nblocks, crps = crps, papply = papply, newdata = newdata, cens.dist = cens.dist)
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
  ## return the importace values after filtering 
  ##
  ##--------------------------------------------------------------
  ## minimum error
  vmin <- v
  if (!all(is.na(err[,3]))) {
    zcut.opt <- zcut[which.min(err[,3])]
    if (verbose) {
      cat("optimal cutoff value", zcut.opt, "\n")
    }
    vmin <- v[imp >= zcut.opt,, drop = FALSE]
  }
  vmin$selected <- vmin$zcenter <- NULL
  ## 1sd error rule -conservative
  v1sd.conserve <- v
  v1sd.conserve$selected <- v1sd.conserve$zcenter <- NULL
  if (!all(is.na(err[,3]))) {
    idx.opt <- which.min(err[,3])
    serr <- mean(err[,4], na.rm = TRUE)
    idx2.opt <- err[,3] < 1 & (err[,3] <= (err[idx.opt,3] + serr))
    idx2.opt[is.na(idx2.opt)] <- FALSE
    if (sum(idx2.opt) > 0) {
      zcut.opt <- zcut[max(which(idx2.opt))]
      if (verbose) {
        cat("optimal 1sd + (conservative) cutoff value", zcut.opt, "\n")
      }
      v1sd.conserve <- v[imp >= zcut.opt,, drop = FALSE]
      v1sd.conserve$selected <- v1sd.conserve$zcenter <- NULL
    }
    else {
      v1sd.conserve <- NULL
    }
  }
  ## 1sd error rule -liberal
  v1sd.liberal <- v
  if (!all(is.na(err[,3]))) {
    idx.opt <- which.min(err[,3])
    serr <- mean(err[,4], na.rm = TRUE)
    zcut.opt <- zcut[min(which(err[,3] <= (err[idx.opt,3] + serr)), na.rm = TRUE)]
    if (verbose) {
      cat("optimal 1sd - (liberal) cutoff value", zcut.opt, "\n")
    }
    v1sd.liberal <- v[imp >= zcut.opt,, drop = FALSE]
  }
  v1sd.liberal$selected <- v1sd.liberal$zcenter <- NULL
  list(imp = vmin,
       imp.conserve = v1sd.conserve,
       imp.liberal = v1sd.liberal,
       err = err)
}
