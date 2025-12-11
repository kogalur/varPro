cv.varpro <- function(f, data, nvar = 30, ntree = 150,
                      local.std = TRUE, zcut = seq(0.1, 2, length = 50), nblocks = 10,
                      split.weight = TRUE, split.weight.method = NULL, sparse = TRUE,
                      nodesize = NULL, max.rules.tree = 150, max.tree = min(150, ntree),
                      papply = mclapply, verbose = FALSE, seed = NULL,
                      fast = FALSE, crps = FALSE,
                      ...)

{		   
		 


  ##--------------------------------------------------------------
  ##
  ## extract original yvalue names
  ## re-define the original data in case there are missing values
  ##
  ##--------------------------------------------------------------

  stump <- get.stump(f, data)
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
  use.rfq <- get.varpro.hidden(NULL, NULL)$use.rfq
  iratio.threshold <- get.varpro.hidden(NULL, NULL)$iratio.threshold

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

  
  o <- do.call("varpro", c(list(f = f, data = data, nvar = nvar, ntree = ntree,
                  split.weight = split.weight, split.weight.method = split.weight.method, sparse = sparse,
                  nodesize = nodesize, max.rules.tree = max.rules.tree, max.tree = max.tree,
		  papply = papply, verbose = verbose, seed = seed), dots))
		

  ##--------------------------------------------------------------
  ##
  ## extract importance values
  ## map importance values which are hot-encoded back to original data 
  ##
  ##--------------------------------------------------------------

  vorg <- get.orgvimp(o, papply = papply, local.std = local.std)
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
        err.zz <- get.sderr(randomForestSRC::rfsrc.fast(f, data[trn, c(yvar.names, xvar.names[pt]), drop = FALSE],
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

  ## minimum error
  vmin <- vorg
  zcut.min <- 0
  if (!all(is.na(err[, 3]))) {
    zcut.min <- zcut[which.min(err[, 3])]
    if (verbose) {
      cat("optimal cutoff value", zcut.min, "\n")
    }
    vmin <- vorg[imp >= zcut.min,, drop = FALSE]
  }


  ## 1sd error rule -conservative
  v1sd.conserve <- vorg
  zcut.1sd <- 0
  if (!all(is.na(err[, 3]))) {
    idx.opt <- which.min(err[, 3])
    serr <- mean(err[, 4], na.rm = TRUE)
    idx2.opt <- err[, 3] < 1 & (err[, 3] <= (err[idx.opt, 3] + serr))
    idx2.opt[is.na(idx2.opt)] <- FALSE
    if (sum(idx2.opt) > 0) {
      zcut.1sd <- zcut[max(which(idx2.opt))]
      if (verbose) {
        cat("optimal 1sd + (conservative) cutoff value", zcut.1sd, "\n")
      }
      v1sd.conserve <- vorg[imp >= zcut.1sd,, drop = FALSE]
    }
    else {
      v1sd.conserve <- NULL
    }
  }
  
  ## 1sd error rule -liberal
  v1sd.liberal <- vorg
  zcut.liberal <- 0
  if (!all(is.na(err[, 3]))) {
    idx.opt <- which.min(err[, 3])
    serr <- mean(err[, 4], na.rm = TRUE)
    zcut.liberal <- zcut[min(which(err[, 3] <= (err[idx.opt, 3] + serr)), na.rm = TRUE)]
    if (verbose) {
      cat("optimal 1sd - (liberal) cutoff value", zcut.liberal, "\n")
    }
    v1sd.liberal <- vorg[imp >= zcut.liberal,, drop = FALSE]
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
  attr(rO, "imp.org") <- importance(o, local.std = local.std)
  attr(rO, "xvar.names") <- o$xvar.names
  attr(rO, "xvar.org.names") <- o$xvar.org.names
  attr(rO, "family") <- o$family

  return(rO)
  
}


## custom print object for cv to make attributes invisible
print.cv.varpro <- function(x, ...) {

  attr(x, "class") <- attr(x, "imp.org") <- attr(x, "xvar.names") <-
    attr(x, "xvar.org.names") <- attr(x, "family") <- NULL
  print(x)
  
}

print.cv <- print.cv.varpro



