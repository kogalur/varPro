############################################################################
###
### variable priority (varPro)
### regression, mv-regression, classification and survival
###
### ------------------------------------------------------------------------
### Written by:
###
### Hemant Ishwaran                     hemant.ishwaran@gmail.com
### Division of Biostatistics           
### Clinical Research Building
### 1120 NW 14th Street
### University of Miami, Miami FL 33136
###
### https:
### ------------------------------------------------------------------------
###
### THIS PROGRAM SHOULD NOT BE COPIED, USED, MODIFIED, OR 
### DISSEMINATED IN ANY WAY WITHOUT SPECIFIC WRITTEN PERMISSION 
### FROM THE AUTHOR.
###
############################################################################
varpro <- function(f, data, nvar = 30, ntree = 500, 
                   split.weight = TRUE, split.weight.method = NULL, sparse = TRUE,
                   nodesize = NULL, max.rules.tree = 150, max.tree = min(150, ntree),
                   parallel = TRUE, cores = get.mc.cores(),
                   verbose = FALSE, seed = NULL,
                   ...)
{
  ## ------------------------------------------------------------------------
  ##
  ##
  ## process data, check coherence of formula, data etc.
  ##
  ##
  ## ------------------------------------------------------------------------
  ## initialize the seed
  seed <- get.seed(seed)
  ## formula must be a formula
  f.org <- f <- as.formula(f)
  ## data must be a data frame
  data <- data.frame(data)
  ## droplevels
  data <- droplevels(data)
  ## run a stumpy tree as a quick way to determine family
  ## use the stumped tree to acquire x and y
  ## save original y - needed for coherent treatment of survival
  ## this also cleans up missing data
  stump <- get.stump(f, data)
  family <- stump$family
  yvar.names <- stump$yvar.names
  y <- stump$yvar
  y.org <- data.frame(y)
  colnames(y.org) <- stump$yvar.names
  x <- stump$xvar
  xvar.org.names <- colnames(x)
  rm(stump)
  gc()
  ## coherence check
  if (!(family == "regr" || family == "regr+" || family == "class" || family == "surv")) {
    stop("this function only works for regression, mv-regression, classification and survival")
  }
  ## check if "y" is used as a name for one of the x features
  if (any(colnames(x) == "y")) {
    yfkname <- "y123XYZ9999abc"
  }
  else {
    yfkname <- "y"
  }
  ## convert factors using hot-encoding
  x <- get.hotencode(x)
  xvar.names <- colnames(x)
  ## ------------------------------------------------------------------------
  ##
  ##
  ## assemble the data
  ##
  ##
  ## ------------------------------------------------------------------------
  data <- data.frame(y, x)
  colnames(data) <- c(yvar.names, xvar.names)
  ## ------------------------------------------------------------------------
  ##
  ##
  ## parse hidden options and set parameters
  ##
  ##
  ## ------------------------------------------------------------------------
  dots <- list(...)
  hidden <- get.varpro.hidden(dots, ntree)
  sampsize <- hidden$sampsize
  nsplit <- hidden$nsplit
  ntree.external <- hidden$ntree.external  
  ntime.external <- hidden$ntime.external
  ntree.reduce <- hidden$ntree.reduce
  dimension.index <- hidden$dimension.index
  use.rfq <- hidden$use.rfq
  iratio.threshold <- hidden$iratio.threshold
  rmst <- hidden$rmst
  maxit <- hidden$maxit
  split.weight.only <- hidden$split.weight.only
  split.weight.tolerance <- hidden$split.weight.tolerance
  nfolds <- hidden$nfolds
  ## set dimensions
  n <- nrow(x)
  p <- ncol(x)
  nvar <- min(nvar, p)
  ## set nodesize values: optimized for n and p
  nodesize <- set.nodesize(n, p, nodesize)
  nodesize.reduce <- set.nodesize(n, p, dots$nodesize.reduce)
  nodedepth.reduce <- set.nodedepth.reduce(n, p, dots$nodedepth.reduce)
  if (is.null(dots$sampsize)) {
    nodesize.external <- set.nodesize(n, p, dots$nodesize.external)
  }
  else {
    if (is.function(dots$sampsize)) {
      nodesize.external <- set.nodesize(dots$sampsize(n), p, dots$nodesize.external)
    }
    else {
      nodesize.external <- set.nodesize(dots$sampsize, p, dots$nodesize.external)
    }
  }
  ## user can pass in a custom split weight vector
  split.weight.custom <- FALSE
  if (!is.null(dots$split.weight.custom)) {
    xvar.wt <- rep(0, length(xvar.names))
    names(xvar.wt) <- xvar.names
    if (length(intersect(xvar.names, names(dots$split.weight.custom))) == 0) {
      stop("custom split weight set incorrectly: no variable names match the original data\n")
    }
    swt <- abs(dots$split.weight.custom)[intersect(xvar.names, names(dots$split.weight.custom))]
    xvar.wt[names(swt)] <- swt 
    pt <- xvar.wt > 0
    if (sparse) {
      xvar.wt[pt] <- (xvar.wt[pt]) ^ dimension.index(1)
    }
    xvar.wt <- xvar.wt / max(xvar.wt, na.rm = TRUE)
    if (verbose) {
      cat("user has provided there own split-weight, vector, split-weight step will be skipped\n")
    }
    split.weight <- split.weight.only <- FALSE
    split.weight.custom <- TRUE
  }
  ## ------------------------------------------------------------------------
  ##
  ##
  ## random survival forests for external estimator
  ## uses INBAG predicted values and not OOB 
  ##
  ##
  ## ------------------------------------------------------------------------
  if (family == "surv") {
    if (verbose) {
      cat("detected a survival family, using external estimator ...\n")
    }  
    ## survival forest used to calculate external estimator
    o.external <- rfsrc(f, data,
                        sampsize = sampsize,
                        ntree = ntree.external,
                        nodesize = nodesize.external,
                        ntime = ntime.external,
                        save.memory = TRUE,
                        perf.type = "none")
    ## use mortality for y
    if (is.null(rmst)) {
      y <- as.numeric(randomForestSRC::get.mv.predicted(o.external, oob = FALSE))
    }
    ## use rmst if user requests
    else {
      y <- get.rmst(o.external, rmst)
    }
    ## we now have regression
    if (!is.matrix(y)) {
      family <- "regr"
      yvar.names <- yfkname
      f <- as.formula(paste0(yfkname, "~."))
    }
    ## rmst is a vector --> we now have multivariate regression
    if (is.matrix(y)) {
      family <- "regr+"
      colnames(y) <- yvar.names <- paste0(yfkname, ".", 1:ncol(y))
      f <- randomForestSRC::get.mv.formula(yvar.names)
    }
    if (verbose) {
      cat("external estimation completed\n")
    }  
  }
  ## ------------------------------------------------------------------------
  ##
  ## store information for classification analysis
  ##
  ## ------------------------------------------------------------------------
  ## default setting
  imbalanced.flag <- FALSE
  if (family == "class") {
    ## number of class labels
    nclass <- length(levels(y))
    ## two class processing: class labels are mapped to {0, 1}
    if (nclass == 2) {
      ## majority label --> 0, minority label --> 1 
      y.frq <- table(y)
      class.labels <- names(y.frq)
      minority <- which.min(y.frq)
      majority <- setdiff(1:2, minority)
      yvar <- rep(0, length(y))
      yvar[y==class.labels[minority]] <- 1
      y <- factor(yvar, levels = c(0, 1))
      ## determine if imbalanced analysis is in play
      threshold <- as.numeric(min(y.frq, na.rm = TRUE) / sum(y.frq, na.rm = TRUE))
      iratio <- max(y.frq, na.rm = TRUE) / min(y.frq, na.rm = TRUE)
      imbalanced.flag <- (iratio > iratio.threshold) & use.rfq
    }
  }
  ## ------------------------------------------------------------------------
  ##
  ##
  ## final assembly of the data
  ##
  ##
  ## ------------------------------------------------------------------------
  data <- data.frame(y, x)
  colnames(data) <- c(yvar.names, xvar.names)
  ## ------------------------------------------------------------------------
  ##
  ##
  ## determine method used to generate split-weight calculation
  ## defaults to lasso+tree/vimp but user options can over-ride this
  ##
  ##
  ## ------------------------------------------------------------------------
  ## custom user setting
  if ((split.weight || split.weight.only) && !is.null(split.weight.method)) {
    use.lasso <- any(grepl("lasso", split.weight.method))
    use.tree <- any(grepl("tree", split.weight.method))
    use.vimp <- any(grepl("vimp", split.weight.method))
  }
  ## default setting
  else {
    use.lasso <- hidden$use.lasso 
    use.vimp <- set.use.vimp(n, p, dots$use.vimp)
    use.tree <- !use.vimp
  }
  ## ------------------------------------------------------------------------
  ##
  ##
  ## split-weight calculation: used for guiding rule generation
  ##
  ## first try lasso, followed by shallow forest
  ## we add the lasso coefficients to the relative split frequency
  ## in some cases we must use vimp
  ##
  ##
  ##
  ## ------------------------------------------------------------------------
  split.weight.raw <- list()
  if (split.weight || split.weight.only) {
    ## verbose output
    if (verbose) {
      cat("acquiring split-weights for guided rule generation ...\n")
    }
    ## initialize xvar.wt
    xvar.wt <- rep(0, p)
    names(xvar.wt) <- xvar.names
    ##---------------------------------------------------------
    ##
    ## lasso split weight calculation
    ## now allows factors by hot-encoding
    ## by default, lasso coefficients use 1 standard error rule
    ##
    ##---------------------------------------------------------
    if (use.lasso) {
      ## register DoMC and set the number of cores
      parallel <- myDoRegister(cores, parallel)
      ## regression
      if (family == "regr") {
        o.glmnet <- tryCatch(
              {suppressWarnings(cv.glmnet(scale(data.matrix(x)), y, nfolds = nfolds, parallel = parallel, maxit = maxit))},
          error=function(ex){NULL})
        if (!is.null(o.glmnet)) {
          beta <- coef(o.glmnet)[-1,]
          xvar.wt[names(beta)] <- abs(beta)
        }
      }
      ## mv-regression
      else if (family == "regr+") {
        o.glmnet <- tryCatch(
               {suppressWarnings(cv.glmnet(scale(data.matrix(x)), y,
                    nfolds = nfolds, parallel = parallel, maxit = maxit, family = "mgaussian"))}, error=function(ex){NULL})
        if (!is.null(o.glmnet)) {
          beta <- do.call(cbind, lapply(coef(o.glmnet), function(o) {o[-1,]}))
          xvar.wt[rownames(beta)] <- rowMeans(abs(beta), na.rm = TRUE)
        }
      }
      ## classification
      else if (family == "class") {
        ## two class
        if (nclass == 2) {
          o.glmnet <- tryCatch(
               {suppressWarnings(cv.glmnet(scale(data.matrix(x)), y,
                    nfolds = nfolds, parallel = parallel, maxit = maxit, family = "binomial"))}, error=function(ex){NULL})
          if (!is.null(o.glmnet)) {
            beta <- coef(o.glmnet)[-1,]
            xvar.wt[names(beta)] <- abs(beta)
          }
        }
        ## multiclass
        else {
          o.glmnet <- tryCatch(
               {suppressWarnings(cv.glmnet(scale(data.matrix(x)), y,
                    nfolds = nfolds, parallel = parallel, maxit = maxit, family = "multinomial"))}, error=function(ex){NULL})
          if (!is.null(o.glmnet)) {
            beta <- do.call(cbind, lapply(coef(o.glmnet), function(o) {o[-1,]}))
            xvar.wt[rownames(beta)] <- rowMeans(abs(beta), na.rm = TRUE)
          }
        }
      }
      ## something is wrong
      else {
        stop("family specified not currently supported: ", family)
      }
      ##----------------------------
      ##
      ## final lasso details
      ##
      ##----------------------------
      ## unregister the backend
      nullO <- myUnRegister(parallel)
      ## assign missing values NA
      xvar.wt[is.na(xvar.wt)] <- 0
      ## store split-weight information (expert mode)
      split.weight.raw$lasso <- xvar.wt
      ## scale the weights using sparse dimension.index
      pt <- xvar.wt > 0
      if (sum(pt) > 0) {
        if (sparse) {
          xvar.wt[pt] <- (xvar.wt[pt] / max(xvar.wt[pt], na.rm = TRUE)) ^ dimension.index(sum(pt))
        }
        else {
          xvar.wt[pt] <- (xvar.wt[pt] / max(xvar.wt[pt], na.rm = TRUE)) ^ dimension.index(1)
        }
      }
    }##end lasso split-weight calculation
    ##---------------------------------------------------------
    ##
    ## add vimp to split weight calculation
    ## sampsize is not deployed since this can non-intuitively slow calculations
    ##
    ##---------------------------------------------------------
    if (use.vimp) {
      ## regression
      if (family == "regr") {
        vmp <- rfsrc(f, data[, c(yvar.names, xvar.names)],
                     ntree = ntree,
                     nodesize = nodesize.reduce,
                     importance = "permute",
                     seed = seed)$importance 
      }
      ## mv-regression
      else if (family == "regr+") {
        vmp <- rowMeans(randomForestSRC::get.mv.vimp(rfsrc(f, data[, c(yvar.names, xvar.names)],
                                                                ntree = ntree,
                                                                nodesize = nodesize.reduce,
                                                                importance = "permute",
                                                                seed = seed)), na.rm = TRUE)
      } 
      ## classification
      ## for class imbalanced scenarios switch to rfq/gmean
      else {
        vmp <- rfsrc(f, data[, c(yvar.names, xvar.names)],
                     rfq = if (imbalanced.flag) TRUE else NULL,
                     perf.type = if (imbalanced.flag) "gmean" else NULL,
                     splitrule = if (imbalanced.flag) "auc" else NULL,
                     ntree = ntree,
                     nodesize = nodesize.reduce,
                     importance = "permute",
                     seed = seed)$importance[, 1]
      }
      ## store split-weight information (expert mode)
      split.weight.raw$vimp <- vmp
      ## scale the weights using dimension.index
      pt <- vmp > 0
      if (sum(pt) > 0) {
        if (sparse) {
          xvar.wt[pt] <- (xvar.wt[pt] +
                 (vmp[pt] / max(vmp[pt], na.rm = TRUE))  ^ dimension.index(sum(pt)))
        }
        else {
          xvar.wt[pt] <- (xvar.wt[pt] +
                 (vmp[pt] / max(vmp[pt], na.rm = TRUE))  ^ dimension.index(1))
        }
      }
      else {
        use.vimp <- FALSE
      }
    }##end vimp split-weight calculation
    ##---------------------------------------------------------
    ##
    ## add split relative frequency from shallow forest to split weights
    ##
    ##---------------------------------------------------------
    if (use.tree) {
      ## fast filtering based on number of splits
      xvar.used <- rfsrc(f, data,
                         splitrule = if (imbalanced.flag) "auc" else NULL,
                         sampsize = sampsize,
                         ntree = ntree.reduce, nodedepth = nodedepth.reduce,
                         mtry = Inf,
                         nsplit = 100,
                         var.used = "all.trees",
                         perf.type = "none")$var.used[xvar.names]
      ## store split-weight information (expert mode)
      split.weight.raw$tree <- xvar.used
      ## update the weights
      pt <- xvar.used >= set.xvar.cut(xvar.used, n)
      if (sum(pt) > 0) {
        if (sparse) {
          xvar.wt[pt] <- (xvar.wt[pt] +
              (xvar.used[pt] / max(xvar.used[pt], na.rm = TRUE))  ^ dimension.index(sum(pt)))
        }
        else {
          xvar.wt[pt] <- (xvar.wt[pt] +
              (xvar.used[pt] / max(xvar.used[pt], na.rm = TRUE))  ^ dimension.index(1))        
        }
      }
      else {
        pt <- which.max(xvar.used)
        xvar.wt[pt] <- 1
      }
    }##end shallow forest split-weight calculation
    ##---------------------------------------------------------
    ##
    ## final steps
    ##
    ##---------------------------------------------------------
    ## in case there was a total failure ...
    if (all(xvar.wt == 0)) {
      xvar.wt <- rep(1, p)
      names(xvar.wt) <- xvar.names
    }
    ## if the user only wants the xvar weights
    if (split.weight.only) {
      ## tolerance: keep weights from becoming too small
      xvar.wt <- tolerance(xvar.wt, split.weight.tolerance)
      return(list(
        split.weight = xvar.wt,
        xvar.org.names = xvar.org.names,
        xvar.names = xvar.names,
        yvar.names = yvar.names,
        x = x,
        y = y,
        y.org = y.org,
        family = family))
    }
  }###########split weight calculations end here
  ## ------------------------------------------------------------------------
  ##
  ## final split weight processing
  ##
  ## ------------------------------------------------------------------------
  if (split.weight || split.weight.only || split.weight.custom) {
    ## final assignment
    if (sum(xvar.wt > 0) > nvar) {
      pt <- order(xvar.wt, decreasing = TRUE)
      xvar.wt[setdiff(1:length(xvar.wt), pt[1:nvar])] <- 0
    }
    xvar.names <- xvar.names[xvar.wt > 0]
    xvar.wt <- xvar.wt[xvar.wt > 0]
    ## tolerance: keep weights from becoming too small
    xvar.wt <- tolerance(xvar.wt, split.weight.tolerance)
    ## verbose
    if (verbose) {
       if (!split.weight.custom) {
         cat("split weight calculation completed\n")
       }
      print(data.frame(xvar = xvar.names, weights = xvar.wt))
    }
    ## update the data
    data <- data[, c(yvar.names, xvar.names)]
    p <- length(xvar.names)
  }
  ## ------------------------------------------------------------------------
  ##
  ##
  ## model based rule generation: uses original y and original formula - true rules
  ## sampsize is not deployed since this can non-intuitively slow calculations
  ##
  ## survival families are now properly handled
  ##
  ## ------------------------------------------------------------------------
  if (verbose) {
    cat("model based rule generation ...\n")
  }  
  if (split.weight || split.weight.custom) {
    object <- rfsrc(if (family=="regr+") f else f.org,
                    if (family=="regr+") data else data.frame(y.org, data[, xvar.names, drop=FALSE]),
                    splitrule = if (imbalanced.flag) "auc" else NULL,
                    xvar.wt = xvar.wt,
                    ntree = ntree,
                    nsplit = nsplit(nrow(data)),
                    nodesize = nodesize,
                    perf.type = "none",
                    seed = seed)
  }
  else {
    object <- rfsrc(if (family=="regr+") f else f.org,
                    if (family=="regr+") data else data.frame(y.org, data[, xvar.names, drop=FALSE]),
                    splitrule = if (imbalanced.flag) "auc" else NULL,
                    mtry = if (is.null(dots$mtry)) Inf else dots$mtry,
                    ntree = ntree,
                    nsplit = nsplit(nrow(data)),
                    nodesize = nodesize,
                    perf.type = "none",
                    seed = seed)
  }
  ## ------------------------------------------------------------------------
  ##
  ##
  ## obtain varpro strength using direct C call via varpro.strength()
  ##
  ## note: for survival y.external --> external estimator
  ##
  ## ------------------------------------------------------------------------
  if (verbose) {
    cat("acquiring rules...\n")
  }
  var.strength <- get.varpro.strength(object = object,
                         max.rules.tree = max.rules.tree,
                         max.tree = max.tree,
                         y.external = data[, yvar.names])$results
  if (verbose) {
    cat("done!\n")
  }
  ## ------------------------------------------------------------------------
  ##
  ##
  ## return the goodies
  ##
  ##
  ## ------------------------------------------------------------------------
  rO <- list(
    rf = object,
    split.weight = if (split.weight || split.weight.custom) xvar.wt else NULL,
    split.weight.raw = split.weight.raw,
    max.rules.tree = max.rules.tree,
    max.tree = max.tree,
    results = var.strength,
    xvar.org.names = xvar.org.names,
    xvar.names = xvar.names,
    yvar.names = yvar.names,
    x = x,
    y = y,
    y.org = y.org[, 1:ncol(y.org)],
    family = object$family)
  class(rO) <- "varpro"
  return(rO)
}
## legacy
varPro <- varpro
