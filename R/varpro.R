################################################################
###  variable priority (varPro) for regression, classification and survival
###
###  TBD
###  - class imbalanced analysis could potentially be improved
###
### ---------------------------------------------------------------
###  Written by:
###
###  Hemant Ishwaran                     hemant.ishwaran@gmail.com
###  Division of Biostatistics           
###  Clinical Research Building
###  1120 NW 14th Street
###  University of Miami, Miami FL 33136
###
###  https:
###  -------------------------------------------------------------
###
###  THIS PROGRAM SHOULD NOT BE COPIED, USED, MODIFIED, OR 
###  DISSEMINATED IN ANY WAY WITHOUT SPECIFIC WRITTEN PERMISSION 
###  FROM THE AUTHOR.
###
####################################################################
varpro <- function(f, data, ntree = 500, split.weight = TRUE,
                   nodesize = NULL,
                   max.rules.tree = 150, max.tree = min(150, ntree),
                   parallel = TRUE, cores = get.number.cores(),
                   papply = mclapply, verbose = FALSE, seed = NULL,
                   ...)
{
  ## ------------------------------------------------------------------------
  ##
  ##
  ## process data, check coherence of formula, data etc.
  ##
  ##
  ## ------------------------------------------------------------------------
  ## formula must be a formula
  f <- as.formula(f)
  ## data must be a data frame
  data <- data.frame(data)
  ## droplevels
  data <- droplevels(data)
  ## initialize the seed
  seed <- get.seed(seed)
  ## run a stumpy tree as a quick way to extract  x, y and determine family
  ## this also cleans up missing data 
  stump <- rfsrc(f, data, mtry = 1, splitrule="random", nodedepth=0, perf.type = "none", save.memory = TRUE, ntree=1)
  yvar.names <- stump$yvar.names
  xvar.names <- stump$xvar.names
  y <- stump$yvar
  y.org <- cbind(y)
  colnames(y.org) <- stump$yvar.names
  x <- stump$xvar
  p <- length(xvar.names)
  n <- nrow(x)
  family <- stump$family
  rm(stump)
  gc()
  ## coherence check
  if (!(family == "regr" || family == "class" || family == "surv")) {
    stop("this function only works for regression, classification and survival")
  }
  ## tree rules are encoded in terms of factor levels, therefore recode all
  ## factors so that their levels correspond to integer values
  anyF <- sapply(x, is.factor)
  if (sum(anyF) > 0) {
    lapply(names(which(anyF)), function(nn) {
      xn <- x[, nn]
      x[, nn] <<- factor(xn, levels(xn), 1:length(levels(xn)))
    })
  }
  ## set nodesize
  if (is.null(nodesize)) {
    nodesize <- max(2, nrow(x) / 200)
  }
  ## ------------------------------------------------------------------------
  ##
  ##
  ## assemble the data
  ##
  ##
  ## ------------------------------------------------------------------------
  data <- data.frame(y, x)
  colnames(data)[1:length(yvar.names)] <- yvar.names
  ## ------------------------------------------------------------------------
  ##
  ##
  ## parse hidden options and set parameters
  ##
  ##
  ## ------------------------------------------------------------------------
  hidden <- get.varpro.hidden(list(...), ntree)
  sampsize <- hidden$sampsize
  nsplit <- hidden$nsplit
  ntree.external <- hidden$ntree.external  
  nodesize.external <- hidden$nodesize.external
  ntime.external <- hidden$ntime.external
  nodesize.reduce <- hidden$nodesize.reduce
  ntree.reduce <- hidden$ntree.reduce
  nodedepth.reduce <- hidden$nodedepth.reduce
  dimension.n <- hidden$dimension.n
  dimension.p <- hidden$dimension.p
  dimension.q <- hidden$dimension.q
  dimension.index <- hidden$dimension.index
  dimension.ir <- hidden$dimension.ir
  rmst <- hidden$rmst
  other.external <- hidden$other.external
  maxit <- hidden$maxit
  split.weight.only <- hidden$split.weight.only
  use.lasso <- hidden$use.lasso
  use.vimp <- hidden$use.vimp
  sparse <- hidden$sparse
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
      cat("detected a survival family, using RSF to calculate external estimator...\n")
    }  
    ## survival forest used to calculate external estimator
    o.external <- rfsrc(f, data, sampsize = sampsize,
         ntree = ntree.external, save.memory = TRUE,
         perf.type = "none", nodesize = nodesize.external, ntime = ntime.external)
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
      yvar.names <- "y"
      f <- as.formula(y ~ .)
    }
    ## rmst is a vector --> we now have multivariate regression
    if (is.matrix(y)) {
      family <- "regr+"
      colnames(y) <- yvar.names <- paste0("y.", 1:ncol(y))
      f <- randomForestSRC::get.mv.formula(yvar.names)
    }
  }
  ## ------------------------------------------------------------------------
  ##
  ## store information for classification analysis
  ##
  ## ------------------------------------------------------------------------
  if (family == "class") {
    ## length of output
    J <- length(levels(y))
    ## two class processing
    ## class labels are mapped to {0, 1}
    if (J == 2) {
      ## majority label --> 0, minority label --> 1 
      y.frq <- table(y)
      class.labels <- names(y.frq)
      minority <- which.min(y.frq)
      majority <- setdiff(1:2, minority)
      yvar <- rep(0, length(y))
      yvar[y==class.labels[minority]] <- 1
      y <- factor(yvar, levels = c(0, 1))
      ## extract useful parameters
      threshold <- as.numeric(min(y.frq, na.rm = TRUE) / sum(y.frq, na.rm = TRUE))
      iratio <- max(y.frq, na.rm = TRUE) / min(y.frq, na.rm = TRUE)
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
  colnames(data)[1:length(yvar.names)] <- yvar.names
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
  if (split.weight) {
    ## verbose output
    if (verbose) {
      cat("acquiring split-weights for guided rule generation ...\n")
    }
    ## initialize xvar.wt
    xvar.wt <- rep(0, p)
    ##---------------------------------------------------------
    ##
    ## lasso split weight calculation
    ## now allows factors by converting them brute force using data.matrix
    ##
    ##---------------------------------------------------------
    ## uncomment if factors not allowed
    #if (sum(anyF) > 0) {
    #  use.lasso <- FALSE
    #}
    if (use.lasso) {
      ## register DoMC and set the number of cores
      regO <- myDoRegister(cores, parallel)
      parallel <- regO$parallel
      nfolds <- regO$nfolds
      ## regression
      if (family == "regr") {
        o.glmnet <- tryCatch(
              {suppressWarnings(cv.glmnet(scale(data.matrix(x)), y, nfolds = nfolds, parallel = parallel, maxit = maxit))},
          error=function(ex){NULL})
        if (!is.null(o.glmnet)) {
          xvar.wt <-  abs(as.numeric(coef(o.glmnet)[-1]))            
        }
      }
      ## mv-regression
      else if (family == "regr+") {
        o.glmnet <- tryCatch(
               {suppressWarnings(cv.glmnet(scale(data.matrix(x)), y,
                    nfolds = nfolds, parallel = parallel, maxit = maxit, family = "mgaussian"))}, error=function(ex){NULL})
        if (!is.null(o.glmnet)) {
          beta <- do.call(cbind, lapply(coef(o.glmnet), function(o) {as.numeric(o)[-1]}))
          xvar.wt <- rowMeans(abs(beta), na.rm = TRUE)
        }
      }
      ## classification
      else {
        o.glmnet <- tryCatch(
               {suppressWarnings(cv.glmnet(scale(data.matrix(x)), y,
                    nfolds = nfolds, parallel = parallel, maxit = maxit, family = "multinomial"))}, error=function(ex){NULL})
        if (!is.null(o.glmnet)) {
          beta <- do.call(cbind, lapply(coef(o.glmnet), function(o) {as.numeric(o)[-1]}))
          xvar.wt <- rowMeans(abs(beta), na.rm = TRUE)
        }
      }
      ## unregister the backend
      nullO <- myUnRegister(parallel)
      ## assign missing values NA
      xvar.wt[is.na(xvar.wt)] <- 0
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
    }
    ##---------------------------------------------------------
    ##
    ## add vimp to lasso split-weight calculation
    ## REQUIRES lasso to be unsuccessful & not big p not big n
    ## sampsize is not deployed since this can non-intuitively slow calculations
    ##
    ##---------------------------------------------------------
    if (n >= dimension.n || p >= dimension.p) {
      use.vimp <- FALSE
    }
    if (use.vimp) {
      if (sum(xvar.wt != 0) > 0) {
        ## regression
        if (family == "regr") {
          vmp <- rfsrc(y~., data.frame(y = y, x[, xvar.names, drop = FALSE]),
                       ntree = ntree, nodesize = nodesize.reduce, importance = "permute",
                       seed = seed)$importance 
        }
        ## mv-regression
        else if (family == "regr+") {
          vmp <- rowMeans(randomForestSRC::get.mv.vimp(rfsrc(f, data.frame(y, x[, xvar.names, drop = FALSE]),
                ntree = ntree, nodesize = nodesize.reduce, importance = "permute",
                seed = seed)), na.rm = TRUE)
        } 
        ## classification
        else {
          ## determine if this is a class imbalanced scenario, if so switch to gmean performance
          ## anti is too agressive when IR is high, so use permute in this case
          iflag <- J == 2 && iratio > dimension.ir
          vmp <- rfsrc(y~., data.frame(y = y , x[, xvar.names, drop = FALSE]),
                       perf.type = if (iflag) "gmean" else NULL,
                       ntree = ntree, nodesize = nodesize.reduce,
                       importance = if (iflag) "permute" else "anti",
                       seed = seed)$importance[, 1]
        }
        ## scale the weights using dimension.index
        pt <- vmp > 0
        if (sum(pt) > 0) {
          if (sparse) {
            vmpsc <- (vmp[vmp > 0]) ^ dimension.index(sum(pt))
          }
          else {
            vmpsc <- (vmp[vmp > 0]) ^ dimension.index(1)
          }
          xvar.wt[pt] <- xvar.wt[pt] + vmpsc
          xvar.wt <- xvar.wt / max(xvar.wt, na.rm = TRUE)
        }
        else {
          use.vimp <- FALSE
        }
      }
      else {
        use.vimp <- FALSE
      }
    }
    ##---------------------------------------------------------
    ##
    ## otherwise add relative frequency from shallow forest to lasso
    ##
    ##---------------------------------------------------------
    if (!use.vimp) {
      ## fast filtering based on number of splits
      xvar.used <- rfsrc(f, data, sampsize = sampsize,
                         ntree = ntree.reduce, nodedepth = nodedepth.reduce(nrow(data), ncol(data)),
                         perf.type = "none", var.used = "all.trees", mtry = Inf, nsplit = 100)$var.used
      ## assign relative frequency cutoff
      if (n >= dimension.n) {
        xvar.cut <- quantile(xvar.used, prob = dimension.q, na.rm = TRUE)
      }
      else {
        xvar.cut <- 1
      }
      ## update the weights
      pt <- xvar.used >= xvar.cut
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
    }
    ##---------------------------------------------------------
    ##
    ## final processing
    ##
    ##---------------------------------------------------------
    ## in case there was a total failure ...
    if (all(xvar.wt == 0)) {
      xvar.wt <- rep(1, p)
    }
    ## if the user only wants the xvar weights
    if (split.weight.only) {
      names(xvar.wt) <- xvar.names
      return(xvar.wt)
    }
    ## final assignment
    xvar.names <- xvar.names[xvar.wt > 0]
    xvar.wt <- xvar.wt[xvar.wt > 0]
    ## verbose
    if (verbose) {
      cat("split weight calculation completed\n")
      print(data.frame(xvar = xvar.names, weights = xvar.wt))
    }
    ## update the data
    data <- data.frame(y = y, x[, xvar.names, drop = FALSE])
    colnames(data)[1:length(yvar.names)] <- yvar.names
    p <- length(xvar.names)
  }
  ## ------------------------------------------------------------------------
  ##
  ##
  ## model based rule generation
  ## sampsize is not deployed since this can non-intuitively slow calculations
  ##
  ## ------------------------------------------------------------------------
  if (verbose) {
    cat("model based rule generation...\n")
  }  
  if (split.weight) {
    object <- rfsrc(f, data,
                    xvar.wt = xvar.wt,
                    #sampsize = sampsize,
                    ntree = ntree,
                    nsplit = nsplit(nrow(data)),
                    nodesize = nodesize,
                    perf.type = "none",
                    seed = seed)
  }
  else {
    object <- rfsrc(f,
                    data,
                    mtry = Inf,
                    #sampsize = sampsize,
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
  ##
  ## ------------------------------------------------------------------------
  if (verbose) {
    cat("acquiring rules...\n")
  }  
  var.strength <- varpro.strength(object = object,
                                  max.rules.tree = max.rules.tree,
                                  max.tree = max.tree)$strengthArray
  ## process the strength array
  var.strength <- get.varpro.strengthArray(var.strength, family, y)
  ## ------------------------------------------------------------------------
  ##
  ##
  ## return the goodies
  ##
  ##
  ## ------------------------------------------------------------------------
  rO <- list(
    rf = object,
    max.rules.tree = max.rules.tree,
    max.tree = max.tree,
    results = var.strength,
    xvar.names = xvar.names,
    yvar.names = yvar.names,
    y = y,
    y.org = y.org,
    family = family)
  class(rO) <- "varpro"
  return(rO)
}
## legacy
varPro <- varpro
