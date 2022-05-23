################################################################
###  variable priority (varPro) for regression, classification and survival
###
###  - external estimator used for survival
###  - classification/multiclass returns unconditional and conditional importance
###  - lasso+tree split-weights
###  - allows rmst vector --> mv-regression
###
###  TBD
###  - in p>>n, nodesize and nodedepth.reduce should be lowered
###    due to small sample sizes; can this be automated?
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
###  THIS PROGRAM SHOULD NOT BE COPIED, USED, MODIFIED, OR 
###  DISSEMINATED IN ANY WAY WITHOUT SPECIFIC WRITTEN PERMISSION 
###  FROM THE AUTHOR.
####################################################################
varpro <- function(f, data, ntree = 500, split.weight = TRUE,
                   nodesize = if (split.weight) 5 else 10,
                   max.rules.tree = 150, max.tree = min(150, ntree),
                   papply = mclapply, verbose = FALSE, ...)
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
  ## run a stumpy tree as a quick way to extract  x, y and determine family
  ## this also cleans up missing data 
  stump <- rfsrc(f, data, splitrule="random", nodedepth=0, ntree=1)
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
  ## ------------------------------------------------------------------------
  ##
  ##
  ## parse hidden options and set parameters
  ##
  ##
  ## ------------------------------------------------------------------------
  hidden <- get.varpro.hidden(list(...), ntree)
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
    o.external <- rfsrc(f, data, ntree = ntree.external, save.memory = TRUE,
                    nodesize = nodesize.external, ntime = ntime.external)
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
  ## variance of y needed to standardize test statistics
  ## last chance to set the response dimension for non-classification families
  ##
  ## ------------------------------------------------------------------------
  var.y <- NULL  
  if (family != "class") {
    if (family == "regr") {
      var.y <- var(y, na.rm = TRUE)
      rdim <- 1
    }
    if (family == "regr+") {
      var.y <- as.numeric(diag(var(y, na.rm = TRUE)))
      rdim <- length(var.y)
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
    rdim <- J + 1
    ## two class processing (used to populate CLASSOBJ)
    ## class labels are mapped to {0, 1}
    if (J == 2) {
      ## map majority label --> 0, minority label --> 1 
      y.frq <- table(y)
      class.labels <- names(y.frq)
      minority <- which.min(y.frq)
      majority <- setdiff(1:2, minority)
      yvar <- rep(0, length(y))
      yvar[y==class.labels[minority]] <- 1
      y <- factor(yvar, levels = c(0, 1))
      ## store useful parameters
      threshold <- as.numeric(min(y.frq, na.rm = TRUE) / sum(y.frq, na.rm = TRUE))
      iratio <- max(y.frq, na.rm = TRUE) / min(y.frq, na.rm = TRUE)
      classObj <- list(J = 2,
                       pihat = table(y) / length(y),
                       phat = NULL,
                       threshold = threshold,
                       iratio = iratio, 
                       ithreshold = dimension.ir)
    }
    ## J > 2 case
    else {
      pihat <- 
      classObj <- list(J = J, pihat = table(y) / length(y), phat = NULL)
    }
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
    lasso.flag <- sum(anyF) == 0
    ##---------------------------------------------------------
    ##
    ## lasso split weight calculation (no factors in X allowed)
    ##
    ##---------------------------------------------------------
    if (lasso.flag) {
      ## regression
      if (family == "regr") {
        o.glmnet <- tryCatch(
              {suppressWarnings(cv.glmnet(scale(data.matrix(x)), y, maxit = maxit))},
          error=function(ex){NULL})
        if (!is.null(o.glmnet)) {
          xvar.wt <-  abs(as.numeric(coef(o.glmnet)[-1]))            
        }
      }
      ## mv-regression
      else if (family == "regr+") {
        o.glmnet <- tryCatch(
               {suppressWarnings(cv.glmnet(scale(data.matrix(x)), y,
                    maxit = maxit, family = "mgaussian"))}, error=function(ex){NULL})
        if (!is.null(o.glmnet)) {
          beta <- do.call(cbind, lapply(coef(o.glmnet), function(o) {as.numeric(o)[-1]}))
          xvar.wt <- rowMeans(abs(beta), na.rm = TRUE)
        }
      }
      ## classification
      else {
        o.glmnet <- tryCatch(
               {suppressWarnings(cv.glmnet(scale(data.matrix(x)), y,
                    maxit = maxit, family = "multinomial"))}, error=function(ex){NULL})
        if (!is.null(o.glmnet)) {
          beta <- do.call(cbind, lapply(coef(o.glmnet), function(o) {as.numeric(o)[-1]}))
          xvar.wt <- rowMeans(abs(beta), na.rm = TRUE)
        }
      }
      ## assign missing values NA
      xvar.wt[is.na(xvar.wt)] <- 0
    }
    ##---------------------------------------------------------
    ##
    ## add vimp to lasso split-weight calculation
    ## REQUIRES lasso to be unsuccessful, not big p not big n
    ##
    ##---------------------------------------------------------
    use.vimp <- FALSE
    if (sum(xvar.wt != 0) <= 1 & n < dimension.n & p < dimension.p) {
      ## regression
      if (family == "regr") {
        vmp <- rfsrc(y~., data.frame(y = y, x[, xvar.names, drop = FALSE]),
                     ntree = ntree, nodesize = nodesize.reduce, importance = "permute")$importance 
      }
      ## mv-regression
      else if (family == "regr+") {
        vmp <- rowMeans(randomForestSRC::get.mv.vimp(rfsrc(f, data.frame(y, x[, xvar.names, drop = FALSE]),
                     ntree = ntree, nodesize = nodesize.reduce, importance = "permute")), na.rm = TRUE)
      } 
      ## classification
      else {
        ## determine if this is a class imbalanced scenario, if so switch to gmean performance
        ## anti is too agressive when IR is high, so use permute in this case
        iflag <- classObj$J == 2 && iratio > dimension.ir
        vmp <- rfsrc(y~., data.frame(y = y , x[, xvar.names, drop = FALSE]),
                     perf.type = if (iflag) "gmean" else NULL,
                     ntree = ntree, nodesize = nodesize.reduce,
                     importance = if (iflag) "permute" else "anti")$importance[, 1]
      }
      ## scale the weights using dimension.index
      if (sum(vmp > 0) > 0) {
        use.vimp <- TRUE
        vmpsc <- (vmp[vmp > 0]) ^ dimension.index
        xvar.wt[vmp > 0] <- xvar.wt[vmp > 0] + vmpsc
        xvar.wt <- xvar.wt / max(xvar.wt, na.rm = TRUE)
      }
    }
    ##---------------------------------------------------------
    ##
    ## otherwise add relative frequency from shallow forest to lasso
    ##
    ##---------------------------------------------------------
    if (!use.vimp) {
      ## fast filtering based on number of splits
      xvar.used <- rfsrc(f, data, ntree = ntree.reduce, nodedepth = nodedepth.reduce,
                         var.used="all.trees", mtry = Inf, nsplit = 100)$var.used
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
        xvar.wt[pt] <- xvar.wt[pt] + (xvar.used[pt] / max(xvar.used[pt], na.rm = TRUE))  ^ dimension.index
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
  ##
  ##
  ## ------------------------------------------------------------------------
  if (verbose) {
    cat("model based rule generation...\n")
  }  
  if (split.weight) {
    object <- rfsrc(f, data,
                    xvar.wt = xvar.wt,
                    ntree = ntree,
                    nsplit = 0,
                    nodesize = nodesize,
                    membership = TRUE)
  }
  else {
    object <- rfsrc(f,
                    data,
                    mtry = Inf,
                    ntree = ntree,
                    nodesize = nodesize,
                    membership = TRUE)
  }
  ## ------------------------------------------------------------------------
  ##
  ##
  ## regression/class external estimators can be added here
  ## experimental, not used 
  ##
  ##
  ## ------------------------------------------------------------------------
  if (other.external && family == "regr") {
    y <- object$predicted.oob
  }
  if (other.external && family == "class") {
    classObj$phat <- object$predicted.oob
  }
  ## ------------------------------------------------------------------------
  ##
  ##
  ## extract inbag membership
  ##
  ##
  ## ------------------------------------------------------------------------
  inbag <- object$inbag
  ## ------------------------------------------------------------------------
  ##
  ##
  ## pull tree rules
  ##
  ##
  ## ------------------------------------------------------------------------
  if (verbose) {
    cat("now extracting tree rules...\n")
  }    
  ## thin out the number of trees
  ntreeSeq <- resample(1:ntree, size = min(ntree, max.tree), replace = FALSE)
  ## extract tree rules
  to <- getFastTreeRule(object, papply = papply, ntreeSeq = ntreeSeq)
  ## exit if NULL
  if (is.null(to)) {
    stop("no rules were generated, check your data\n")
  }
  ## new tree sequence
  treeSeq <- 1:max.tree
  ## thin out the number of rules
  pt.rule <- unlist(lapply(treeSeq, function(b) {
    pt <- to$treeID == b
    resample(which(pt), size = min(max.rules.tree, sum(pt)))
  }))
  ## assemble the final values
  treeRule <- to$treeRule[pt.rule]
  varNames <- to$varNames[pt.rule]
  varUsed <- to$varUsed[pt.rule]
  tree <- to$treeID[pt.rule]
  gc()
  ## ------------------------------------------------------------------------
  ##
  ##
  ## variable acquisition step
  ##
  ##
  ## ------------------------------------------------------------------------
  if (verbose) {
    cat("now acquiring variable strength...\n")
  }    
  ## set the parallelization depending on size of
  if (n < dimension.n) {
    papply1 <- papply
    papply2 <- lapply
  }
  else {
    papply1 <- lapply
    papply2 <- papply
  }
  stepcounter <- max(1, round(length(treeSeq) / 20))
  ##---------------------------------------------------------------------
  ##
  ## loop over each tree
  ##
  ##---------------------------------------------------------------------
  var.strength <- papply1(treeSeq, function(ii) {
    ## restrict the data to a specific tree
    pt <- tree == ii
    trule <- treeRule[pt]
    varnames <- varNames[pt]
    varused <- varUsed[pt]
    oob <- inbag[, treeSeq[ii]] == 0
    if (verbose && ii%%stepcounter == 0) {
      cat("(tree, nrules)=(", ii, ",", length(trule), ")\n")
    }
    ##---------------------------------------------------------------------
    ##
    ## loop over each branch acquiring variable strength 
    ##
    ##---------------------------------------------------------------------
    strength <- papply2(1:length(trule), function(j) {
      ##---------------------------------------------------------------------
      ##
      ## branch/rule acquisition
      ##
      ##---------------------------------------------------------------------
      lj <- length(varused[[j]])
      myrulej <- parseRule(trule[[j]])
      branchj <- eval(parse(text=myrulej))
      ## estimators for the branch
      ptj.oob <- oob & branchj
      estimatorj.oob <- varpro.estimator(y, ptj.oob, ptj.oob, var.y, classObj)
      nj.oob <- estimatorj.oob$n
      ##---------------------------------------------------------------------
      ##
      ## varPro importance calculation
      ##
      ##---------------------------------------------------------------------
      ## there is more than one variable in the rule
      if (lj > 1) {
        sj <- do.call(rbind, lapply(1:lj, function(k) {
          ##identify neighborhood of the rule to be released for the target variable
          kseq <- which(!(varnames[[j]] %in% varused[[j]][k]))
          myrulejk <- parseRule(trule[[j]][kseq])
          branchjk <- eval(parse(text=myrulejk))
          if (split.weight) {
            ptjk.complement.oob <- setdiff(which(oob & branchjk), which(ptj.oob))
          }
          else {
            ptjk.complement.oob <- which(oob & branchjk)#original estimator
          }
          ##--------------------------------------------------------------------------
          ##
          ## key step where we calculate importance
          ##
          ##--------------------------------------------------------------------------
          ## use complement of cases in released branch to form "perturbed" predictor
          if (nj.oob[1] > 0 & length(ptjk.complement.oob) > 0) {
            estimatorjk.complement.oob <- varpro.estimator(y, ptj.oob, ptjk.complement.oob, var.y, classObj)
            vmpjk.oob <- estimatorjk.complement.oob$est
          }
          else {
            vmpjk.oob <- rep(NA, rdim)
          }
          c(ii,
            j, 
            k,
            which(xvar.names == varused[[j]][k]),
            nj.oob,
            vmpjk.oob
            )
        }))
      sj
      }
      ## ---------------------------------------------------------------------------------
      ##
      ## only one variable is in the rule - this is likely a strong variable, so take care!
      ##
      ## ---------------------------------------------------------------------------------
      else {
        if (split.weight) {
          ptj.complement.oob <- setdiff(which(oob), which(ptj.oob))
        }
        else {
          ptj.complement.oob <- which(oob)#original estimator
        }
        if (nj.oob[1] > 0 & sum(ptj.complement.oob) > 0) {
          estimatorj.oob <- varpro.estimator(y, ptj.oob, ptj.complement.oob, var.y, classObj)
          vmpj.oob <- estimatorj.oob$est 
        }
        else {
          vmpj.oob <- rep(NA, rdim)
        }
        c(ii,
          j,
          1,
          which(xvar.names == varused[[j]]),
          nj.oob,
          vmpj.oob
          )
      }
    })## complete loop over rules
    ## remove NULL entries, convert to matrix
    strength <- strength[!sapply(strength, is.null)]
    if (length(strength) > 0) {
      strength <- do.call(rbind, strength)
      ## regression
      if (family == "regr") {
        strength.nms <- c("tree",
                          "branch",
                          "unique",
                          "variable",
                          "n.oob",
                          "imp")
      }
      ## mv-regression
      else if (family == "regr+") {
        strength.nms <- c("tree",
                          "branch",
                          "unique",
                          "variable",
                          "n.oob",
                          paste0("imp.", 1:ncol(y)))
      }
      ## classification
      else {
        strength.nms <- c("tree",
                          "branch",
                          "unique",
                          "variable",
                          c("n.oob", paste0("n.oob.", 1:J)),
                          c("imp", paste0("imp.", 1:J)))
      }
      colnames(strength) <- strength.nms
      strength
    }
    else {
      NULL
    }
  })
  if (verbose) {
    cat("done\n")
  }
  ## ------------------------------------------------------------------------
  ##
  ##
  ## output results
  ##
  ##
  ## ------------------------------------------------------------------------
  rO <- list(results = data.frame(do.call(rbind, var.strength)),
             xvar.names = xvar.names,
             yvar.names = yvar.names,
             y = y,
             y.org = y.org,
             family = family)
  class(rO) <- "varpro"
  rO
}
## for legacy
varPro <- varpro
