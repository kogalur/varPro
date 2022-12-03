unsupv.varpro <- function(data, nvar = 5, cutoff = NULL,
                          method = c("sh1", "sh2"), cv = FALSE,
                          auto.encoder = FALSE, tolerance = 1e-10,                          
                          ntree = 150, nodesize = NULL,
                          max.rules.tree = 150, max.tree = min(150, ntree),
                          papply = mclapply, verbose = FALSE, seed = NULL,
                          ...)
{		   
  ##------------------------------------------------------------------
  ##
  ##
  ## pre-processing 
  ##
  ##
  ##------------------------------------------------------------------
  ## remove any column with less than two unique values
  void.var <- sapply(data, function(x){length(unique(x, na.rm = TRUE)) < 2})
  if (sum(void.var) > 0) {
    data[, which(void.var)] <- NULL
  }
  ## decide which method to use
  method <- match.arg(method, c("sh1", "sh2"))
  if (method == "sh1") {
    mode <- 1
  }
  else {
    mode <- 2
  }
  ##--------------------------------------------------------------
  ##
  ## extract additional options specified by user
  ##
  ##--------------------------------------------------------------
  dots <- list(...)
  f <- as.formula("classes ~ .")
  varpro.names <- c(get.varpro.names())
  cv.varpro.names <- c(varpro.names, "zcut", "nblocks", "fast", "crps")
  ##------------------------------------------------------------------
  ##
  ## DEFAULT METHOD
  ##  
  ## use varPro on artificially created two class data
  ## - works if there is only 1 variable left
  ## - there needs to be enough unique values always 
  ##
  ##
  ##------------------------------------------------------------------
  if (!auto.encoder) {
    ## if nvar >= p nothing to do (assumes default settings for cutoff and cv)
    if (nvar >= nrow(data) && is.null(cutoff) && !cv) {
      return(colnames(data))
    }
    ## standard varpro call
    if (!cv) {
      o <- do.call(varpro, c(list(f = f, data = make.sh(data, mode)),
                             ntree = ntree, nodesize = nodesize,
                             max.rules.tree = max.rules.tree, max.tree = max.tree,
                             papply = papply, verbose = verbose, seed = seed,
                             dots[names(dots) %in% varpro.names]))
      if (is.null(cutoff)) {
        bestvar <- rownames(importance(o, cutoff = 0)$unconditional)
        bestvar[1:min(nvar, length(bestvar))]
      }
      else {
        imp <- importance(o, cutoff = cutoff)$unconditional
        imp[is.na(imp)] <- 0
        if (sum(imp$selected == 1, na.rm = TRUE) > 0) {
          rownames(imp[imp$selected == 1,, drop = FALSE])
        }
        else {
          NULL
        }
      }
    }
    ## use cv to select variables (slower, but more accurate)
    else {
      o <- do.call(cv.varpro, c(list(f, data = make.sh(data, mode)),
                                ntree = ntree, nodesize = nodesize,
                                max.rules.tree = max.rules.tree, max.tree = max.tree,
                                papply = papply, verbose = verbose, seed = seed,
                                dots[names(dots) %in% cv.varpro.names]))
      ## return all three options
      bestvar <- lapply(1:3, function(j) {rownames(o[[j]])})
      names(bestvar) <- names(o)[1:3]
      bestvar
    }
  }
  ##------------------------------------------------------------------
  ##
  ## autoencoder
  ##  
  ## obtain weights from varPro run on artificially created two class data
  ## select features using nvar
  ## run multivariate forests of x against x (so-called autoencoder)
  ##
  ##
  ##------------------------------------------------------------------
  else {
    ## if nvar >= p nothing to do
    if (nvar >= nrow(data)) {
      return(colnames(data))
    }
    ##------------------------------------------------------------------
    ##
    ##
    ## use varPro on artificially created two class data
    ## extract weights only
    ##
    ##
    ##------------------------------------------------------------------
    dots$split.weight.only <- TRUE
    xvar.weight <- do.call(varpro, c(list(f = f, data = make.sh(data, mode)),
                                     ntree = ntree, nodesize = nodesize,
                                     max.rules.tree = max.rules.tree, max.tree = max.tree,
                                     papply = papply, verbose = verbose, seed = seed,
                                     dots[names(dots) %in% varpro.names]))
    ##------------------------------------------------------------------
    ##
    ##
    ## filter variables
    ##
    ##
    ##------------------------------------------------------------------
    xvar.weight <- sort(xvar.weight, decreasing = TRUE)
    xvar.names <- names(xvar.weight)
    pt <- xvar.weight > tolerance
    if (sum(pt)  > 0) {
      ntop <- min(nvar, sum(pt), length(xvar.names))
      xvar.names <- xvar.names[1:ntop]
      xvar.weight <- xvar.weight[1:ntop]
    }
    else {
      return(xvar.names[1])
    }
    ##------------------------------------------------------------------
    ##
    ##
    ## run regr+ with weighted mtry 
    ##
    ##
    ##------------------------------------------------------------------
    p <- length(xvar.names)
    mv.dta <- data.frame(y = data[, xvar.names], data[, xvar.names])
    f <- randomForestSRC::get.mv.formula(colnames(mv.dta)[1:p])
    o <- rfsrc(f, mv.dta, xvar.weight = xvar.weight, perf.type = "none")
    ##------------------------------------------------------------------
    ##
    ##
    ## call varpro.strength
    ##
    ##
    ##------------------------------------------------------------------
    oo <- get.varpro.strength(o)
    ##------------------------------------------------------------------
    ##
    ##
    ## return the importance values
    ##
    ##
    ##------------------------------------------------------------------
    vmp <- importance(oo, ...)
    names(vmp) <- xvar.names
    vmp.matrix <- do.call(rbind, lapply(vmp, function(imp) {
      imp$z
    }))
    colnames(vmp.matrix) <- xvar.names
    vmp.matrix
  }
}
