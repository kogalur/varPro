unsupv.varpro <- function(data, nvar = 20, 
                          mode = c("sh1", "sh2", "none"), 
                          tolerance = 1e-10,                          
                          ntree = 150, nodesize = NULL,
                          max.rules.tree = 150, max.tree = 150,
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
  ## decide which mode to use
  mode <- match.arg(mode, c("sh1", "sh2", "none"))
  if (mode == "sh1") {
    mode <- 1
  }
  if (mode == "sh2") {
    mode <- 2
  }
  ##--------------------------------------------------------------
  ##
  ## extract additional options specified by user
  ## define the entropy function used for importance
  ##
  ##--------------------------------------------------------------
  dots <- list(...)
  f <- as.formula("classes ~ .")
  varpro.names <- c(get.varpro.names())
  ## default entropy is the mean difference in variance
  if (is.null(dots$entropy)) {
    entropy <- function(xC, xO) {
      mean(abs(apply(xC, 2, sd, na.rm = TRUE) - apply(xO, 2, sd, na.rm = TRUE)))
    }
  }
  if (mode != "none") {
    ##------------------------------------------------------------------
    ##
    ##
    ## use varPro on artificially created two class data
    ## extract weights only
    ##
    ##
    ##------------------------------------------------------------------
    dots$split.weight.only <- TRUE
    max.tree <- min(150, ntree)  
    o <- do.call(varpro, c(list(f = f, data = make.sh(data, mode, papply)),
                           nvar = nvar,
                           ntree = ntree, nodesize = nodesize,
                           max.rules.tree = max.rules.tree, max.tree = max.tree,
                           papply = papply, verbose = verbose, seed = seed,
                           dots[names(dots) %in% varpro.names]))
    xvar.wt <- o$xvar.wt
    data <- o$x[o$y.org == 1,, drop = FALSE]
    ##------------------------------------------------------------------
    ##
    ##
    ## filter variables
    ##
    ##
    ##------------------------------------------------------------------
    xvar.wt <- sort(xvar.wt, decreasing = TRUE)
    xvar.names <- names(xvar.wt)
    pt <- xvar.wt > tolerance
    if (sum(pt)  > 0) {
      ntop <- min(nvar, sum(pt), length(xvar.names))
      xvar.names <- xvar.names[1:ntop]
      xvar.wt <- xvar.wt[1:ntop]
    }
    else {
      xvar.names <- xvar.names[1]
      xvar.wt <- 1
    }
    ##------------------------------------------------------------------
    ##
    ##
    ## obtain the subsetted data
    ##
    ##
    ##------------------------------------------------------------------
    data <- data[, xvar.names, drop = FALSE]
  }
  else {
    ##------------------------------------------------------------------
    ##
    ## no filtering has been requested
    ##
    ##
    ##------------------------------------------------------------------
    data <- get.hotencode(data, papply)
    xvar.names <- colnames(data)
    xvar.wt <- rep(1, ncol(data))
  }
  ##------------------------------------------------------------------
  ##
  ##
  ## run regr+ with weighted mtry 
  ##
  ##
  ##------------------------------------------------------------------
  ## set nodesize: optimized for n and p
  nodesize <- set.nodesize(nrow(data), ncol(data), nodesize)
  ## use weighted mtry?  default = no
  use.weights <- !is.null(dots$use.weights)
  mv.dta <- data.frame(y = data, data)
  f <- randomForestSRC::get.mv.formula(xvar.names)
  o <- rfsrc(f, mv.dta,
             ntree = ntree,
             nodesize = nodesize,
             xvar.wt = (if (use.weights) xvar.wt else NULL),
             perf.type = "none")
  rm(mv.dta)
  ##------------------------------------------------------------------
  ##
  ##
  ## call varpro.strength and extract necessary information
  ##
  ##
  ##------------------------------------------------------------------
  oo <- varpro.strength(o, membership = TRUE, max.rules.tree = max.rules.tree, max.tree = max.tree)
  ## membership lists
  oobMembership <- oo$oobMembership
  compMembership <- oo$compMembership
  ## identify useful rules and variables at play
  xreleaseId <- oo$strengthArray$xReleaseID
  keep.rules <- which(oo$strengthArray$oobCT > 0 & oo$strengthArray$compCT > 0)
  keep.xvar <- xvar.names[sort(unique(xreleaseId))]
  ## standardize x
  x <- scale(data, center = FALSE)
  ## used to store the new importance values
  results <- oo$strengthArray[, 1:5, drop = FALSE]
  colnames(results) <- c("tree", "branch", "variable", "n.oob", "imp")
  results$imp <- NA
  ##------------------------------------------------------------------
  ##
  ##
  ## obtain the "X" importance values
  ## uses the default (or user specified) entropy function
  ##
  ##
  ##------------------------------------------------------------------
  if (length(keep.rules) > 0) {
    imp <- unlist(papply(keep.rules, function(i) {
      xO <- x[oobMembership[[i]],, drop = FALSE]
      xC <- x[compMembership[[i]],, drop = FALSE]
      entropy(xC, xO)
    }))
  }
  ##------------------------------------------------------------------
  ##
  ##
  ## package the results up as a varpro object
  ##
  ##
  ##------------------------------------------------------------------
  results$imp[keep.rules] <- imp
  rO <- list()
  rO$results <- results
  rO$x <- data
  rO$y <- NULL
  rO$y.org <- NULL
  rO$xvar.names <- xvar.names
  rO$xvar.wt <- xvar.wt
  rO$max.rules.tree <- max.rules.tree
  rO$max.tree <- max.tree
  rO$family <- "unsupv"
  class(rO) <- "varpro"
  rO
}
