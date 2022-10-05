unsupv.varpro <- function(data, nvar = 5, cutoff = NULL,
                         method = c("sh1", "sh2"), cv = FALSE,
                         ntree = 150, nodesize = 10, nodesize.reduce = 10,
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
  ## if nvar >= p nothing to do (assumes default settings for cutoff and cv)
  if (nvar >= nrow(data) && is.null(cutoff) && !cv) {
    return(colnames(data))
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
  dots$nodesize.reduce <- nodesize.reduce
  f <- as.formula("classes ~ .")
  varpro.names <- c(get.varpro.names())
  cv.varpro.names <- c(varpro.names, "zcut", "blocks")
  ##------------------------------------------------------------------
  ##
  ##
  ## use varPro on artificially created two class data
  ## - works if there is only 1 variable left
  ## - there needs to be enough unique values always 
  ##
  ##
  ##------------------------------------------------------------------
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
    ## uncomment if you want (1) standard (2) conservative or (3) liberal
    ## bestvar <- rownames(o$imp)
    ## bestvar <- rownames(o$imp.conserve)
    ## bestvar <- rownames(o$imp.liberal)
    ## return all three options
    bestvar <- lapply(1:3, function(j) {rownames(o[[j]])})
    names(bestvar) <- names(o)[1:3]
    bestvar
  }
}
