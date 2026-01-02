clusterpro <- function(data,
                       method = c("auto", "unsupv", "rnd"),
                       ntree = 100, nodesize = NULL,
                       max.rules.tree = 40, max.tree = 40,
                       papply = mclapply, verbose = FALSE, seed = NULL,
                       ...) {
  ## varpro call 
  dots <- list(...)
  o <- do.call("uvarpro", c(list(
                           data = data,
                           method = method,
                           ntree = ntree,
                           nodesize = nodesize,
                           max.rules.tree = max.rules.tree,
                           max.tree = max.tree,
                           papply = papply,
                           verbose = verbose,
                           seed = seed), dots))
  ## get topvars
  vmp <- get.vimp(o, pretty=FALSE)
  vmp <- vmp[vmp>0]
  xvars <- names(vmp)
  ## filter x and scale it
  x <- o$x[, xvars, drop=FALSE]
  ## set the sparsity parameter (should probably put this into a utility)
  sparse <- 2
  ## parse the entropy
  cO <- lapply(xvars, function(releaseX) {
    if (sum(xvars != releaseX) > 0) {
      keepX <- xvars[xvars != releaseX]
      dO <- do.call(rbind, papply(o$entropy[[releaseX]], function(rule) {
        wts <- get.beta.workhorse(releaseX, rule, x)
        if (!is.null(wts)) {
          wts <- wts ^ sparse
          wts <- wts / max(wts, na.rm=TRUE)
          wts[releaseX] <- 1##do not shrink the release variable to zero here (do this later)
          xOm.org <- colMeans(x[rule[[1]],, drop=FALSE], na.rm=TRUE)
          xCm.org <- colMeans(x[rule[[2]],, drop=FALSE], na.rm=TRUE)
          rbind(wts * xOm.org, wts * xCm.org)
        }
        else {
          NULL
        }
      }))
      if (!is.null(dO)) {
        dO <- data.frame(dO)
        colnames(dO) <- colnames(x)
        dO <- na.omit(dO)
        if (nrow(dO) == 0) {
          dO <- NULL
        }
        dO
      }
    }
    else {
      NULL
    }
  })
  ## return the goodies
  names(cO) <- xvars
  cO <- list(x=cO, importance=vmp)
  class(cO) <- "clusterpro"
  cO
}
