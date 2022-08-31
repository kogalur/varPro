cv.varpro <- function(f, data, ntree = 300,
                   zcut = seq(0.1, 2, length = 50),
                   split.weight = TRUE,
                   nodesize = 10, max.rules.tree = 150, max.tree = min(150, ntree),
                   papply = mclapply, verbose = FALSE, seed = NULL,
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
  ## varpro call
  ##
  ##--------------------------------------------------------------
  o <- do.call("varpro", c(list(f = f, data = data, split.weight = split.weight,
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
  ## extract original yvalue names
  ##
  ##--------------------------------------------------------------
  yvar.names <- rfsrc(f, data, nodedepth = 0, ntree = 1, splitrule = "random")$yvar.names
  ##--------------------------------------------------------------
  ##
  ## select zcut using out-of-sample performance
  ##
  ##--------------------------------------------------------------
  ## use same inbag/oob members to reduce MC error
  samp <- randomForestSRC:::make.sample(ntree, nrow(data))
  ## loop over zcut sequence and acquire OOB error rate
  err <- sapply(zcut, function(zz) {
    pt <- imp >= zz
    if (sum(pt) > 0) {
      err.zz <- randomForestSRC::get.mv.error(rfsrc(f, data[, c(yvar.names, xvar.names[pt])],
                                  nodesize = nodesize, ntree = ntree,
                                  bootstrap = "by.user", samp = samp), standardize = TRUE)
    }     
    else {
      err.zz <- NA 
    }
    if (verbose) {
      cat("zcut value", zz, "error", err.zz, "\n")
    }
    err.zz
  })
  ##--------------------------------------------------------------
  ##
  ## return the importace values after filtering using optimal zcut
  ##
  ##--------------------------------------------------------------
  if (!all(is.na(err))) {
    zcut.opt <- zcut[which.min(err)]
    if (verbose) {
      cat("optimal cutoff value", zcut.opt, "\n")
    }
    v <- v[imp >= zcut.opt,, drop = FALSE]
  }
  ## everything is now selected
  v$selected <- 1
  v$zcenter <- NULL
  v
}
