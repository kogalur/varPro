cv.varpro <- function(f, data, ntree = 150,
                      zcut = seq(0.1, 2, length = 50),
                      nblocks=10,
                      split.weight = TRUE,
                      nodesize = 10, nodesize.reduce = 10,
                      max.rules.tree = 150, max.tree = min(150, ntree),
                      papply = mclapply, verbose = FALSE, seed = NULL,
                      crps = FALSE,
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
  ## remove zcut values that lead to duplicated models
  ##
  ##--------------------------------------------------------------
  zcut.models <- do.call(rbind, lapply(zcut, function(zz) {
    1 * (imp >= zz)
  }))
  zcut <- zcut[!duplicated(zcut.models)]
  ##--------------------------------------------------------------
  ##
  ## extract original yvalue names
  ## re-define the original data in case there are missing values
  ##
  ##--------------------------------------------------------------
  stump <- rfsrc(f, data, nodedepth = 0, perf.type = "none", save.memory = TRUE, ntree = 1, splitrule = "random")
  n <- stump$n
  yvar.names <- stump$yvar.names
  data <- data.frame(stump$yvar, stump$xvar)
  colnames(data)[1:length(yvar.names)] <- yvar.names
  rm(stump)
  ##--------------------------------------------------------------
  ##
  ## select zcut using out-of-sample performance
  ##
  ##--------------------------------------------------------------
  ## use same inbag/oob members to reduce MC error
  samp <- randomForestSRC:::make.sample(ntree, n)
  ## set the seed
  seed <- get.seed(seed)
  ## loop over zcut sequence and acquire OOB error rate
  err <- do.call(rbind, lapply(zcut, function(zz) {
    pt <- imp >= zz
    if (sum(pt) > 0) {
      err.zz <- get.sderr(rfsrc(f, data[, c(yvar.names, xvar.names[pt])],
                                nodesize = nodesize, ntree = ntree, perf.type = "none",
                                bootstrap = "by.user", samp = samp, seed = seed),
                          nblocks = nblocks, crps = crps, papply = papply)
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
