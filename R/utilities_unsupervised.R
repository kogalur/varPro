####################################################################
##
##
## default entropy functions
##
##
####################################################################
entropy.ssq <- function(xC, xO) {
  wss <- mean(apply(rbind(xO, xC), 2, sd, na.rm = TRUE))
  bss <- mean(apply(xC, 2, sd, na.rm = TRUE)) + mean(apply(xO, 2, sd, na.rm = TRUE))
  0.5 * bss / wss
}
entropy.default <- function(xC, xO, alpha = .025, beta = FALSE, ...) {
  imp <- entropy.ssq(xC, xO)
  dots <- list(...)
  list(imp = imp, membership = list(comp = dots$compMembership, oob = dots$oobMembership))
}
get.entropy.default <- function(entropy.values, xvar.names, ...) {
  entropy.values
}
####################################################################
##
##
## example of a custom entropy functions
##
##
####################################################################
entropy.custom <- function(xC, xO, alpha = .025, beta = FALSE, ...) {
  imp <- entropy.ssq(xC, xO)
  x <- data.matrix(rbind(xO, xC))
  o <- tryCatch({suppressWarnings(pcsel(x[, 1], x[, -1, drop = FALSE],
        alpha = alpha, beta = beta))}, error=function(ex){NULL})
  list(imp = imp,
       pcselreturn = if (!is.null(o)) switch(1+(beta), o$partial, o$beta) else NULL)
}
get.entropy.custom <- function(entropy.imp, xvar.names, nlegit = 25, ...) {
  do.call(rbind, lapply(entropy.imp, function(oo) {
    pv <- unlist(oo)
    if (length(pv) > 0) {
      mn <- rep(0, length(xvar.names))
      names(mn) <- xvar.names
      if (length(na.omit(as.numeric(pv))) >= nlegit) {
        mn[sort(unique(names(pv)))] <- tapply(as.numeric(pv), names(pv), mean, na.rm = TRUE)
      }
      mn
    }
    else {
      NULL
    }
  }))
}
####################################################################
##
##
## classification analysis for rule region versus complementary region
##
##
####################################################################
get.beta.entropy <- function(o,
                             papply=mclapply,
                             lasso=TRUE,
                             nfolds=10,
                             maxit=2500,
                             thresh=1e-3,
                             glm.thresh=10) {
  ## input value must be an unusupervised varpro object
  if (!inherits(o, "unsupv", TRUE)) {
    stop("this wrapper only applies to unsupervised varpro")
  }
  ## get topvars, filter x
  vmp <- get.vimp(o, pretty=FALSE)
  vmp <- vmp[vmp>0]
  if (length(vmp)==0) {
    return(NULL)
  }
  xvars <- names(vmp)
  x <- o$x[, xvars, drop=FALSE]
  ## parse the membership values to obtain beta for each variable
  beta <- lapply(xvars, function(releaseX) {
    if (sum(xvars != releaseX) > 0) {
      bO <- do.call(rbind, papply(o$entropy[[releaseX]], function(rule) {
        get.beta.workhorse(releaseX, rule, x,
          lasso=lasso, nfolds=nfolds, maxit=maxit, thresh=thresh, glm.thresh=glm.thresh)
      }))
      if (!is.null(bO)) {
        colMeans(bO, na.rm = TRUE)
      }
      else {
        bO
      }
    }
    else {
      NULL
    }
  })
  names(beta) <- xvars
  do.call(rbind, beta)
}      
get.beta.workhorse <- function(releaseX,
                               rule,
                               xorg,
                               parallel=FALSE,
                               lasso=TRUE,
                               nfolds=10,
                               maxit=2500,
                               thresh=1e-3,
                               glm.thresh=10) {
  ## build the x data
  xC <- xorg[rule[[1]],]
  xO <- xorg[rule[[2]],]
  x <- rbind(xC, xO)
  nC <- nrow(xC)
  nO <- nrow(xO)
  x <- x[, colnames(x)!=releaseX]
  x <- scale(x, center=FALSE)
  ## define the classifier outcome
  class <- factor(c(rep(0, nC), rep(1, nO)))
  ##
  xnms <- colnames(xorg)
  p <- length(xnms)
  ## failure returns NULL
  beta <- NULL
  ## glmnet - maybe slower but reliable
  if (lasso) {
    o.glmnet <- tryCatch(
                  {suppressWarnings(cv.glmnet(as.matrix(x), class, family="binomial",
                         nfolds=nfolds, parallel=parallel, maxit=maxit, thresh=thresh))},
                         error=function(ex){NULL})
    if (!is.null(o.glmnet)) {
      bhat <- abs(coef(o.glmnet)[-1,1])
      beta <- rep(0, p)
      names(beta) <- xnms
      beta[names(bhat)] <- bhat
    }
  }
  ## glm - could be faster but very unreliable
  else {
    o.glm <- tryCatch(
              {suppressWarnings(glm(class~., data.frame(class=class, x), family="binomial"))},
      error=function(ex){NULL})
    if (!is.null(o.glm)) {
      if (max(abs(summary(o.glm)$coef[-1,1]), na.rm=TRUE) < glm.thresh) {
        bhat <- abs(summary(o.glm)$coef[-1,3])
        beta <- rep(NA, p)
        names(beta) <- xnms
        beta[names(bhat)] <- bhat
      }
    }
  }
  beta
}
####################################################################
##
##
## cv performance metrics for matrix
##
##
####################################################################
## need a custom prediction function since predict.mlm* is hidden
predict.mlm <- function(o, x) {
  if (class(o)[1] == "mlm") {
    as.matrix(cbind(1, x[,rownames(coef(o))[-1],drop=FALSE])) %*% coef(o)
  }
  else {
    predict.lm(o, x)
  }
}
cv.matrix.performance <- function(x, K = 10, plot.it = TRUE,
                                  papply = mclapply, tol = 1e-10) {
  ## convert to real
  x <- data.frame(data.matrix(x))
  ## obtain variance of x features for standardization
  vx <- apply(x, 2, var, na.rm = TRUE)
  ## check for singular x variables: turn those into noise
  zeropt <- vx < tol
  if (sum(zeropt) > 0) {
    x[, zeropt] <- matrix(rnorm(nrow(x)*sum(zeropt)), ncol=sum(zeropt))
    vx[zeropt] <- 1
  }
  ## set the folds
  tst <- randomForestSRC:::cv.folds(nrow(x), min(nrow(x), K))
  ## loop over the folds to acquire mse
  mse <- do.call(rbind, papply(1:length(tst), function(k) {
    ## train/test splits
    xtrn <- x[setdiff(1:nrow(x), tst[[k]]),, drop = FALSE]
    xtst <- x[tst[[k]],, drop = FALSE]
    ## fit sequential multivariate regression models 
    sapply(1:(ncol(x)-1), function(j) {
      o <- tryCatch({suppressWarnings(
         lm(do.call(cbind, as.list(xtrn[,1:j,drop=FALSE]))~., xtrn[,-(1:j),drop=FALSE]))},
         error=function(ex){NULL})
      if (!is.null(o)) {
        mean(colMeans(cbind((xtst[,1:j]-predict.mlm(o, xtst)))^2, na.rm = TRUE) / vx[1:j])
      }
      else {
        NA
      }
    })
  }))
  ## plot it?
  if (plot.it) {
    xv <- 1:ncol(mse)
    avg <- colMeans(mse, na.rm = TRUE)
    sdev <- apply(mse, 2, sd, na.rm = TRUE) / sqrt(nrow(mse))
    ylim <- range(c(avg, avg-sdev, avg+sdev), na.rm = TRUE)
    plot(xv, avg, type = "l", lwd = 3, ylim = ylim,
         xlab = "number of variables", ylab = "std. mse")
    arrows(xv, avg-sdev, xv, avg+sdev, length=0.05, angle=90, code=3)
    lines(lowess(xv, avg), col = 2, lty = 2)
  }
  ## extract mean/se from cv results
  err <- cbind(colMeans(mse, na.rm = TRUE), apply(mse, 2, sd, na.rm = TRUE))
  colnames(err) <- c("mean", "se")
  rownames(err) <- colnames(x)[1:(ncol(x)-1)]
  ## obtain the cv solution
  idx <- which.min(err[, 1])
  if (length(idx) > 0) {
    serr <- mean(err[, 2], trim = .05, na.rm = TRUE)#trim the serr
    idxpt <- err[, 1] <= (err[idx, 1] + serr)
    idxpt[is.na(idxpt)] <- FALSE
    if (sum(idxpt) > 0) {
      idxc <- min(which(idxpt))
      idxl <- max(which(idxpt))
    }
    else {
      idxc <- idxl <- 1
    }
  }
  else {
    idx <- idxc <- idxl <- 1
  }
  ## return the goodies 
  list(err = err,
       vars = colnames(x)[1:idx],
       vars.conserve = colnames(x)[1:idxc],
       vars.liberal = colnames(x)[1:idxl])
}
cv.matrix <- cv.matrix.performance
####################################################################
##
## cv for unsupervised variable selection
##
####################################################################
cv.unsupv.varpro <- function(object, K = 10, plot.it = FALSE, tol = 1e-10) {
  ## input value must be a varpro object
  if (!inherits(object, "varpro", TRUE)) {
    stop("object must be a varpro object")
  }
  if (object$family != "unsupv") {
    stop("this wrapper only applies to unsupervised families")
  }
  ## obtain the cv solution 
  cv.matrix(object$x[, rownames(importance(object)), drop = FALSE], K = K,
            plot.it = plot.it, , tol = tol)
}
cv.unsupv <- cv.unsupv.varpro 
####################################################################
##
##
## residuals from generalized inverse regression
## used to obtain beta from partial coefficients
##
##
####################################################################
ginvResidual <- function (y, x, tol = sqrt(.Machine$double.eps)) {
  x <- as.matrix(cbind(1, x))
  xsvd <- svd(x)
  Positive <- xsvd$d > max(tol * xsvd$d[1L], 0)
  if (all(Positive)) {
    ginv <- xsvd$v %*% (1/xsvd$d * t(xsvd$u))
  }
  else if (!any(Positive)) {
    ginv <- array(0, dim(x)[2L:1L])
  }
  else {
    ginv <- xsvd$v[, Positive, drop = FALSE] %*% ((1/xsvd$d[Positive]) * 
       t(xsvd$u[, Positive, drop = FALSE]))
  }
  c(y - x %*% (ginv %*% y))
}
ginvMLS <- function (y, x, tol = sqrt(.Machine$double.eps)) {
  x <- as.matrix(cbind(1, x))
  xsvd <- svd(x)
  Positive <- xsvd$d > max(tol * xsvd$d[1L], 0)
  if (all(Positive)) {
    ginv <- xsvd$v %*% (1/xsvd$d * t(xsvd$u))
  }
  else if (!any(Positive)) {
    ginv <- array(0, dim(x)[2L:1L])
  }
  else {
    ginv <- xsvd$v[, Positive, drop = FALSE] %*% ((1/xsvd$d[Positive]) * 
       t(xsvd$u[, Positive, drop = FALSE]))
  }
  ## return the MLS coefficients (remove intercept)
  c(ginv %*% y)[-1] 
}
####################################################################
##
##
## variable selection using the PC-simple algorithm
##
##  Buhlmann P., Kalisch M. and Maathuis M. H. (2010). Variable s
##  selection in high-dimensional linear models: partially faithful
##  distributions and the PC-simple algorithm. Biometrika, 97(2): 261-278.
##
##
####################################################################
pcsel.critical <- function(n, k, alpha) {
  crit <- exp(2 * qt(1 - alpha/2, n - 3) / sqrt(n - k - 3))
  (crit - 1) / (crit + 1)
}
pcsel <- function (y, x, alpha = 0.025, beta = FALSE, tol = 1e-6) {
  ## process data, obtain r
  yorg <- y
  xorg <- data.matrix(x)
  y <- (y - mean(y, na.rm = TRUE)) / sd(y, na.rm = TRUE)
  x <- scaleM(data.matrix(x))
  xnms <- colnames(x)
  n <- nrow(x)
  p <- ncol(x)
  r <- abs(colSums(x * y)) / n
  ## filter data based on r
  sela <- which(r > pcsel.critical(n, 0, alpha))
  if (length(sela) == 0) {
    stop("error: no variables meet critical value\n")
  }
  xyIdx <- c(1, length(sela) + 1)##last coordinate used for y
  r <- r[sela]
  sela <- sela[order(r)]
  ##hereafter data is subsetted
  R <- crossprod(cbind(x[, sela, drop = FALSE], y)) / n
  ina <- 1:length(sela)
  ball <- rall <- vector("list", length(sela))
  names(ball) <- names(rall) <- xnms.sela <- names(sela)
  ## main loop
  k <- 0
  while (k < sum(ina > 0)) {
    k <- k + 1
    crit <- pcsel.critical(n, k, alpha)
    for (i in ina[ina > 0]) {
      j <- 0
      r <- Inf
      sam <- setdiff(ina[ina > 0], i)
      if (length(sam) > k) {
        sam <- combn(sam, k)
      }
      else if (length(sam) == k) {
        sam <- as.matrix(sam)
      }
      else {
        sam <- as.matrix(sam)
        r <- -Inf
      }
      xyIdx[1] <- i
      corrMatrix <- R[xyIdx, xyIdx]
      while (j < NCOL(sam) & r > crit) {
        j <- j + 1
        csIdx <- sam[, j]
        ## obtain partial correlation of (X[i], y) after adjusting for other variables
        ## be careful about division by zero
        residCorrMatrix <- corrMatrix - R[xyIdx, csIdx] %*% solve(R[csIdx, csIdx], rbind(R[csIdx, xyIdx]))
        r11 <- residCorrMatrix[1, 1] +
          tol * (residCorrMatrix[1, 1] <= .Machine$double.xmin)
        r22 <- residCorrMatrix[2, 2] +
          tol * (residCorrMatrix[2, 2] <= .Machine$double.xmin)
        r <- abs(residCorrMatrix[1, 2] / sqrt(r11 * r22))
        ## store absolute partial correlation information
        if (r > crit) {
          rall[[i]][[length(rall[[i]])+1]] <-
            list(i=xnms.sela[i], S=xnms.sela[csIdx], r=r, crit=crit)
          if (beta) {
            r1 <- ginvResidual(y, x[, xnms.sela[csIdx], drop = FALSE])
            r2 <- ginvResidual(x[, xnms.sela[i]], x[, xnms.sela[csIdx], drop = FALSE])
            bhat <- r * sd(r1, na.rm = TRUE) / sd(r2, na.rm = TRUE)
            ball[[i]][[length(ball[[i]])+1]] <-
              list(i=xnms.sela[i], S=xnms.sela[csIdx], beta = bhat)
          }
        }
      }
      if (r < crit && r >= 0) {
        ina[i] <- 0
      }
    }
  }
  ## process the output
  ## selected variables
  vars <- xnms.sela[ina]
  ##partial correlations
  rallavg <- do.call(rbind, lapply(rall, function(rr) {
    rvec <- rep(0, p)
    names(rvec) <- xnms
    lapply(rr, function(rr2) {
      rvec[rr2$S] <<- rvec[rr2$S] + rr2$r
    })
    rvec / length(rr)
  }))
  rallavg[rallavg == 0] <- NA
  rallavg <- rowMeans(rallavg, na.rm = TRUE)
  rallavg[setdiff(names(rallavg), vars)] <- 0##we only keep PC for vars selected
  ## beta coefficients
  bmls <- ballavg <- NULL
  if (beta) {
    ballavg <- do.call(rbind, lapply(ball, function(bb) {
      bvec <- rep(0, p)
      names(bvec) <- xnms
      lapply(bb, function(bb2) {
        bvec[bb2$S] <<- bvec[bb2$S] + bb2$beta
      })
      bvec / length(bb)
    }))
    ballavg[ballavg == 0] <- NA
    ballavg <- rowMeans(ballavg, na.rm = TRUE)
    ballavg[setdiff(names(ballavg), vars)] <- 0
    ## mls estimator
    bmls <- ginvMLS(yorg, xorg[, vars, drop = FALSE])
    names(bmls) <- vars
  }
  ## return the goodies
  list(vars = vars, partial = rallavg, beta = ballavg, betaorg = bmls)
}
####################################################################
##
##
## custom nodesize for unsupervised setting
## typically much larger than varpro
##
##
####################################################################
set.unsupervised.nodesize <- function(n, p, nodesize = NULL) {
  if (is.null(nodesize)) {
    if (n <= 300 & p > n) {
      nodesize <- 2
    }
    else if (n <= 300 & p <= n) {
      nodesize <- min(n / 4, 20)
    }
    else if (n > 300 & n <= 2000) {
      nodesize <- 40
    }
    else {
      nodesize <- n / 50
    }
  }
  nodesize
}
####################################################################
##
## custom scale matrix: avoids NA's due to columns being singular
##
####################################################################
scaleM <- function(x, center = TRUE, scale = TRUE) {
  x <- data.matrix(x)
  d <- do.call(cbind, lapply(1:ncol(x), function(j) {
    sj <- sd(x[, j], na.rm = TRUE)
    if (scale) {
      if (sj < .Machine$double.eps) {
        sj <- 1
      }
      if (center) {
        (x[, j] - mean(x[, j], na.rm = TRUE)) / sj
      }
      else {
        x[, j] / sj
      }
    }
    else {
      if (center) {
        (x[, j] - mean(x[, j], na.rm = TRUE))
      }
      else {
        x[, j]
      }
    }
  }))
  colnames(d) <- colnames(x)
  data.matrix(d)
}
