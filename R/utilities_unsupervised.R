####################################################################
##
##
## entropy functions
##
##
####################################################################
entropy.ssq <- function(xC, xO) {
  mean(abs(apply(xC, 2, sd, na.rm = TRUE) - apply(xO, 2, sd, na.rm = TRUE)))
}
entropy.ssq <- function(xC, xO) {
  wss <- mean(apply(rbind(xO, xC), 2, sd, na.rm = TRUE))
  bss <- mean(apply(xC, 2, sd, na.rm = TRUE)) + mean(apply(xO, 2, sd, na.rm = TRUE))
  0.5 * bss / wss
}
entropy.default <- function(xC, xO, alpha = .025, beta = FALSE) {
  imp <- entropy.ssq(xC, xO)
  x <- data.matrix(rbind(xO, xC))
  o <- tryCatch({suppressWarnings(pcsel(x[, 1], x[, -1, drop = FALSE],
        alpha = alpha, beta = beta))}, error=function(ex){NULL})
  list(imp = imp,
       partial = if (!is.null(o)) switch(1 + beta, o$partial, o$beta) else NULL)
}
entropy.default.importance <- function(entropy.imp, xvar.names, nlegit = 25) {
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
entropy.custom <- function(xC, xO, alpha = .025) {
  imp <- entropy.ssq(xC, xO)
  x <- data.matrix(rbind(xO, xC))
  o <- tryCatch({suppressWarnings(pcsel(x[, 1], x[, -1, drop = FALSE],
                          alpha = alpha))}, error = function(ex){NULL})
  list(imp = imp, select = if (!is.null(o)) o$vars else NULL)
}
entropy.custom.importance <- function(entropy.imp, xvar.names, sort = FALSE) {
  entropy.imp <- do.call(rbind, lapply(entropy.imp, function(oo) {
      sv <- unlist(oo)
      if (length(sv) > 0) {
        frq <- rep(0, length(xvar.names))
        names(frq) <- xvar.names
        frq[sort(unique(sv))] <- tapply(sv, sv, length)
        frq
      }
      else {
        NULL
      }
  }))
  if (sort) {
    entropy.imp[order(rowSums(entropy.imp),decreasing=T), order(colSums(entropy.imp),decreasing=T)]
  }
  else {
    entropy.imp
  }
}
####################################################################
##
## performance metrics for unsupervised variable selection
##
####################################################################
entropy.fp.workhorse <- function(x) {
  if (ncol(x) == 1) {
    (mean(c(x)^2, na.rm = TRUE))^2
  }
  else {
    ## loop over columns forming the frame-potential metric
    sum(sapply(combn(1:ncol(x), 2, simplify = FALSE), function(j) {
      abs(mean((x[, j[1]] * x[, j[2]]), na.rm = TRUE))^2
    }))
  }
}
entropy.fp <- function(x, nvar = NULL) {
  ## restrict x to nvar columns
  if (!is.null(nvar)) {
    x <- x[, 1:min(nvar, ncol(x)), drop = FALSE]
  }
  ## standardize x
  x <- scale(x, center = TRUE, scale = TRUE)
  ## fp(x)
  fpx <- entropy.fp.workhorse(x)
  ## return fp for sequential models formed by columns
  sapply(1:ncol(x), function(j) {
    fpx - entropy.fp.workhorse(x[, 1:j, drop = FALSE])
  })
}
entropy.auc <- function(x, nvar = NULL) {
  ## restrict x to nvar columns
  if (!is.null(nvar)) {
    x <- x[, 1:min(nvar, ncol(x)), drop = FALSE]
  }
  ## standardize x
  x <- as.matrix(scale(x, center = TRUE, scale = TRUE))
  ## r(x), v(x)
  gols <- randomForestSRC:::ginverse(x) %*% x
  rx <- x - x %*% gols
  vx <- sum(c(x)^2)
  ## loop over columns forming the auc metric
  sapply(1:ncol(x), function(j) {
    xj <- x[, 1:j, drop = FALSE]
    golsj <- randomForestSRC:::ginverse(xj) %*% x
    rxj <- x - xj %*% golsj
    1 - sum(c(rx - rxj)^2) / vx
  })
}
get.matrix.performance <- function(x) {
  ## convert x to a data matrix
  x <- data.matrix(x)
  ## pull the metrics
  rO <- data.frame(rbind(fp = entropy.fp(x), auc = entropy.auc(x)))
  colnames(rO) <- colnames(x)
  rO
}
get.unsupervised.performance <- function(o) {
  ## input value must be a varpro object
  if (!inherits(o, "varpro", TRUE)) {
    stop("object must be a varpro object")
  }
  if (o$family != "unsupv") {
    stop("this wrapper only applies to unsupervised families")
  }
  ## pull the top variables, sort x accordingly
  topvars <- get.topvars(o)
  x <- o$x[, c(topvars, setdiff(colnames(o$x), topvars))]
  ## pull the metrics
  rO <- data.frame(rbind(fp = entropy.fp(x), auc = entropy.auc(x)))
  colnames(rO) <- colnames(x)
  rO
}
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
  y <- (y - mean(y, na.rm = TRUE)) / sd(y, na.rm = TRUE)
  x <- scale(data.matrix(x))
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
  ## abs partial correlations
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
  ## obtain abs beta?
  ballavg <- NULL
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
    ballavg[setdiff(names(ballavg), vars)] <- 0##we only keep PC for vars selected
  }
  ## return the goodies
  list(vars = vars, partial = rallavg, beta = ballavg)
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
## breiman shi-horvath legacy code
##
####################################################################
##  make SH data (modes 1 and 2)
make.sh <- function(dat, mode = 1, papply = mclapply) {
  ## extract sample size dimension
  nr <- dim(dat)[[1]]
  nc <- dim(dat)[[2]]
  if (nc == 0) {
    stop("can't make SH data ... not enough unique values\n")
  }
  ## coerce to data frame format
  if (!is.data.frame(dat)) {
    dat <- data.frame(dat)
  }
  ## mode 1
  if (mode == 1) {
    data.frame(classes = factor(c(rep(1, nr), rep(2, nr))),
      rbind(dat, data.frame(papply(dat, sample, replace = TRUE))))
  }
  ## mode 2
  else {
    data.frame(classes = factor(c(rep(1, nr), rep(2, nr))),
      rbind(dat, data.frame(papply(dat, function(x) {
        if (is.factor(x)) {
          resample(x, replace = TRUE)
        }
        else {
          runif(nr, min(x, na.rm = TRUE), max(x, na.rm = TRUE))
        }
      }))))
  }
}
