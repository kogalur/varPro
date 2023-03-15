####################################################################
##
##
## entropy functions
##
##
####################################################################
entropy.custom <- function(xC, xO, alpha) {
  imp <- mean(abs(apply(xC, 2, sd, na.rm = TRUE) - apply(xO, 2, sd, na.rm = TRUE)))
  x <- data.matrix(rbind(xO, xC))
  pc.o <- tryCatch({suppressWarnings(pcsel(x[, 1], x[, -1],
                          alpha = alpha))}, error = function(ex){NULL})
  list(imp = imp, select = if (!is.null(pc.o)) pc.o$vars else NULL)
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
entropy.default <- function(xC, xO, alpha) {
  imp <- mean(abs(apply(xC, 2, sd, na.rm = TRUE) - apply(xO, 2, sd, na.rm = TRUE)))
  x <- data.matrix(rbind(xO, xC))
  pc.o <- tryCatch({suppressWarnings(pcsel(x[, 1], x[, -1],
               alpha = alpha))}, error=function(ex){NULL})
  list(imp = imp, partial = if (!is.null(pc.o)) pc.o$partial else NULL)
}
entropy.default.importance <- function(entropy.imp, xvar.names) {
  do.call(rbind, lapply(entropy.imp, function(oo) {
    pv <- unlist(oo)
    if (length(pv) > 0) {
      mn <- rep(0, length(xvar.names))
      names(mn) <- xvar.names
      mn[sort(unique(names(pv)))] <- tapply(as.numeric(pv), names(pv), mean, na.rm = TRUE)
      mn
    }
    else {
      NULL
    }
  }))
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
pcsel <- function (y, x, ystand = TRUE, xstand = TRUE, alpha = 0.05, tol = 1e-6) {
  if (ystand) 
    y <- (y - mean(y, na.rm = TRUE)) / sd(y, na.rm = TRUE)
  x <- data.matrix(x)
  if (xstand) {
    x <- scale(x)
    attr(x, "scaled.scale") <- attr(x, "scaled.center") <- NULL
  }
  dm <- dim(x)
  n <- dm[1]
  p <- dm[2]
  ina <- 1:p
  xyIdx <- 1:2
  nu <- n - 1
  k <- 0
  r <- abs(colSums(x * y)) / nu
  crit <- pcsel.critical(n, k, alpha)
  sela <- which(r > crit)
  if (length(sela) == 0) {
    stop("error: no variables meet critical value\n")
  }
  r <- r[sela]
  sela <- sela[order(r)]
  R <- crossprod(cbind(x[, sela, drop = FALSE], y))/nu##hereafter data is subsetted
  n.tests <- p
  len <- length(sela)
  ina <- ina2 <- 1:len
  d <- len + 1##last coordinate is used for y
  xnms <- colnames(x)
  xnms.sela <- colnames(x[, sela, drop = FALSE])
  rall <- vector("list", len)##list for saving all partial correlations
  names(rall) <- xnms.sela
  while (k < len) {
    k <- k + 1
    crit <- pcsel.critical(n, k, alpha)
    tes <- 0
    n.tests[k + 1] <- 0
    for (i in ina[ina > 0]) {
      j <- 0
      r <- Inf
      sam <- setdiff(ina2[ina2 > 0], i)
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
      xyIdx <- c(i, d)
      corrMatrix <- R[xyIdx, xyIdx]
      while (j < NCOL(sam) & r > crit) {
        j <- j + 1
        tes <- tes + 1
        csIdx <- sam[, j]
        ## obtain partial correlation of (X[i], y) after adjusting for other variables
        ## partial correlation obtained using regression: see equation (27.50) from Kendall and Stuart II
        residCorrMatrix <- corrMatrix - R[xyIdx, csIdx] %*% solve(R[csIdx, csIdx], rbind(R[csIdx, xyIdx]))
        r11 <- residCorrMatrix[1, 1] + tol * (residCorrMatrix[1, 1] <= .Machine$double.xmin)##singularity
        r22 <- residCorrMatrix[2, 2] + tol * (residCorrMatrix[2, 2] <= .Machine$double.xmin)##solution!
        r <- abs(residCorrMatrix[1, 2]) / sqrt(r11 * r22)
        ## store all the partial correlation information
        rall[[i]][[length(rall[[i]])+1]] <- list(i=xnms[i], j=xnms[csIdx], r=r, crit=crit)
      }
      if (r < crit && r >= 0) {
        ina[i] <- 0
        ina2[i] <- 0
      }
    }
    n.tests[k + 1] <- n.tests[k + 1] + tes
    len <- sum(ina > 0)
  }
  ## selected variables
  vars <- names(sela[ina])
  ## partial correlations
  rallavg <- do.call(rbind, lapply(rall, function(rr) {
    rvec <- rep(0, p)
    names(rvec) <- xnms
    lapply(rr, function(rr2) {
      rvec[rr2$j] <<- rvec[rr2$j] + rr2$r
    })
    rvec / length(rr)
  }))
  rallavg[rallavg == 0] <- NA
  rallavg <- rowMeans(rallavg, na.rm = TRUE)
  rallavg[setdiff(names(rallavg), vars)] <- 0##we only keep PC for vars selected
  ## number of tests
  n.tests <- n.tests[n.tests > 0]
  names(n.tests) = paste("k=", 0:(length(n.tests) - 1), sep = "")
  ## return the goodies
  list(vars = vars, partial = rallavg, n.tests = n.tests)
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
      nodesize <- 20
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
