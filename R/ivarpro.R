ivarpro <- function(object,
                    cut = seq(.05, 1, length=21),
                    nmin = 20, nmax = 150,
                    y.external = NULL,
                    noise.na = TRUE,
                    papply = mclapply,
                    max.rules.tree = 150,
                    max.tree = 150)
{
  ## allows both varpro and rfsrc object
  if (!inherits(object, "varpro")) {
    if (sum(inherits(object, c("rfsrc", "grow"), TRUE) == c(1, 2)) != 2) {
      stop("This function only works for objects of class 'varpro' or `(rfsrc, grow)'")
    }
    ## this is a random forest object
    ## !! x cannot contain factors !!
    else {
      y <- data.matrix(object$predicted.oob)
      xvar.names <- object$xvar.names
      x <- object$xvar
      if (any(sapply(x, is.factor))) {
        stop("factors not allowed in x-features ... consider using a varpro object instead of a forest object")
      }
    }
  }
  ## this is a varpro object
  else {
    max.rules.tree <- object$max.rules.tree
    max.tree <- object$max.tree
    y <- data.matrix(object$rf$predicted.oob)
    xvar.names <- object$xvar.names
    x <- object$x[, xvar.names]
  }
  ## overwrite y if y.external is provided
  if (!is.null(y.external)) {
    y <- data.matrix(y.external)
  }
  ## final check on nmax
  nmax <- max(nmin, min(nmax, ncol(x) / 10))
  ## call varpro strength and pull relevant information
  o <- get.varpro.strength(object, membership = TRUE,
            max.rules.tree = max.rules.tree, max.tree = max.tree)
  keep.rules <- which(o$strengthArray$oobCT > 0 & o$strengthArray$compCT > 0)
  oobMembership <- o$oobMembership
  compMembership <- o$compMembership
  xreleaseId <- o$strengthArray$xReleaseID
  xreleaseIdUnq <- sort(unique(xreleaseId))
  results <- o$results
  ## y is the OOB estimator - keep in mind that y can be multivariate
  if (ncol(y) == 1 | (object$family == "class" & ncol(y) == 2)) {
    y <- y[, 1]
  }
  ## build up the results data frame with importance values and other rule information
  rO <- data.frame(do.call(rbind, papply(keep.rules, function(i) {
    xO <- x[oobMembership[[i]],, drop=FALSE]
    xC <- x[compMembership[[i]],, drop=FALSE]
    if (is.matrix(y)) {
      yO <- y[oobMembership[[i]],,drop=FALSE]
      yC <- y[compMembership[[i]],,drop=FALSE]
    }
    else {
      yO <- y[oobMembership[[i]]]
      yC <- y[compMembership[[i]]]
    }
    c(
      tree=results[i,"tree"],
      branch=results[i,"branch"],
      variable=xreleaseId[i],
      n.oob=length(oobMembership[[i]]),
      imp=cs.local.importance(yO,yC,xO,xC,idx=xreleaseId[i],cut=cut,noise.na=noise.na,nmin=nmin,nmax=nmax)
      )
  })))
  ## now split the data frame into cases and form the case-specific importance
  csO <- list()
  csO$results <- rO
  csO$xvar.names <- xvar.names
  csO$oobMembership <- oobMembership[keep.rules]
  csO$n <- nrow(x)
  if (is.matrix(y)) {
    lapply(1:ncol(y), function(j) {
      csO$results <- rO[, c((1:4), 4+j)]
      csimp.varpro.workhorse(csO, papply, noise.na)
    })
  }
  else {
    csimp.varpro.workhorse(csO, papply, noise.na)
  }
}
##############################################################
##
## utilities for calculating case-specific importance
##
##############################################################
csimp.varpro.workhorse <- function(o, papply = mclapply, noise.na) {
  xn <- o$xvar.names
  data.frame(do.call(rbind, papply(1:o$n, function(i) {
    pt <- sapply(o$oobMembership, function(l) {is.element(i, l)})
    if (noise.na) {
      imp <- rep(NA, length(xn))
    }
    else {
      imp <- rep(0, length(xn))
    }
    names(imp) <- xn
    if (sum(pt) > 0) {
      df <- o$results[which(pt),,drop=FALSE]
      v <- lapply(split(df, df$variable), function(d) {mean(d$imp, na.rm=TRUE)})
      xidx <- as.numeric(names(v))
      imp[xn[xidx]] <- unlist(v)      
    }
    imp
  })))
}
cs.local.importance <- function(yO, yC, xO, xC, idx, cut, noise.na, nmin, nmax) {
  if (!is.matrix(yC)) {
    grad.est(yO, yC, xO[,idx], xC[,idx], cut, noise.na, nmin, nmax)
  }
  else {
    sapply(1:ncol(yC), function(j) {
      grad.est(yO[,j], yC[,j], xO[,idx], xC[,idx], cut, noise.na, nmin, nmax)
    })
  }
}
grad.est <- function(yO, yC, xO, xC, cut, noise.na, nmin = 10, nmax = 20) {
  mn <- mean(xO, na.rm=TRUE)
  x <- c(xO, xC) - mn
  y <- c(yO, yC)
  grp <- c(rep(1, length(xO)), rep(2, length(xC)))
  sdx <- sd(x, na.rm = TRUE)
  maX(do.call(rbind, lapply(cut, function(scl) {
    pt <- abs(x) <= (sdx * scl)
    if (sum(pt) >= nmin) {
      J <- min(sum(pt), nmax)
      pt <- which(pt)[order(abs(x[pt]))[1:J]]
      x <- scale(x[pt], center = FALSE)
      y <- y[pt]
      o.lm <- tryCatch({suppressWarnings(lm(y~., data.frame(y=y, x=x)))}, error = function(ex) {NULL})
      if (is.null(o.lm)) {
        if (noise.na) c(J, NA, NA) else c(J, 0, .Machine$double.xmax)
      }
      else {
        influence <- x ^ 2 / sum(x ^ 2)
        rsq <- rsq.loo(o.lm$residuals, influence, J)
        c(J, abs(o.lm$coef["x"]), rsq)
      }
    }
    else {
      if (noise.na) c(sum(pt), NA, NA) else c(sum(pt), 0, .Machine$double.xmax)
    }
  })))
}
rsq.loo <- function(r, influence, J, K = 5) {
  hii <- 1 / J + influence
  mean((r / (1 - K * hii) )^2, na.rm = TRUE)
}
maX <- function(df) {
  if (all(is.na(df[, 2]))) {
    NA
  }
  else {
    df[, 2][which.min(df[, 3])]
  }
}
