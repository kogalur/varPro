ivarpro <- function(object,
                    cut = NULL,
                    cut.max = 1,
                    ncut = 21,
                    nmin = 20, nmax = 150,
                    y.external = NULL,
                    noise.na = TRUE,
                    papply = mclapply,
                    max.rules.tree = NULL,
                    max.tree = NULL,
                    use.loo = TRUE,
                    adaptive = FALSE)
{

  ## allows both varpro and rfsrc object
  if (!inherits(object, "varpro")) {
    if (!(inherits(object, "rfsrc") && inherits(object, "grow"))) {
      stop("This function only works for objects of class 'varpro' or an 'rfsrc' grow object")
    }
    ## this is a random forest object
    ## we do not handle multivariate forests
    if (object$family == "regr+" ||
        object$family == "class+" ||
        object$family == "mix+" ||
        object$family == "unsupv") {
      stop("This function does not handle multivariate or unsupervised forests")
    }
    ## !! x cannot contain factors !!
    y <- data.matrix(object$predicted.oob)
    xvar.names <- object$xvar.names
    x <- object$xvar
    if (any(sapply(x, is.factor))) {
      stop("factors not allowed in x-features ... consider using a varpro object instead of a forest object")
    }
  }
  ## this is a varpro object
  else {
    if (is.null(max.rules.tree)) max.rules.tree <- object$max.rules.tree
    if (is.null(max.tree))       max.tree       <- object$max.tree
    y <- data.matrix(object$rf$predicted.oob)
    xvar.names <- object$xvar.names
    x <- object$x[, xvar.names]
  }

  ## overwrite y if y.external is provided
  if (!is.null(y.external)) {
    y.external <- data.matrix(y.external)
    if (nrow(y.external) != nrow(x)) {
      stop("y.external must have the same number of rows as the feature matrix x")
    }
    y <- y.external
  }

  ## construct cut grid (primary tuning via cut.max / adaptive)
  if (is.null(cut)) {
    if (adaptive) {
      n <- nrow(x)
      ## simple bandwidth-style rule: cut.max.data ~ 0.5 when n ~ 500
      cut.max.data <- min(cut.max, 1.7 * n^(-1/5))
      cut <- seq(0, cut.max.data, length.out = ncut)
    } else {
      cut <- seq(0, cut.max, length.out = ncut)
    }
  } else {
    cut <- sort(unique(cut))
  }

  ## final check on nmax (cap at 10% of sample size)
  nmax <- max(nmin, min(nmax, floor(0.1 * nrow(x))))

  ## ensure max.rules.tree / max.tree have values for rfsrc objects too
  if (is.null(max.rules.tree)) max.rules.tree <- 150
  if (is.null(max.tree))       max.tree       <- 150

  ## call varpro strength and pull relevant information
  o <- get.varpro.strength(object, membership = TRUE,
                           max.rules.tree = max.rules.tree,
                           max.tree       = max.tree)

  keep.rules    <- which(o$strengthArray$oobCT > 0 & o$strengthArray$compCT > 0)
  oobMembership <- o$oobMembership
  compMembership<- o$compMembership
  xreleaseId    <- o$strengthArray$xReleaseID
  results       <- o$results

  ## y is the OOB estimator - keep in mind that y can be multivariate
  if (ncol(y) == 1 || (object$family == "class" && ncol(y) == 2)) {
    y <- y[, 1]
  }

  ## build up the results data frame with importance values and other rule information
  rO <- data.frame(do.call(rbind, papply(keep.rules, function(i) {
    xO <- x[oobMembership[[i]], , drop = FALSE]
    xC <- x[compMembership[[i]], , drop = FALSE]
    if (is.matrix(y)) {
      yO <- y[oobMembership[[i]], , drop = FALSE]
      yC <- y[compMembership[[i]], , drop = FALSE]
    } else {
      yO <- y[oobMembership[[i]]]
      yC <- y[compMembership[[i]]]
    }
    c(
      tree     = results[i, "tree"],
      branch   = results[i, "branch"],
      variable = xreleaseId[i],
      n.oob    = length(oobMembership[[i]]),
      imp      = cs.local.importance(
                   yO, yC, xO, xC,
                   idx      = xreleaseId[i],
                   cut      = cut,
                   noise.na = noise.na,
                   nmin     = nmin,
                   nmax     = nmax,
                   use.loo  = use.loo
                 )
    )
  })))

  ## now split the data frame into cases and form the case-specific importance
  csO <- list()
  csO$results       <- rO
  csO$xvar.names    <- xvar.names
  csO$oobMembership <- oobMembership[keep.rules]
  csO$n             <- nrow(x)

  if (is.matrix(y)) {
    out <- lapply(1:ncol(y), function(j) {
      csO$results <- rO[, c(1:4, 4 + j)]
      ## ensure the importance column is always named "imp"
      names(csO$results)[5] <- "imp"
      csimp.varpro.workhorse(csO, papply, noise.na)
    })
    names(out) <- colnames(y)
    out
  } else {
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
    pt <- sapply(o$oobMembership, function(l) { is.element(i, l) })
    if (noise.na) {
      imp <- rep(NA, length(xn))
    } else {
      imp <- rep(0, length(xn))
    }
    names(imp) <- xn
    if (sum(pt) > 0) {
      df <- o$results[which(pt), , drop = FALSE]
      v  <- lapply(split(df, df$variable),
                   function(d) mean(d$imp, na.rm = TRUE))
      xidx <- as.numeric(names(v))
      imp[xn[xidx]] <- unlist(v)
    }
    imp
  })))
}


cs.local.importance <- function(yO, yC, xO, xC, idx,
                                cut, noise.na, nmin, nmax,
                                use.loo = TRUE) {
  if (!is.matrix(yC)) {
    grad.est(yO, yC, xO[, idx], xC[, idx],
             cut, noise.na, nmin, nmax, use.loo)
  } else {
    sapply(1:ncol(yC), function(j) {
      grad.est(yO[, j], yC[, j],
               xO[, idx], xC[, idx],
               cut, noise.na, nmin, nmax, use.loo)
    })
  }
}


grad.est <- function(yO, yC, xO, xC,
                     cut, noise.na,
                     nmin = 10, nmax = 20,
                     use.loo = TRUE) {

  ## combine OOB and complement
  x_all <- c(xO, xC)
  y_all <- c(yO, yC)

  ok <- is.finite(x_all) & is.finite(y_all)
  x_all <- x_all[ok]
  y_all <- y_all[ok]

  if (length(x_all) < nmin) {
    return(if (noise.na) NA_real_ else 0)
  }

  ux <- sort(unique(x_all))

  ## --- 0/1 (one-hot) branch ---------------------------------
  if (length(ux) == 2 && all(ux %in% c(0, 1))) {
    n0 <- sum(x_all == 0)
    n1 <- sum(x_all == 1)

    if (n0 == 0 || n1 == 0 || (n0 + n1) < nmin) {
      return(if (noise.na) NA_real_ else 0)
    }

    if (n0 + n1 > nmax) {
      J1   <- max(1, round(nmax * n1 / (n0 + n1)))
      J0   <- nmax - J1
      idx1 <- sample(which(x_all == 1), J1)
      idx0 <- sample(which(x_all == 0), J0)
      idx  <- c(idx0, idx1)
      x_use <- x_all[idx]
      y_use <- y_all[idx]
    } else {
      x_use <- x_all
      y_use <- y_all
    }

    z <- scale(x_use, center = TRUE, scale = TRUE)

    o.lm <- tryCatch(
      suppressWarnings(lm(y_use ~ z)),
      error = function(e) NULL
    )

    if (is.null(o.lm)) {
      return(if (noise.na) NA_real_ else 0)
    } else {
      return(abs(coef(o.lm)[["z"]]))
    }
  }

  ## --- continuous branch ------------------------------------
  mn  <- mean(xO, na.rm = TRUE)
  x   <- c(xO, xC) - mn
  y   <- c(yO, yC)
  sdx <- sd(x, na.rm = TRUE)

  if (!is.finite(sdx) || sdx == 0) {
    return(if (noise.na) NA_real_ else 0)
  }

  rows <- lapply(cut, function(scl) {
    pt   <- abs(x) <= (sdx * scl)
    n_pt <- sum(pt)

    if (n_pt >= nmin) {
      J   <- min(n_pt, nmax)
      idx <- which(pt)[order(abs(x[pt]))[1:J]]
      xx  <- scale(x[idx], center = FALSE)
      yy  <- y[idx]

      o.lm <- tryCatch(
        suppressWarnings(lm(yy ~ xx)),
        error = function(e) NULL
      )

      if (is.null(o.lm)) {
        if (noise.na) c(J, NA_real_, NA_real_) else c(J, 0, .Machine$double.xmax)
      } else {
        if (use.loo) {
          score <- rsq.loo(o.lm)
        } else {
          score <- NA_real_
        }
        c(J, abs(coef(o.lm)[["xx"]]), score)
      }
    } else {
      if (noise.na) c(n_pt, NA_real_, NA_real_) else c(n_pt, 0, .Machine$double.xmax)
    }
  })

  df <- do.call(rbind, rows)
  maX(df, use.loo = use.loo)
}


rsq.loo <- function(o.lm) {
  r <- resid(o.lm)
  h <- hatvalues(o.lm)
  ok <- is.finite(r) & is.finite(h) & (1 - h) > 1e-8
  if (!any(ok)) return(NA_real_)
  mean((r[ok] / (1 - h[ok]))^2, na.rm = TRUE)
}


maX <- function(df, use.loo = TRUE) {
  J    <- df[, 1]
  grad <- df[, 2]

  if (use.loo) {
    err  <- df[, 3]
    good <- which(is.finite(grad) & is.finite(err))
    if (!length(good)) return(NA_real_)
    k <- good[which.min(err[good])]
    grad[k]
  } else {
    good <- which(is.finite(grad) & is.finite(J) & J > 0)
    if (!length(good)) return(NA_real_)
    k <- good[which.max(J[good])]
    grad[k]
  }
}
