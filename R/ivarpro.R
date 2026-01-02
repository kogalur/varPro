## ============================================================
## iVarPro (case-specific variable importance) with cut-ladder path
## v2.0
##  - Always computes and stores a rule-level "cut.max ladder" gradient path from ONE call.
##  - Aggregation uses a single rule-centric workhorse (no legacy/alternate aggregators).
## Notes:
##  - Main return value matches the original ivarpro contract (case x variable matrix/data.frame).
##  - Ladder/path info is attached as attribute "ivarpro.path".
## ============================================================
ivarpro <- function(object,
                    adaptive = TRUE,
                    cut = NULL,
                    cut.max = 1,
                    ncut = 51,
                    nmin = 20, nmax = 150,
                    y.external = NULL,
                    noise.na = TRUE,
                    papply = mclapply,
                    max.rules.tree = NULL,
                    max.tree = NULL,
                    use.loo = TRUE,
                    use.abs = FALSE,
                    path.store.membership = TRUE,
                    keep.data = TRUE) {
  ## ------------------------------------------------------------
  ## allows both varpro and rfsrc object
  ## ------------------------------------------------------------
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
    ## get.varpro.strength does not work nicely with survival
    y <- data.matrix(object$predicted.oob)
    object$yvar <- as.numeric(y)
    ## !! x cannot contain factors !!
    xvar.names <- object$xvar.names
    x <- object$xvar
    if (any(sapply(x, is.factor))) {
      stop("factors not allowed in x-features ... consider using a varpro object instead of a forest object")
    }
  } else {
    ## this is a varpro object
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
  keep.rules     <- which(o$strengthArray$oobCT > 0 & o$strengthArray$compCT > 0)
  oobMembership  <- o$oobMembership
  compMembership <- o$compMembership
  xreleaseId     <- o$strengthArray$xReleaseID
  results        <- o$results
  ## y is the OOB estimator - keep in mind that y can be multivariate
  if (ncol(y) == 1 || (object$family == "class" && ncol(y) == 2)) {
    y <- y[, 1]
  }
  ## ladder bookkeeping (exclude edges: cut[1] and cut[end])
  nladder <- max(0, length(cut) - 2)
  cut.ladder <- if (nladder > 0) cut[2:(length(cut) - 1)] else numeric(0)
  ## ------------------------------------------------------------
  ## rule-level gradient estimation (always returns full + ladder)
  ## ------------------------------------------------------------
  ruleMat <- do.call(rbind, papply(keep.rules, function(i) {
    xO <- x[oobMembership[[i]], , drop = FALSE]
    xC <- x[compMembership[[i]], , drop = FALSE]
    if (is.matrix(y)) {
      yO <- y[oobMembership[[i]], , drop = FALSE]
      yC <- y[compMembership[[i]], , drop = FALSE]
    } else {
      yO <- y[oobMembership[[i]]]
      yC <- y[compMembership[[i]]]
    }
    imp <- cs.local.importance(
      yO, yC, xO, xC,
      idx      = xreleaseId[i],
      cut      = cut,
      noise.na = noise.na,
      nmin     = nmin,
      nmax     = nmax,
      use.loo  = use.loo,
      use.abs  = use.abs,
      return.path = TRUE
    )
    c(
      tree     = results[i, "tree"],
      branch   = results[i, "branch"],
      variable = xreleaseId[i],
      n.oob    = length(oobMembership[[i]]),
      imp
    )
  }))
  ## parse ruleMat into rule meta + main imp + ladder
  mresp <- if (is.matrix(y)) ncol(y) else 1L
  ## rule meta (always present)
  rO.meta <- data.frame(
    tree     = ruleMat[, 1],
    branch   = ruleMat[, 2],
    variable = ruleMat[, 3],
    n.oob    = ruleMat[, 4]
  )
  ## main importance columns (same as original output contract)
  if (mresp == 1L) {
    rO <- cbind(rO.meta, imp = ruleMat[, 5])
  } else {
    rO <- cbind(rO.meta, ruleMat[, 5:(4 + mresp), drop = FALSE])
    colnames(rO)[5:(4 + mresp)] <- paste0("imp.", seq_len(mresp))
  }
  ## ladder columns: response-major blocks of length nladder
  rule.ladder <- vector("list", mresp)
  if (nladder > 0) {
    for (j in seq_len(mresp)) {
      start <- 4 + mresp + (j - 1L) * nladder + 1L
      end   <- start + nladder - 1L
      rule.ladder[[j]] <- as.matrix(ruleMat[, start:end, drop = FALSE])
    }
  } else {
    for (j in seq_len(mresp)) rule.ladder[[j]] <- matrix(numeric(0), nrow(ruleMat), 0)
  }
  ## ------------------------------------------------------------
  ## case-specific aggregation (main output; unchanged)
  ## ------------------------------------------------------------
  csO <- list()
  csO$results       <- rO
  csO$xvar.names    <- xvar.names
  csO$oobMembership <- oobMembership[keep.rules]
  csO$n             <- nrow(x)
  if (is.matrix(y)) {
    out <- lapply(seq_len(ncol(y)), function(j) {
      csO$results <- rO[, c(1:4, 4 + j), drop = FALSE]
      ## ensure the importance column is always named "imp"
      names(csO$results)[5] <- "imp"
      csimp.varpro.workhorse(csO, noise.na = noise.na)
    })
    names(out) <- colnames(y)
    ## attach path attributes (one per response)
    for (j in seq_along(out)) {
      attr(out[[j]], "ivarpro.path") <- list(
        cut = cut,
        cut.ladder = cut.ladder,
        use.loo = use.loo,
        use.abs = use.abs,
        adaptive = adaptive,
        nmin = nmin,
        nmax = nmax,
        noise.na = noise.na,
        xvar.names = xvar.names,
        rule.variable = as.integer(rO$variable),
        oobMembership = if (isTRUE(path.store.membership)) csO$oobMembership else NULL,
        rule.imp.ladder = rule.ladder[[j]],
        ## optional metadata (useful for debugging)
        rule.tree = as.integer(rO$tree),
        rule.branch = as.integer(rO$branch),
        rule.n.oob = as.integer(rO$n.oob)
      )
    }
  } else {
    out <- csimp.varpro.workhorse(csO, noise.na = noise.na)
    ## attach path attributes
    attr(out, "ivarpro.path") <- list(
      cut = cut,
      cut.ladder = cut.ladder,
      use.loo = use.loo,
      use.abs = use.abs,
      adaptive = adaptive,
      nmin = nmin,
      nmax = nmax,
      noise.na = noise.na,
      xvar.names = xvar.names,
      rule.variable = as.integer(rO$variable),
      oobMembership = if (isTRUE(path.store.membership)) csO$oobMembership else NULL,
      rule.imp.ladder = rule.ladder[[1]],
      ## optional metadata (useful for debugging)
      rule.tree = as.integer(rO$tree),
      rule.branch = as.integer(rO$branch),
      rule.n.oob = as.integer(rO$n.oob)
    )
  }
  if (keep.data) {
    attr(out, "data") <- data.frame(x, y = y)
  }
  out
}
##############################################################
##
## utilities for calculating case-specific importance
##
##############################################################
## rule-centric aggregator
## Matches original semantics:
##  - variables absent for a case stay NA (noise.na=TRUE) or 0 (noise.na=FALSE)
##  - variables present but all-NA contributions yield NaN (as mean(numeric(0)) does)
csimp.varpro.workhorse <- function(o, noise.na = TRUE) {
  xn <- o$xvar.names
  n  <- o$n
  p  <- length(xn)
  ## safety
  if (n <= 0 || p <= 0) {
    return(data.frame(matrix(numeric(0), nrow = 0, ncol = 0)))
  }
  ## allocate accumulators
  sum_mat <- matrix(0, nrow = n, ncol = p)
  ## counts to decide presence and NA-removal
  count_total <- matrix(0L, nrow = n, ncol = p)
  if (isTRUE(noise.na)) {
    count_nonNA <- matrix(0L, nrow = n, ncol = p)
  } else {
    count_nonNA <- NULL
  }
  ## loop over rules (results rows aligned with oobMembership)
  R <- nrow(o$results)
  if (R > 0) {
    var_id <- as.integer(o$results$variable)
    imp    <- o$results$imp
    memb   <- o$oobMembership
    for (r in seq_len(R)) {
      j <- var_id[r]
      if (!is.finite(j) || j < 1L || j > p) next
      idx <- memb[[r]]
      if (!length(idx)) next
      ## mark presence (even if imp is NA)
      count_total[idx, j] <- count_total[idx, j] + 1L
      v <- imp[r]
      if (isTRUE(noise.na)) {
        if (is.finite(v)) {
          sum_mat[idx, j] <- sum_mat[idx, j] + v
          count_nonNA[idx, j] <- count_nonNA[idx, j] + 1L
        }
      } else {
        ## noise.na=FALSE: imp is assumed numeric (NA treated as 0 by grad.est)
        if (!is.finite(v)) v <- 0
        sum_mat[idx, j] <- sum_mat[idx, j] + v
      }
    }
  }
  if (isTRUE(noise.na)) {
    ## mean with na.rm=TRUE, but only for present cells; absent remain NA
    sum_mat <- sum_mat / count_nonNA  ## yields NaN where count_nonNA==0
    sum_mat[count_total == 0L] <- NA_real_
  } else {
    ## mean including zeros; absent stay 0
    pos <- count_total > 0L
    sum_mat[pos] <- sum_mat[pos] / count_total[pos]
    ## count_total==0 stays 0
  }
  colnames(sum_mat) <- xn
  data.frame(sum_mat, check.names = FALSE)
}
cs.local.importance <- function(yO, yC, xO, xC, idx,
                                cut, noise.na, nmin, nmax,
                                use.loo = TRUE, use.abs = FALSE,
                                return.path = FALSE) {
  if (!is.matrix(yC)) {
    grad.est(yO, yC, xO[, idx], xC[, idx],
             cut, noise.na, nmin, nmax, use.loo, use.abs,
             return.path = return.path)
  } else {
    m <- ncol(yC)
    if (!isTRUE(return.path)) {
      ## one scalar per response
      sapply(seq_len(m), function(j) {
        grad.est(yO[, j], yC[, j],
                 xO[, idx], xC[, idx],
                 cut, noise.na, nmin, nmax, use.loo, use.abs,
                 return.path = FALSE)
      })
    } else {
      ## path mode: return main + ladder for each response, response-major
      tmp <- lapply(seq_len(m), function(j) {
        grad.est(yO[, j], yC[, j],
                 xO[, idx], xC[, idx],
                 cut, noise.na, nmin, nmax, use.loo, use.abs,
                 return.path = TRUE)
      })
      ## tmp is a list of length m, each element length (1 + nladder)
      main   <- sapply(tmp, function(z) z[1])
      ladder <- do.call(cbind, lapply(tmp, function(z) z[-1]))  ## nladder x m (or 0 x m)
      c(main, c(ladder))  ## response-major because c(ladder) stacks columns
    }
  }
}
grad.est <- function(yO, yC, xO, xC,
                     cut, noise.na,
                     nmin = 10, nmax = 20,
                     use.loo = TRUE, use.abs = FALSE,
                     return.path = FALSE) {
  ## ladder length (exclude edges: cut[1], cut[end])
  nladder <- max(0, length(cut) - 2)
  ## combine OOB and complement
  x_all <- c(xO, xC)
  y_all <- c(yO, yC)
  ok <- is.finite(x_all) & is.finite(y_all)
  x_all <- x_all[ok]
  y_all <- y_all[ok]
  if (length(x_all) < nmin) {
    out0 <- if (noise.na) NA_real_ else 0
    if (isTRUE(return.path)) {
      return(c(out0, rep(out0, nladder)))
    } else {
      return(out0)
    }
  }
  ux <- sort(unique(x_all))
  ## --- 0/1 (one-hot) branch ---------------------------------
  if (length(ux) == 2 && all(ux %in% c(0, 1))) {
    n0 <- sum(x_all == 0)
    n1 <- sum(x_all == 1)
    if (n0 == 0 || n1 == 0 || (n0 + n1) < nmin) {
      out0 <- if (noise.na) NA_real_ else 0
      if (isTRUE(return.path)) {
        return(c(out0, rep(out0, nladder)))
      } else {
        return(out0)
      }
    }
    ## robust subsampling that always keeps both levels (when both exist)
    x_use <- x_all
    y_use <- y_all
    if ((n0 + n1) > nmax) {
      n_use <- min(nmax, n0 + n1)
      ## initial proportional split
      J1 <- round(n_use * n1 / (n0 + n1))
      J1 <- max(1L, min(J1, n1, n_use - 1L))
      J0 <- n_use - J1
      J0 <- max(1L, min(J0, n0, n_use - 1L))
      ## adjust if caps caused mismatch
      if ((J0 + J1) < n_use) {
        rem <- n_use - (J0 + J1)
        cap1 <- n1 - J1
        cap0 <- n0 - J0
        add1 <- min(rem, max(0L, cap1))
        J1 <- J1 + add1
        rem <- rem - add1
        add0 <- min(rem, max(0L, cap0))
        J0 <- J0 + add0
      } else if ((J0 + J1) > n_use) {
        over <- (J0 + J1) - n_use
        drop1 <- min(over, max(0L, J1 - 1L))
        J1 <- J1 - drop1
        over <- over - drop1
        drop0 <- min(over, max(0L, J0 - 1L))
        J0 <- J0 - drop0
      }
      ## final safety
      J0 <- max(1L, min(J0, n0))
      J1 <- max(1L, min(J1, n1))
      if ((J0 + J1) > n_use) {
        if (J0 > J1 && J0 > 1L) J0 <- J0 - 1L else if (J1 > 1L) J1 <- J1 - 1L
      }
      if ((J0 + J1) < n_use) {
        if (J1 < n1) J1 <- J1 + 1L else if (J0 < n0) J0 <- J0 + 1L
      }
      idx1 <- sample(which(x_all == 1), J1)
      idx0 <- sample(which(x_all == 0), J0)
      idx  <- c(idx0, idx1)
      x_use <- x_all[idx]
      y_use <- y_all[idx]
    }
    z <- scale(x_use, center = TRUE, scale = TRUE)
    o.lm <- tryCatch(
      suppressWarnings(lm(y_use ~ z)),
      error = function(e) NULL
    )
    if (is.null(o.lm)) {
      out0 <- if (noise.na) NA_real_ else 0
      if (isTRUE(return.path)) {
        return(c(out0, rep(out0, nladder)))
      } else {
        return(out0)
      }
    } else {
      g <- coef(o.lm)[["z"]]
      if (use.abs) g <- abs(g)
      if (isTRUE(return.path)) {
        return(c(g, rep(g, nladder)))
      } else {
        return(g)
      }
    }
  }
  ## --- continuous branch ------------------------------------
  mn  <- mean(xO, na.rm = TRUE)
  if (!is.finite(mn)) {
    out0 <- if (noise.na) NA_real_ else 0
    if (isTRUE(return.path)) {
      return(c(out0, rep(out0, nladder)))
    } else {
      return(out0)
    }
  }
  x <- c(xO, xC) - mn
  y <- c(yO, yC)
  ok2 <- is.finite(x) & is.finite(y)
  x <- x[ok2]
  y <- y[ok2]
  if (length(x) < nmin) {
    out0 <- if (noise.na) NA_real_ else 0
    if (isTRUE(return.path)) {
      return(c(out0, rep(out0, nladder)))
    } else {
      return(out0)
    }
  }
  sdx <- sd(x, na.rm = TRUE)
  if (!is.finite(sdx) || sdx == 0) {
    out0 <- if (noise.na) NA_real_ else 0
    if (isTRUE(return.path)) {
      return(c(out0, rep(out0, nladder)))
    } else {
      return(out0)
    }
  }
  rows <- lapply(cut, function(scl) {
    pt <- abs(x) <= (sdx * scl)
    ## pt is logical without NA because x is finite
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
        g <- coef(o.lm)[["xx"]]
        if (use.abs) g <- abs(g)
        c(J, g, score)
      }
    } else {
      if (noise.na) c(n_pt, NA_real_, NA_real_) else c(n_pt, 0, .Machine$double.xmax)
    }
  })
  df <- do.call(rbind, rows)
  if (!isTRUE(return.path)) {
    maX(df, use.loo = use.loo)
  } else {
    best_prefix <- maX.prefix(df, use.loo = use.loo)
    full <- best_prefix[length(cut)]
    ladder <- if (nladder > 0) best_prefix[2:(length(cut) - 1)] else numeric(0)
    c(full, ladder)
  }
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
## best gradient selected along prefixes of the cut-grid
## returns a vector best[t] = maX(df[1:t,], use.loo)
maX.prefix <- function(df, use.loo = TRUE) {
  J    <- df[, 1]
  grad <- df[, 2]
  n    <- nrow(df)
  best <- rep(NA_real_, n)
  if (use.loo) {
    err <- df[, 3]
    have <- FALSE
    best_err <- Inf
    best_grad <- NA_real_
    for (t in seq_len(n)) {
      if (is.finite(grad[t]) && is.finite(err[t])) {
        if (!have || err[t] < best_err) {
          have <- TRUE
          best_err <- err[t]
          best_grad <- grad[t]
        }
      }
      best[t] <- if (have) best_grad else NA_real_
    }
  } else {
    have <- FALSE
    best_J <- -Inf
    best_grad <- NA_real_
    for (t in seq_len(n)) {
      if (is.finite(grad[t]) && is.finite(J[t]) && J[t] > 0) {
        if (!have || J[t] > best_J) {
          have <- TRUE
          best_J <- J[t]
          best_grad <- grad[t]
        }
        ## ties keep first occurrence (do nothing)
      }
      best[t] <- if (have) best_grad else NA_real_
    }
  }
  best
}
