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
    if (object$family == "regr+" ||
        object$family == "class+" ||
        object$family == "mix+" ||
        object$family == "unsupv") {
      stop("This function does not handle multivariate or unsupervised forests")
    }
    y <- data.matrix(object$predicted.oob)
    object$yvar <- as.numeric(y)
    xvar.names <- object$xvar.names
    x <- object$xvar
    if (any(sapply(x, is.factor))) {
      stop("factors not allowed in x-features ... consider using a varpro object instead of a forest object")
    }
  } else {
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
  if (is.null(max.rules.tree)) max.rules.tree <- 150
  if (is.null(max.tree))       max.tree       <- 150
  o <- get.varpro.strength(object, membership = TRUE,
                           max.rules.tree = max.rules.tree,
                           max.tree       = max.tree)
  keep.rules     <- which(o$strengthArray$oobCT > 0 & o$strengthArray$compCT > 0)
  ## subset everything ONCE so rule index is 1..R
  oobMembership  <- o$oobMembership[keep.rules]
  compMembership <- o$compMembership[keep.rules]
  xreleaseId     <- as.integer(o$strengthArray$xReleaseID[keep.rules])
  results        <- o$results[keep.rules, , drop = FALSE]
  ## y is the OOB estimator - keep in mind that y can be multivariate
  if (ncol(y) == 1 || (object$family == "class" && ncol(y) == 2)) {
    y <- y[, 1]
  }
  ## ladder bookkeeping (exclude edges: cut[1] and cut[end])
  nladder <- max(0L, length(cut) - 2L)
  cut.ladder <- if (nladder > 0) cut[2:(length(cut) - 1)] else numeric(0)
  ## ------------------------------------------------------------
  ## rule-level gradient estimation (no mclapply; avoid copying full xO/xC)
  ## ------------------------------------------------------------
  mresp <- if (is.matrix(y)) ncol(y) else 1L
  R <- length(xreleaseId)
  n_imp_cols <- mresp * (1L + nladder)
  n_cols <- 4L + n_imp_cols
  ruleMat <- matrix(NA_real_, nrow = R, ncol = n_cols)
  x_is_df <- is.data.frame(x)
  y_is_mat <- is.matrix(y)
  for (r in seq_len(R)) {
    idxO <- oobMembership[[r]]
    idxC <- compMembership[[r]]
    var_id <- xreleaseId[r]
    ## extract ONLY the release coordinate (big speed win)
    if (x_is_df) {
      xO <- x[idxO, var_id, drop = TRUE]
      xC <- x[idxC, var_id, drop = TRUE]
    } else {
      xO <- x[idxO, var_id]
      xC <- x[idxC, var_id]
    }
    if (y_is_mat) {
      yO <- y[idxO, , drop = FALSE]
      yC <- y[idxC, , drop = FALSE]
    } else {
      yO <- y[idxO]
      yC <- y[idxC]
    }
    imp <- cs.local.importance(
      yO, yC, xO, xC,
      idx = NULL,
      cut = cut,
      noise.na = noise.na,
      nmin = nmin,
      nmax = nmax,
      use.loo = use.loo,
      use.abs = use.abs,
      return.path = TRUE
    )
    ruleMat[r, ] <- c(
      results[r, "tree"],
      results[r, "branch"],
      var_id,
      length(idxO),
      imp
    )
  }
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
  csO$oobMembership <- oobMembership
  csO$n             <- nrow(x)
  if (is.matrix(y)) {
    out <- lapply(seq_len(ncol(y)), function(j) {
      csO$results <- rO[, c(1:4, 4 + j), drop = FALSE]
      names(csO$results)[5] <- "imp"
      csimp.varpro.workhorse(csO, noise.na = noise.na)
    })
    names(out) <- colnames(y)
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
        rule.tree = as.integer(rO$tree),
        rule.branch = as.integer(rO$branch),
        rule.n.oob = as.integer(rO$n.oob)
      )
    }
  } else {
    out <- csimp.varpro.workhorse(csO, noise.na = noise.na)
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
## ------------------------------------------------------------
## helper: fast simple regression y ~ x with intercept
## returns slope (for x) and optional LOO MSE (same criterion as rsq.loo())
## ------------------------------------------------------------
.ivarpro_fast_lm1 <- function(x, y, use.loo = TRUE) {
  n <- length(x)
  if (n < 2L) return(list(slope = NA_real_, loo = NA_real_))
  xbar <- mean(x)
  ybar <- mean(y)
  xc <- x - xbar
  yc <- y - ybar
  Sxx <- sum(xc * xc)
  if (!is.finite(Sxx) || Sxx <= 0) {
    return(list(slope = NA_real_, loo = NA_real_))
  }
  slope <- sum(xc * yc) / Sxx
  if (!isTRUE(use.loo)) {
    return(list(slope = slope, loo = NA_real_))
  }
  intercept <- ybar - slope * xbar
  res <- y - (intercept + slope * x)
  ## hatvalues for simple regression with intercept:
  ## h_i = 1/n + (x_i - xbar)^2 / sum((x - xbar)^2)
  h <- 1 / n + (xc * xc) / Sxx
  ok <- is.finite(res) & is.finite(h) & ((1 - h) > 1e-8)
  loo <- if (any(ok)) mean((res[ok] / (1 - h[ok]))^2) else NA_real_
  list(slope = slope, loo = loo)
}
## ------------------------------------------------------------
## grad.est: fast closed-form version (drop-in signature)
## ------------------------------------------------------------
grad.est <- function(yO, yC, xO, xC,
                     cut, noise.na,
                     nmin = 10, nmax = 20,
                     use.loo = TRUE, use.abs = FALSE,
                     return.path = FALSE) {
  nladder <- max(0L, length(cut) - 2L)
  out0 <- if (isTRUE(noise.na)) NA_real_ else 0
  ## combine OOB and complement
  x_all <- c(xO, xC)
  y_all <- c(yO, yC)
  ok <- is.finite(x_all) & is.finite(y_all)
  x_all <- x_all[ok]
  y_all <- y_all[ok]
  if (length(x_all) < nmin) {
    if (isTRUE(return.path)) return(c(out0, rep(out0, nladder)))
    return(out0)
  }
  ux <- sort(unique(x_all))
  ## --- 0/1 (one-hot) branch ---------------------------------
  if (length(ux) == 2L && all(ux %in% c(0, 1))) {
    n0 <- sum(x_all == 0)
    n1 <- sum(x_all == 1)
    if (n0 == 0L || n1 == 0L || (n0 + n1) < nmin) {
      if (isTRUE(return.path)) return(c(out0, rep(out0, nladder)))
      return(out0)
    }
    x_use <- x_all
    y_use <- y_all
    ## robust subsampling that always keeps both levels (when both exist)
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
    ## lm(y ~ scale(x, center=TRUE, scale=TRUE)) coefficient is:
    ## coef(y ~ x) * sd_sample(x)
    y1 <- y_use[x_use == 1]
    y0 <- y_use[x_use == 0]
    if (!length(y0) || !length(y1)) {
      if (isTRUE(return.path)) return(c(out0, rep(out0, nladder)))
      return(out0)
    }
    b_x <- mean(y1) - mean(y0)
    xbar <- mean(x_use)
    sd_x <- sqrt(sum((x_use - xbar)^2) / (length(x_use) - 1L))
    g <- b_x * sd_x
    if (isTRUE(use.abs)) g <- abs(g)
    if (isTRUE(return.path)) return(c(g, rep(g, nladder)))
    return(g)
  }
  ## --- continuous branch ------------------------------------
  mn <- mean(xO, na.rm = TRUE)
  if (!is.finite(mn)) {
    if (isTRUE(return.path)) return(c(out0, rep(out0, nladder)))
    return(out0)
  }
  x <- c(xO, xC) - mn
  y <- c(yO, yC)
  ok2 <- is.finite(x) & is.finite(y)
  x <- x[ok2]
  y <- y[ok2]
  if (length(x) < nmin) {
    if (isTRUE(return.path)) return(c(out0, rep(out0, nladder)))
    return(out0)
  }
  sdx <- sd(x, na.rm = TRUE)
  if (!is.finite(sdx) || sdx == 0) {
    if (isTRUE(return.path)) return(c(out0, rep(out0, nladder)))
    return(out0)
  }
  ## Sort once by |x|. Each cut corresponds to a prefix of this ordering.
  absx <- abs(x)
  ord <- order(absx)
  absx <- absx[ord]
  x_ord <- x[ord]
  y_ord <- y[ord]
  thr <- sdx * cut
  k_vec <- findInterval(thr, absx)  # number of points with abs(x) <= thr[t]
  nc <- length(cut)
  best <- rep(out0, nc)
  if (isTRUE(use.loo)) {
    have <- FALSE
    best_err <- Inf
    best_grad <- out0
  } else {
    have <- FALSE
    best_J <- -Inf
    best_grad <- out0
  }
  ## cache stats when J doesn't change across neighboring cut values
  last_J <- NA_integer_
  last_g <- out0
  last_err <- NA_real_
  for (t in seq_len(nc)) {
    k <- k_vec[t]  # n_pt (points inside the threshold)
    Jval <- k      # matches original df[,1] semantics when k < nmin
    if (k < nmin) {
      if (isTRUE(noise.na)) {
        g <- NA_real_
        err <- NA_real_
      } else {
        g <- 0
        err <- if (isTRUE(use.loo)) .Machine$double.xmax else NA_real_
      }
    } else {
      J <- min(k, nmax)
      Jval <- J
      if (is.na(last_J) || J != last_J) {
        xsub <- x_ord[seq_len(J)]
        ysub <- y_ord[seq_len(J)]
        st <- .ivarpro_fast_lm1(xsub, ysub, use.loo = use.loo)
        ## original code regresses on xx = scale(xsub, center=FALSE),
        ## which is just xsub scaled by sqrt(sum(x^2)/(n-1)).
        s0 <- sqrt(sum(xsub * xsub) / (J - 1L))
        if (!is.finite(st$slope) || !is.finite(s0) || s0 <= 0) {
          if (isTRUE(noise.na)) {
            g <- NA_real_
            err <- NA_real_
          } else {
            g <- 0
            err <- if (isTRUE(use.loo)) .Machine$double.xmax else NA_real_
          }
        } else {
          g <- st$slope * s0
          if (isTRUE(use.abs)) g <- abs(g)
          err <- st$loo
          if (!isTRUE(use.loo)) err <- NA_real_
        }
        last_J <- J
        last_g <- g
        last_err <- err
      } else {
        g <- last_g
        err <- last_err
      }
    }
    ## prefix-best update (same as maX.prefix())
    if (isTRUE(use.loo)) {
      if (is.finite(g) && is.finite(err)) {
        if (!have || err < best_err) {
          have <- TRUE
          best_err <- err
          best_grad <- g
        }
      }
      best[t] <- if (have) best_grad else out0
    } else {
      if (is.finite(g) && is.finite(Jval) && Jval > 0) {
        if (!have || Jval > best_J) {
          have <- TRUE
          best_J <- Jval
          best_grad <- g
        }
      }
      best[t] <- if (have) best_grad else out0
    }
  }
  full <- best[nc]
  if (!isTRUE(return.path)) return(full)
  ladder <- if (nladder > 0L) best[2:(nc - 1L)] else numeric(0)
  c(full, ladder)
}
## ------------------------------------------------------------
## cs.local.importance: now supports xO/xC vectors when idx=NULL
## (backward compatible with matrix inputs when idx is supplied)
## ------------------------------------------------------------
cs.local.importance <- function(yO, yC, xO, xC, idx = NULL,
                                cut, noise.na, nmin, nmax,
                                use.loo = TRUE, use.abs = FALSE,
                                return.path = FALSE) {
  if (!is.null(idx)) {
    ## backward compat: old calling pattern passed xO/xC as matrices
    xO <- xO[, idx]
    xC <- xC[, idx]
  }
  if (!is.matrix(yC)) {
    grad.est(yO, yC, xO, xC,
             cut, noise.na, nmin, nmax, use.loo, use.abs,
             return.path = return.path)
  } else {
    m <- ncol(yC)
    if (!isTRUE(return.path)) {
      sapply(seq_len(m), function(j) {
        grad.est(yO[, j], yC[, j], xO, xC,
                 cut, noise.na, nmin, nmax, use.loo, use.abs,
                 return.path = FALSE)
      })
    } else {
      tmp <- lapply(seq_len(m), function(j) {
        grad.est(yO[, j], yC[, j], xO, xC,
                 cut, noise.na, nmin, nmax, use.loo, use.abs,
                 return.path = TRUE)
      })
      main   <- sapply(tmp, function(z) z[1])
      ladder <- do.call(cbind, lapply(tmp, function(z) z[-1]))  ## nladder x m
      c(main, c(ladder))  ## response-major (columns stacked)
    }
  }
}
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
