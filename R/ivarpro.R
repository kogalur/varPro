## ============================================================
## iVarPro (case-specific variable importance)
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
                    save.data = TRUE,
                    save.model = TRUE,
                    scale = c("local","global","none")) {
  scale <- match.arg(scale)
  ## Validate the controls that determine the neighborhood search.
  flags <- list(adaptive = adaptive, noise.na = noise.na,
                use.loo = use.loo, use.abs = use.abs,
                path.store.membership = path.store.membership,
                save.data = save.data, save.model = save.model)
  for (nn in names(flags)) {
    z <- flags[[nn]]
    if (!is.logical(z) || length(z) != 1L || is.na(z)) {
      stop(nn, " must be TRUE or FALSE")
    }
  }
  counts <- list(nmin = nmin, nmax = nmax)
  if (is.null(cut)) counts$ncut <- ncut
  for (nn in names(counts)) {
    z <- counts[[nn]]
    lower <- if (nn == "ncut") 1L else 2L
    if (!is.numeric(z) || length(z) != 1L || !is.finite(z) ||
        z < lower || z != floor(z)) {
      stop(nn, " must be an integer of at least ", lower)
    }
  }
  if (is.null(cut)) {
    if (!is.numeric(cut.max) || length(cut.max) != 1L ||
        !is.finite(cut.max) || cut.max < 0) {
      stop("cut.max must be a finite, nonnegative number")
    }
  } else if (!is.numeric(cut) || !length(cut) ||
             any(!is.finite(cut)) || any(cut < 0)) {
    stop("cut must contain finite, nonnegative neighborhood sizes")
  }
  ## Resolve the rule-generating forest and its processed predictors.
  if (inherits(object, "varpro")) {
    if (is.null(max.rules.tree)) max.rules.tree <- object$max.rules.tree
    if (is.null(max.tree))       max.tree       <- object$max.tree
    forest <- object$rf
    xvar.names <- object$xvar.names
    x <- object$x[, xvar.names, drop = FALSE]
  } else {
    if (!(inherits(object, "rfsrc") && inherits(object, "grow"))) {
      stop("object must be a varpro object or an rfsrc grow object")
    }
    if (object$family %in% c("regr+", "class+", "mix+", "unsupv")) {
      stop("Direct rfsrc input must be a univariate supervised forest")
    }
    forest <- object
    xvar.names <- object$xvar.names
    x <- object$xvar[, xvar.names, drop = FALSE]
  }
  if (!(is.data.frame(x) || is.matrix(x)) ||
      !nrow(x) || !ncol(x)) {
    stop("object must contain a nonempty predictor matrix")
  }
  if (any(!vapply(as.data.frame(x), is.numeric, logical(1)))) {
    stop("Predictors must be numeric; use a varpro object to hot-encode factors")
  }
  ## Keep the original forest outcomes intact when extracting memberships.
  ## For multivariate VarPro forests, use the standard prediction extractor
  ## when predictions are stored in response-specific forest components.
  external <- !is.null(y.external)
  y <- if (external) y.external else forest$predicted.oob
  if (is.null(y) && !external) {
    y <- get.mv.predicted(forest, oob = TRUE)
  }
  if (is.null(y) ||
      any(!vapply(as.data.frame(y), is.numeric, logical(1)))) {
    stop("Numeric OOB predictions or a numeric y.external are required")
  }
  y <- data.matrix(y)
  if (nrow(y) != nrow(x) || ncol(y) < 1L) {
    stop("The response must have one row per processed training observation")
  }
  ## Record response labels before simplifying a single target to a vector.
  response.names <- colnames(y)
  if (is.null(response.names)) {
    if (!external && length(forest$yvar.names) == ncol(y) &&
        forest$family %in% c("regr", "regr+")) {
      response.names <- forest$yvar.names
    } else {
      response.names <- paste0("response", seq_len(ncol(y)))
    }
  }
  missing.name <- is.na(response.names) | !nzchar(response.names)
  response.names[missing.name] <- paste0("response", which(missing.name))
  response.names <- make.unique(response.names)
  colnames(y) <- response.names
  ## Binary forest probabilities retain the established first-class target.
  ## A supplied response matrix, including a two-column matrix, keeps all
  ## of its targets.
  if (!external && identical(forest$family, "class") && ncol(y) == 2L) {
    y <- y[, 1L, drop = FALSE]
    response.names <- response.names[1L]
  }
  if (ncol(y) == 1L) y <- y[, 1L]
  ## global SD of predictors (used when scale="global")
  x_sd_global <- if (is.data.frame(x)) {
    v <- sapply(x, function(z) stats::sd(as.numeric(z), na.rm = TRUE))
    as.numeric(v)
  } else {
    as.numeric(apply(as.matrix(x), 2, stats::sd, na.rm = TRUE))
  }
  names(x_sd_global) <- xvar.names
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
  ## ------------------------------------------------------------
  ## rule-level gradient estimation
  ## ------------------------------------------------------------
  mresp <- if (is.matrix(y)) ncol(y) else 1L
  R <- length(xreleaseId)
  n_cols <- 4L + mresp
  ruleMat <- matrix(NA_real_, nrow = R, ncol = n_cols)
  ## Extra per-rule metadata (used for interaction/Hessian diagnostics)
  ## - center: mean of released variable among OOB members (used for centering)
  ## - slope : unscaled slope estimate at chosen neighborhood
  ## - scale : local predictor scale at the chosen neighborhood
  ## - J     : effective neighborhood size used at the chosen cut
  ## - cut.idx: index of cut value at which the best estimate was attained
  rule.center <- rep(NA_real_, R)
  if (mresp == 1L) {
    rule.slope  <- rep(NA_real_, R)
    rule.scale  <- rep(NA_real_, R)
    rule.J      <- rep(NA_integer_, R)
    rule.cutidx <- rep(NA_integer_, R)
  } else {
    rule.slope  <- matrix(NA_real_, nrow = R, ncol = mresp)
    rule.scale  <- matrix(NA_real_, nrow = R, ncol = mresp)
    rule.J      <- matrix(NA_integer_, nrow = R, ncol = mresp)
    rule.cutidx <- matrix(NA_integer_, nrow = R, ncol = mresp)
  }
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
    ## store OOB center for this rule/variable
    rule.center[r] <- mean(xO, na.rm = TRUE)
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
      scale = scale,
      sd.global = x_sd_global[var_id]
    )
    ex <- attr(imp, "ivarpro.extra", exact = TRUE)
    if (mresp == 1L) {
      if (!is.null(ex)) {
        rule.slope[r]  <- ex$slope
        rule.scale[r]  <- ex$scale
        rule.J[r]      <- ex$J
        rule.cutidx[r] <- ex$cut.idx
      }
    } else {
      if (!is.null(ex) && length(ex) == mresp) {
        for (j in seq_len(mresp)) {
          rule.slope[r, j]  <- ex[[j]]$slope
          rule.scale[r, j]  <- ex[[j]]$scale
          rule.J[r, j]      <- ex[[j]]$J
          rule.cutidx[r, j] <- ex[[j]]$cut.idx
        }
      }
    }
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
    ## precompute common rule metadata (shared across responses)
    rule.variable <- as.integer(rO$variable)
    rule.tree     <- as.integer(rO$tree)
    rule.branch   <- as.integer(rO$branch)
    rule.n.oob    <- as.integer(rO$n.oob)
    memb_store    <- if (isTRUE(path.store.membership)) csO$oobMembership else NULL
    comp_store    <- if (isTRUE(path.store.membership)) compMembership else NULL
    ## Retain selected-rule diagnostics. Common metadata is stored on the
    ## list; response-specific slopes and scales are stored on each element.
    attr(out, "ivarpro.path") <- list(
      cut = cut,
      use.loo = use.loo,
      use.abs = use.abs,
      scale = scale,
      x.sd.global = x_sd_global,
      adaptive = adaptive,
      nmin = nmin,
      nmax = nmax,
      noise.na = noise.na,
      xvar.names = xvar.names,
      rule.variable = rule.variable,
      rule.center = rule.center,
      oobMembership = memb_store,
      compMembership = comp_store,
      rule.tree = rule.tree,
      rule.branch = rule.branch,
      rule.n.oob = rule.n.oob
    )
    for (j in seq_along(out)) {
      attr(out[[j]], "ivarpro.path") <- list(
        rule.imp = as.numeric(rO[[4 + j]]),
        rule.slope = as.numeric(rule.slope[, j]),
        rule.scale = as.numeric(rule.scale[, j]),
        rule.J = as.integer(rule.J[, j]),
        rule.cutidx = as.integer(rule.cutidx[, j])
      )
    }
  } else {
    out <- csimp.varpro.workhorse(csO, noise.na = noise.na)
    attr(out, "ivarpro.path") <- list(
      cut = cut,
      use.loo = use.loo,
      use.abs = use.abs,
      scale = scale,
      x.sd.global = x_sd_global,
      adaptive = adaptive,
      nmin = nmin,
      nmax = nmax,
      noise.na = noise.na,
      xvar.names = xvar.names,
      rule.variable = as.integer(rO$variable),
      rule.center = rule.center,
      oobMembership = if (isTRUE(path.store.membership)) csO$oobMembership else NULL,
      compMembership = if (isTRUE(path.store.membership)) compMembership else NULL,
      rule.imp = as.numeric(rO$imp),
      rule.slope = as.numeric(rule.slope),
      rule.scale = as.numeric(rule.scale),
      rule.J = as.integer(rule.J),
      rule.cutidx = as.integer(rule.cutidx),
      rule.tree = as.integer(rO$tree),
      rule.branch = as.integer(rO$branch),
      rule.n.oob = as.integer(rO$n.oob)
    )
  }
  ## Keep row identity and use unique names for responses in plotting data.
  if (is.list(out) && !inherits(out, "data.frame")) {
    for (j in seq_along(out)) rownames(out[[j]]) <- rownames(x)
  } else {
    rownames(out) <- rownames(x)
  }
  attr(out, "target") <- response.names
  if (isTRUE(save.data)) {
    proposed <- if (is.matrix(y)) paste0("y.", response.names) else "y"
    data.names <- utils::tail(make.unique(c(xvar.names, proposed)),
                             length(proposed))
    responses <- as.data.frame(y, check.names = FALSE)
    names(responses) <- data.names
    rownames(responses) <- rownames(x)
    saved <- data.frame(x, responses, check.names = FALSE)
    attr(saved, "response.names") <- setNames(data.names, response.names)
    attr(out, "data") <- saved
  }
  if (isTRUE(save.model)) {
    attr(out, "model") <- object
  }
  class(out) <- unique(c("ivarpro", class(out)))
  out
}
## ------------------------------------------------------------
## Print method for ivarpro objects
##
## For univariate ivarpro objects ('a data.frame with class c("ivarpro","data.frame")),
## this defers to the default data.frame printer.
##
## For multivariate/multiclass ivarpro objects (a list with class c("ivarpro","list")),
## this prints a short summary by default. Use print(x, full=TRUE) to print the full list.
## ------------------------------------------------------------
print.ivarpro <- function(x, ..., full = FALSE) {
  ## Multivariate/multiclass: list of gradient matrices
  if (is.list(x) && !inherits(x, "data.frame")) {
    k <- length(x)
    cat("ivarpro object (", k, " outcome", if (k == 1L) "" else "s", ")\n", sep = "")
    nm <- names(x)
    if (!is.null(nm) && length(nm)) {
      cat("  outcomes: ", paste(nm, collapse = ", "), "\n", sep = "")
    }
    if (k > 0L && inherits(x[[1]], "data.frame")) {
      cat("  each element: ", nrow(x[[1]]), " x ", ncol(x[[1]]), " gradient matrix\n", sep = "")
    }
    if (!is.null(attr(x, "data", exact = TRUE)))  cat("  data:  stored\n")
    if (!is.null(attr(x, "model", exact = TRUE))) cat("  model: stored\n")
    if (isTRUE(full)) {
      cat("\n")
      NextMethod()
    } else {
      cat("  (use x[[<target>]] to print one outcome; or print(x, full=TRUE))\n")
    }
    return(invisible(x))
  }
  ## Univariate: behave exactly like a data.frame
  NextMethod()
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
## grad.est: fast closed-form version (local standardized effect size)
## ------------------------------------------------------------
grad.est <- function(yO, yC, xO, xC,
                     cut, noise.na,
                     nmin = 10, nmax = 20,
                     use.loo = TRUE, use.abs = FALSE,
                     scale = c("local","global","none"),
                     sd.global = NA_real_) {
  out0 <- if (isTRUE(noise.na)) NA_real_ else 0
  scale <- match.arg(scale)
  ## combine OOB and complement
  x_all <- c(xO, xC)
  y_all <- c(yO, yC)
  ok <- is.finite(x_all) & is.finite(y_all)
  x_all <- x_all[ok]
  y_all <- y_all[ok]
  if (length(x_all) < nmin) {
    out <- out0
    attr(out, "ivarpro.extra") <- list(slope = NA_real_, scale = NA_real_, J = NA_integer_, cut.idx = NA_integer_)
    return(out)
  }
  ux <- sort(unique(x_all))
  ## --- 0/1 (one-hot) branch ---------------------------------
  if (length(ux) == 2L && all(ux %in% c(0, 1))) {
    n0 <- sum(x_all == 0)
    n1 <- sum(x_all == 1)
    if (n0 == 0L || n1 == 0L || (n0 + n1) < nmin) {
      out <- out0
      attr(out, "ivarpro.extra") <- list(slope = NA_real_, scale = NA_real_, J = NA_integer_, cut.idx = NA_integer_)
      return(out)
    }
    x_use <- x_all
    y_use <- y_all
    ## subsample down to nmax if needed, preserving both levels
    if ((n0 + n1) > nmax) {
      n_use <- min(nmax, n0 + n1)
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
      ## deterministic (no randomness): take first J0/J1 indices
      idx1_all <- which(x_all == 1)
      idx0_all <- which(x_all == 0)
      idx1 <- idx1_all[seq_len(J1)]
      idx0 <- idx0_all[seq_len(J0)]
      idx  <- c(idx0, idx1)
      x_use <- x_all[idx]
      y_use <- y_all[idx]
    }
    ## unscaled "slope" for 0/1 is mean difference
    y1 <- y_use[x_use == 1]
    y0 <- y_use[x_use == 0]
    b_x <- mean(y1) - mean(y0)
    xbar <- mean(x_use)
    sd_x <- sqrt(sum((x_use - xbar)^2) / (length(x_use) - 1L))
        if (identical(scale, "none")) {
      g <- b_x
    } else if (identical(scale, "global")) {
      sdg <- sd.global
      if (!is.finite(sdg) || sdg <= 0) sdg <- sd_x
      g <- b_x * sdg
    } else {
      ## local (default)
      g <- b_x * sd_x
    }
    if (isTRUE(use.abs)) g <- abs(g)
    out <- g
    attr(out, "ivarpro.extra") <- list(slope = b_x, scale = sd_x, J = length(x_use), cut.idx = NA_integer_)
    return(out)
  }
  ## --- continuous branch ------------------------------------
  mn <- mean(xO, na.rm = TRUE)
  if (!is.finite(mn)) {
    out <- out0
    attr(out, "ivarpro.extra") <- list(slope = NA_real_, scale = NA_real_, J = NA_integer_, cut.idx = NA_integer_)
    return(out)
  }
  x <- c(xO, xC) - mn
  y <- c(yO, yC)
  ok2 <- is.finite(x) & is.finite(y)
  x <- x[ok2]
  y <- y[ok2]
  if (length(x) < nmin) {
    out <- out0
    attr(out, "ivarpro.extra") <- list(slope = NA_real_, scale = NA_real_, J = NA_integer_, cut.idx = NA_integer_)
    return(out)
  }
  sdx <- sd(x, na.rm = TRUE)
  if (!is.finite(sdx) || sdx == 0) {
    out <- out0
    attr(out, "ivarpro.extra") <- list(slope = NA_real_, scale = NA_real_, J = NA_integer_, cut.idx = NA_integer_)
    return(out)
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
  ## track the best model and its diagnostics
  have <- FALSE
  if (isTRUE(use.loo)) {
    best_err <- Inf
  } else {
    best_Jcrit <- -Inf
  }
  best_grad <- out0
  best_slope <- NA_real_
  best_scale <- NA_real_
  best_J <- NA_integer_
  best_cutidx <- NA_integer_
  ## cache stats when J doesn't change across neighboring cut values
  last_J <- NA_integer_
  last_g <- out0
  last_err <- NA_real_
  last_slope <- NA_real_
  last_scale <- NA_real_
  for (t in seq_len(nc)) {
    k <- k_vec[t]
    if (k < nmin) {
      g <- out0
      err <- if (isTRUE(use.loo)) NA_real_ else NA_real_
      slope <- NA_real_
      scale.local <- NA_real_
      Jval <- k
    } else {
      J <- min(k, nmax)
      Jval <- J
      if (is.na(last_J) || J != last_J) {
        xsub <- x_ord[seq_len(J)]
        ysub <- y_ord[seq_len(J)]
        st <- .ivarpro_fast_lm1(xsub, ysub, use.loo = use.loo)
        ## local scale used by xx = scale(xsub, center=FALSE)
        s0 <- sqrt(sum(xsub * xsub) / (J - 1L))
        if (!is.finite(st$slope) || !is.finite(s0) || s0 <= 0) {
          g <- out0
          err <- if (isTRUE(use.loo)) NA_real_ else NA_real_
          slope <- NA_real_
          scale.local <- NA_real_
        } else {
          if (identical(scale, "none")) {
            g <- st$slope
          } else if (identical(scale, "global")) {
            sdg <- sd.global
            if (!is.finite(sdg) || sdg <= 0) sdg <- s0
            g <- st$slope * sdg
          } else {
            ## local (default)
            g <- st$slope * s0
          }
          if (isTRUE(use.abs)) g <- abs(g)
          err <- st$loo
          slope <- st$slope
          scale.local <- s0
        }
        last_J <- J
        last_g <- g
        last_err <- err
        last_slope <- slope
        last_scale <- scale.local
      } else {
        g <- last_g
        err <- last_err
        slope <- last_slope
        scale.local <- last_scale
      }
    }
    ## Select one valid neighborhood over the complete candidate grid.
    if (isTRUE(use.loo)) {
      if (is.finite(slope) && is.finite(g) && is.finite(err)) {
        if (!have || err < best_err) {
          have <- TRUE
          best_err <- err
          best_grad <- g
          best_slope <- slope
          best_scale <- scale.local
          best_J <- Jval
          best_cutidx <- t
        }
      }
    } else {
      if (is.finite(slope) && is.finite(g) && Jval >= nmin) {
        if (!have || Jval > best_Jcrit) {
          have <- TRUE
          best_Jcrit <- Jval
          best_grad <- g
          best_slope <- slope
          best_scale <- scale.local
          best_J <- Jval
          best_cutidx <- t
        }
      }
    }
  }
  out <- if (have) best_grad else out0
  attr(out, "ivarpro.extra") <- list(
    slope = best_slope,
    scale = best_scale,
    J = best_J,
    cut.idx = best_cutidx
  )
  out
}
## ------------------------------------------------------------
## cs.local.importance: supports vector xO/xC and matrix y
## ------------------------------------------------------------
cs.local.importance <- function(yO, yC, xO, xC, idx = NULL,
                                cut, noise.na, nmin, nmax,
                                use.loo = TRUE, use.abs = FALSE,
                                scale = c("local", "global", "none"),
                                sd.global = NA_real_) {
  if (!is.null(idx)) {
    xO <- xO[, idx]
    xC <- xC[, idx]
  }
  if (!is.matrix(yC)) {
    return(grad.est(yO, yC, xO, xC,
                    cut, noise.na, nmin, nmax, use.loo, use.abs,
                    scale = scale, sd.global = sd.global))
  }
  tmp <- lapply(seq_len(ncol(yC)), function(j) {
    grad.est(yO[, j], yC[, j], xO, xC,
             cut, noise.na, nmin, nmax, use.loo, use.abs,
             scale = scale, sd.global = sd.global)
  })
  out <- vapply(tmp, function(z) as.numeric(z[1L]), numeric(1))
  attr(out, "ivarpro.extra") <- lapply(tmp, function(z) {
    attr(z, "ivarpro.extra", exact = TRUE)
  })
  out
}
## ------------------------------------------------------------
## Case-specific aggregation workhorse
## ------------------------------------------------------------
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
        ## noise.na=FALSE: NA treated as 0
        if (!is.finite(v)) v <- 0
        sum_mat[idx, j] <- sum_mat[idx, j] + v
      }
    }
  }
  if (isTRUE(noise.na)) {
    ## mean with na.rm=TRUE, but only for present cells; absent remain NA
    sum_mat <- sum_mat / count_nonNA  ## yields NaN where count_nonNA==0
    sum_mat[count_nonNA == 0L] <- NA_real_
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
