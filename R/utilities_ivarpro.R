# Unified base-R iVarPro SHAP summary plot
# ivar_mat: n x p_i matrix of iVarPro values (local gradients) for some features
# dat_mat : n x p_all matrix of original feature values (may have more columns)
#
# - Uses column names to align ivarand dat_mat (typical case: dat_mat is
#   full design matrix, ivar_mat is subset).
# - scale_value = TRUE  : per-feature feature-values are rescaled to [0,1]
#                         for colouring (avoids one big-range feature killing
#                         the colour range for others).
# - style = "blobby"    : quasi-beeswarm style (default)
#   style = "jitter"    : simple vertical jitter per feature
ivarpro.shap <- function(ivar,
                         dat = NULL,
                         feature_names = NULL,
                         max.points   = 5000,
                         max.points.per.feature = NULL,
                         point.alpha  = 1.0,
                         point.size   = 0.35,
                         point.pch    = 16,
                         scale.value  = TRUE,
                         style        = c("blobby", "jitter"),
                         blobby.separation = 3) 
{
  style <- match.arg(style)
  ## special handling for class outcomes (TBD: extend to multivariate)
  if (is.list(ivar) && !inherits(ivar, "data.frame")) {
    if (is.null(dat) && !is.null(attr(ivar, "data"))) {
      dat  <- attr(ivar, "data")
    }
    ivar <- ivar[[1]]
  }
  ## pull the x,y values from the original call, otherwise x must be supplied
  if (is.null(dat) && !is.null(attr(ivar, "data"))) {
    dat  <- attr(ivar, "data")
  }
  if (is.null(dat)) {
    stop("need to supply data from the original varpro call")
  }
  ## --- remove importance with all missing values, or all zeroes ---
  bad <- sapply(data.frame(ivar), function(x) {all(is.na(x) | x == 0)})
  if (sum(!bad) == 0) stop("all importance values are zero or NA")
  ivar <- ivar[, !bad]
  ## conversion to matrices
  ivar_mat <- as.matrix(ivar)
  dat_mat  <- as.matrix(dat)
  ## coherence checks
  if (nrow(ivar_mat) != nrow(dat_mat)) {
    stop("ivar_mat and dat_mat must have the same number of rows.")
  }
  n  <- nrow(ivar_mat)
  pI <- ncol(ivar_mat)
  ## --- Feature names for ivar_mat ---
  if (is.null(colnames(ivar_mat))) {
    if (!is.null(feature_names)) {
      if (length(feature_names) != pI) {
        stop("feature_names must have length ncol(ivar_mat).")
      }
      colnames(ivar_mat) <- feature_names
    } else if (!is.null(colnames(dat_mat)) && ncol(dat_mat) == pI) {
      colnames(ivar_mat) <- colnames(dat_mat)
    } else {
      colnames(ivar_mat) <- paste0("x", seq_len(pI))
    }
  } else {
    if (!is.null(feature_names) &&
        !identical(feature_names, colnames(ivar_mat))) {
      warning("feature_names provided but ivar_mat already has column names; ",
              "using colnames(ivar_mat).")
    }
  }
  ## --- Align dat_mat to ivar_mat (dat_mat can have extra columns) ---
  ivar_names <- colnames(ivar_mat)
  if (!is.null(colnames(dat_mat))) {
    dat_names <- colnames(dat_mat)
    if (all(ivar_names %in% dat_names)) {
      dat_mat_sub <- dat_mat[, ivar_names, drop = FALSE]
    } else if (ncol(dat_mat) == pI) {
      warning("dat_mat does not contain all ivar_mat column names; ",
              "using positional matching.")
      dat_mat_sub <- dat_mat
      colnames(dat_mat_sub) <- ivar_names
    } else {
      stop("Cannot align dat_mat to ivar_mat: ",
           "dat_mat must contain all columns named in ivar_mat ",
           "(or have the same number of columns).")
    }
  } else {
    if (ncol(dat_mat) == pI) {
      dat_mat_sub <- dat_mat
      colnames(dat_mat_sub) <- ivar_names
    } else {
      stop("dat_mat has no column names and more columns than ivar_mat; ",
           "cannot automatically align features.")
    }
  }
  ## --- Order features by global importance: mean |iVarPro| ---
  mean_abs <- colMeans(abs(ivar_mat), na.rm = TRUE)
  ord      <- order(mean_abs, decreasing = TRUE)
  ivar_mat    <- ivar_mat[, ord, drop = FALSE]
  dat_mat_sub <- dat_mat_sub[, ord, drop = FALSE]
  mean_abs    <- mean_abs[ord]
  p <- ncol(ivar_mat)
  feature_labels <- colnames(ivar_mat)
  ## --- Flatten to "long" representation ---
  importance    <- as.vector(ivar_mat)               # length n * p
  value         <- as.vector(dat_mat_sub)            # same length
  feature_index <- rep(seq_len(p), each = n)         # 1..p repeated
  keep <- is.finite(importance) & is.finite(value)
  importance    <- importance[keep]
  value         <- value[keep]
  feature_index <- feature_index[keep]
  if (!length(importance)) {
    stop("No finite iVarPro / feature-value pairs to plot.")
  }
  ## --- Global subsample if many points ---
  n_points <- length(importance)
  if (n_points > max.points) {
    set.seed(2025)
    idx <- sample.int(n_points, max.points)
    importance    <- importance[idx]
    value         <- value[idx]
    feature_index <- feature_index[idx]
    n_points      <- max.points
  }
  ## --- Optional per-feature cap (for speed on very large data) ---
  if (!is.null(max.points.per.feature)) {
    if (!is.numeric(max.points.per.feature) ||
        length(max.points.per.feature) != 1L ||
        max.points.per.feature <= 0) {
      stop("max.points.per.feature must be a positive scalar or NULL.")
    }
    tab <- table(feature_index)
    big <- as.integer(names(tab[tab > max.points.per.feature]))
    if (length(big)) {
      set.seed(2025)
      keep_idx <- rep(TRUE, length(feature_index))
      for (j in big) {
        idx_j  <- which(feature_index == j)
        n_j    <- length(idx_j)
        drop_n <- n_j - max.points.per.feature
        if (drop_n > 0L) {
          drop_j <- sample(idx_j, drop_n)
          keep_idx[drop_j] <- FALSE
        }
      }
      importance    <- importance[keep_idx]
      value         <- value[keep_idx]
      feature_index <- feature_index[keep_idx]
      n_points      <- length(importance)
    }
  }
  ## --- Colour mapping: feature value -> gradient ---
  palette_cols <- c("#313695", "#4575b4", "#abd9e9",
                    "#ffffbf",
                    "#fdae61", "#d73027", "#a50026")
  pal_fun  <- grDevices::colorRampPalette(palette_cols)
  n_col    <- 256L
  col_ramp <- pal_fun(n_col)
  if (isTRUE(scale.value)) {
    value_scaled <- numeric(length(value))
    for (j in seq_len(p)) {
      idx_j <- which(feature_index == j)
      if (!length(idx_j)) next
      vj <- value[idx_j]
      vr <- range(vj, finite = TRUE)
      if (!is.finite(vr[1]) || !is.finite(vr[2]) || vr[1] == vr[2]) {
        value_scaled[idx_j] <- 0.5
      } else {
        z <- (vj - vr[1]) / (vr[2] - vr[1])
        z[z < 0] <- 0
        z[z > 1] <- 1
        value_scaled[idx_j] <- z
      }
    }
    z_scaled <- value_scaled
    z_scaled[!is.finite(z_scaled)] <- 0.5
    z_scaled[z_scaled < 0] <- 0
    z_scaled[z_scaled > 1] <- 1
    val_range_legend <- c(0, 1)
    legend_label     <- "Feature value (scaled)"
  } else {
    vr <- range(value, finite = TRUE)
    if (!is.finite(vr[1]) || !is.finite(vr[2]) || vr[1] == vr[2]) {
      z_scaled <- rep(0.5, length(value))
    } else {
      z_scaled <- (value - vr[1]) / diff(vr)
      z_scaled[z_scaled < 0] <- 0
      z_scaled[z_scaled > 1] <- 1
    }
    val_range_legend <- vr
    legend_label     <- "Feature value"
  }
  col_index  <- floor(z_scaled * (n_col - 1L)) + 1L
  point.cols <- col_ramp[col_index]
  point.cols <- grDevices::adjustcolor(point.cols, alpha.f = point.alpha)
  ## --- Y positions: "blobby" (beeswarm-like) vs "jitter" ---
  y_lim <- c(0.5, p + 0.5)
  y     <- numeric(length(feature_index))
  if (style == "jitter") {
    # Simple, very fast, SHAP-style jitter
    jitter_height <- 0.25
    y <- feature_index +
      stats::runif(length(feature_index), -jitter_height, jitter_height)
  } else {  # style == "blobby"
    # Separation between points in y-units; bigger -> less overlap
    radius_y <- blobby.separation * 0.03 * (point.size / 0.7)
    if (!is.finite(radius_y) || radius_y <= 0) radius_y <- 0.03
    radius_y <- min(radius_y, 0.12)   # allow a bit bigger than before
    diameter_y <- 2 * radius_y
    max.disp   <- 0.45
    radius2    <- (2 * radius_y)^2
    max.dx     <- 2 * radius_y
    for (j in seq_len(p)) {
      idx_j <- which(feature_index == j)
      if (!length(idx_j)) next
      x_sub <- importance[idx_j]
      n_sub <- length(x_sub)
      y_sub <- rep(j, n_sub)
      if (n_sub == 1L) {
        y[idx_j] <- j
        next
      }
      ord_j    <- order(x_sub)
      x_sorted <- x_sub[ord_j]
      y_sorted <- rep(j, n_sub)
      for (kk in seq_len(n_sub)) {
        y_i   <- j
        tries <- 0L
        if (kk > 1L) {
          repeat {
            conflict <- FALSE
            for (mm in (kk - 1L):1L) {
              dx <- x_sorted[kk] - x_sorted[mm]
              if (dx > max.dx) break
              dy <- y_i - y_sorted[mm]
              if ((dx * dx + dy * dy) < radius2) {
                conflict <- TRUE
                break
              }
            }
            if (!conflict) break
            tries <- tries + 1L
            layer <- ceiling(tries / 2)
            sign  <- if ((tries %% 2L) == 1L) 1 else -1
            y_i   <- j + sign * layer * diameter_y * 0.55
            if (abs(y_i - j) > max.disp) {
              y_i <- j + stats::runif(1L, -max.disp, max.disp)
              break
            }
          }
        }
        y_sorted[kk] <- y_i
      }
      y_sub[ord_j] <- y_sorted
      y[idx_j]     <- y_sub
    }
  }
  ## --- X limits (with a bit of padding) ---
  x_range <- range(importance, finite = TRUE)
  x_pad   <- 0.05 * diff(x_range)
  if (!is.finite(x_pad) || x_pad == 0) x_pad <- 1
  x_min <- x_range[1] - x_pad
  x_max <- x_range[2] + x_pad
  # If all importance values are non-negative, don't extend left of 0
  if (min(importance, na.rm = TRUE) >= 0) {
    x_min <- 0
  }
  x_lim <- c(x_min, x_max)
  ## --- Plot: base graphics ---
  op <- par(no.readonly = TRUE)
  on.exit(par(op), add = TRUE)
  par(mar = c(5, 7, 4, 6) + 0.1)
  plot(importance, y,
       type = "n",
       xlim = x_lim,
       ylim = y_lim,
       xlab = "iVarPro value (local gradient)",
       ylab = "",
       yaxt = "n",
       main = "iVarPro SHAP summary plot",
       xaxs = "i")  # no extra padding on x
  axis(2, at = seq_len(p), labels = feature_labels, las = 1, cex.axis = 0.8)
  abline(v = 0, lty = 2)
  points(importance, y,
         pch  = point.pch,
         cex  = point.size,
         col  = point.cols)
  ## --- Vertical colour legend ---
  usr <- par("usr")
  x_left   <- usr[2] + 0.02 * diff(usr[1:2])
  x_right  <- usr[2] + 0.06 * diff(usr[1:2])
  y_bottom <- usr[3]
  y_top    <- usr[4]
  y_seq <- seq(y_bottom, y_top, length.out = n_col + 1L)
  par(xpd = TRUE)
  for (i in seq_len(n_col)) {
    rect(x_left, y_seq[i], x_right, y_seq[i + 1L],
         border = NA, col = col_ramp[i])
  }
  text(x = (x_left + x_right) / 2,
       y = y_top + 0.05 * (y_top - y_bottom),
       labels = legend_label,
       adj = c(0.5, 0),
       cex = 0.8)
  text(x = x_right + 0.01 * diff(usr[1:2]),
       y = y_bottom,
       labels = signif(val_range_legend[1], 3),
       adj = c(0, 0.5),
       cex = 0.7)
  text(x = x_right + 0.01 * diff(usr[1:2]),
       y = y_top,
       labels = signif(val_range_legend[2], 3),
       adj = c(0, 0.5),
       cex = 0.7)
  invisible(NULL)
}
##############################################################
##
## partial plot 
##
##############################################################
## Scatter: x[var] vs ivar[var] with optional ladder-band.
## Optional: color points by x[col.var], size points by x[size.var]
ivarpro.partial <- function(ivar,
                            var,
                            col.var = NULL,
                            size.var = NULL,
                            x = NULL,
                            ladder = TRUE,
                            ladder.cuts = NULL,
                            ladder.max.segments = 3000,
                            pch = 16,
                            cex = 0.8,
                            cex.range = c(0.5, 2),
                            main = NULL,
                            xlab = NULL,
                            ylab = "iVarPro gradient",
                            legend = TRUE,
                            ...) {
  ## special handling for class outcomes (TBD: extend to multivariate)
  if (is.list(ivar) && !inherits(ivar, "data.frame")) {
    if (is.null(x) && !is.null(attr(ivar, "data"))) {
      x  <- attr(ivar, "data")
    }
    ivar <- ivar[[1]]
  }
  ## resolve feature matrix: also add y if possible
  if (is.null(x) && !is.null(attr(ivar, "data"))) {
    x  <- attr(ivar, "data")
  }
  if (is.null(x)) {
    stop("need to supply x from the original varpro call")
  }
  ## variable name
  if (is.character(var)) {
    var_name <- var
  } else {
    var_name <- colnames(ivar)[as.integer(var)]
  }
  if (is.null(var_name) || !(var_name %in% colnames(ivar))) {
    stop("Could not resolve 'var' in ivar columns.")
  }
  if (!(var_name %in% colnames(x))) {
    stop("Plotting requires that 'x' contains the plotted variable.")
  }
  xv <- x[, var_name]
  yv <- ivar[[var_name]]
  ## check if requested var has all missing values
  if (all(is.na(yv))) {
    stop("requested variable has gradient with all missing values:", var, "\n")
  }
  ## compute ladder band if requested and available
  band_df <- NULL
  if (isTRUE(ladder)) {
    path <- attr(ivar, "ivarpro.path")
    if (!is.null(path)) {
      ## ivarpro_band() will error if membership is not stored; in that case
      ## the plot still works, just without ladder info.
      band_df <- tryCatch(
        ivarpro_band(ivar, var = var_name, cuts = ladder.cuts, return.matrix = FALSE),
        error = function(e) NULL
      )
    }
  }
  ## colors
  col_pt <- rep("black", length(yv))
  col_legend <- NULL
  if (!is.null(col.var)) {
    if (!(col.var %in% colnames(x))) stop("col.var not found in x.")
    cv <- x[, col.var]
    if (is.numeric(cv) && length(unique(cv)) > 2) {
      pal <- grDevices::colorRampPalette(c("navy", "skyblue", "gold", "firebrick"))(100)
      rng <- range(cv, na.rm = TRUE)
      if (is.finite(rng[1]) && is.finite(rng[2]) && rng[2] > rng[1]) {
        z <- (cv - rng[1]) / (rng[2] - rng[1])
        k <- pmax(1L, pmin(100L, 1L + floor(99 * z)))
        col_pt <- pal[k]
        col_legend <- list(type = "numeric", rng = rng, pal = pal, var = col.var)
      }
    } else {
      ff <- as.factor(cv)
      lev <- levels(ff)
      pal <- grDevices::rainbow(length(lev))
      col_pt <- pal[as.integer(ff)]
      col_legend <- list(type = "factor", lev = lev, pal = pal, var = col.var)
    }
  }
  ## sizes
  cex_pt <- rep(cex, length(yv))
  if (!is.null(size.var)) {
    if (!(size.var %in% colnames(x))) stop("size.var not found in x.")
    sv <- x[, size.var]
    if (!is.numeric(sv)) sv <- as.numeric(sv)
    rng <- range(sv, na.rm = TRUE)
    if (is.finite(rng[1]) && is.finite(rng[2]) && rng[2] > rng[1]) {
      z <- (sv - rng[1]) / (rng[2] - rng[1])
      cex_pt <- cex.range[1] + z * (cex.range[2] - cex.range[1])
    }
  }
  ## finite plotting set
  ok <- is.finite(xv) & is.finite(yv)
  if (!is.null(band_df)) {
    ok <- ok & is.finite(band_df$main)
  }
  xv <- xv[ok]
  yv <- yv[ok]
  col_pt <- col_pt[ok]
  cex_pt <- cex_pt[ok]
  if (!is.null(band_df)) {
    lo <- band_df$lower[ok]
    hi <- band_df$upper[ok]
  } else {
    lo <- hi <- NULL
  }
  ## plot
  if (is.null(main)) main <- paste0(var_name, " vs iVarPro gradient")
  if (is.null(xlab)) xlab <- var_name
  graphics::plot(xv, yv,
                 xlab = xlab, ylab = ylab,
                 main = main,
                 pch = pch, col = col_pt, cex = cex_pt,
                 ...)
  ## add ladder band as vertical segments (thinned if necessary)
  if (!is.null(lo) && !is.null(hi)) {
    okb <- is.finite(lo) & is.finite(hi)
    if (any(okb)) {
      nn <- sum(okb)
      take <- which(okb)
      if (nn > ladder.max.segments) {
        take <- take[round(seq(1, nn, length.out = ladder.max.segments))]
      }
      band_col <- grDevices::adjustcolor("gray60", alpha.f = 0.35)
      graphics::segments(x0 = xv[take], y0 = lo[take],
                         x1 = xv[take], y1 = hi[take],
                         col = band_col, lwd = 1)
    }
  }
  ## (re)plot points on top for clarity
  graphics::points(xv, yv, pch = pch, col = col_pt, cex = cex_pt)
  ## legend
  if (isTRUE(legend) && !is.null(col_legend)) {
    if (col_legend$type == "factor") {
      graphics::legend("topright",
                       legend = col_legend$lev,
                       col = col_legend$pal, pch = 16, bty = "n",
                       title = col_legend$var)
    } else if (col_legend$type == "numeric") {
      ## simple numeric legend using quantiles
      qs <- stats::quantile(x[, col_legend$var], probs = c(0.05, 0.5, 0.95), na.rm = TRUE)
      kk <- pmax(1L, pmin(100L, 1L + floor(99 * (qs - col_legend$rng[1]) / (col_legend$rng[2] - col_legend$rng[1]))))
      graphics::legend("topright",
                       legend = sprintf("%s = %.3g", names(qs), as.numeric(qs)),
                       col = col_legend$pal[kk],
                       pch = 16, bty = "n",
                       title = col_legend$var)
    }
  }
  invisible(TRUE)
}
## Path helper: per-variable bands from rule-level ladder
## Compute per-case ladder summary for ONE variable.
## - 'ivar' must be a data.frame returned by ivarpro() from this script (>= 1.7)
## - Returns a data.frame with main gradient + lower/upper band across ladder cuts.
ivarpro_band <- function(ivar,
                         var,
                         cuts = NULL,
                         return.matrix = FALSE) {
  if (is.list(ivar) && !inherits(ivar, "data.frame")) {
    ## allow users to pass multivariate output by selecting first element
    ivar <- ivar[[1]]
  }
  path <- attr(ivar, "ivarpro.path")
  if (is.null(path)) {
    stop("No 'ivarpro.path' attribute found. This object was likely created with an older ivarpro(). Re-run ivarpro() using this script to attach path information.")
  }
  if (is.null(path$oobMembership) || is.null(path$rule.imp.ladder)) {
    stop("Path info is missing membership and/or ladder gradients. Make sure path.store.membership=TRUE (default) when calling ivarpro().")
  }
  xn <- path$xvar.names
  if (is.character(var)) {
    j <- match(var, xn)
    if (is.na(j)) stop("Unknown 'var': not found in xvar.names.")
    var_name <- var
  } else {
    j <- as.integer(var)
    if (!is.finite(j) || j < 1L || j > length(xn)) stop("Invalid 'var' index.")
    var_name <- xn[j]
  }
  n <- nrow(ivar)
  main <- ivar[[var_name]]
  ladder <- path$rule.imp.ladder
  cut.ladder <- path$cut.ladder
  L <- length(cut.ladder)
  if (L == 0L || ncol(ladder) == 0L) {
    ## nothing to summarize
    out <- data.frame(main = main,
                      lower = NA_real_,
                      upper = NA_real_,
                      n.rules = 0L)
    attr(out, "cut.ladder") <- cut.ladder
    attr(out, "var") <- var_name
    return(out)
  }
  ## choose which ladder cuts to include
  if (is.null(cuts)) {
    kk <- seq_len(L)
  } else {
    ## if integer-ish, treat as indices; else treat as cut values
    if (all(is.finite(cuts)) && all(abs(cuts - round(cuts)) < 1e-8) &&
        all(cuts >= 1) && all(cuts <= L)) {
      kk <- unique(as.integer(cuts))
    } else {
      kk <- match(cuts, cut.ladder)
      kk <- kk[is.finite(kk)]
      kk <- unique(as.integer(kk))
    }
    if (!length(kk)) stop("No valid ladder cuts selected.")
  }
  ## select rules for this release variable
  ridx <- which(path$rule.variable == j)
  memb <- path$oobMembership
  ## alloc (only for selected kk)
  K <- length(kk)
  sum_mat <- matrix(0, nrow = n, ncol = K)
  cnt_mat <- matrix(0L, nrow = n, ncol = K)
  cnt_rules <- integer(n)
  ## accumulate
  for (rr in ridx) {
    idx <- memb[[rr]]
    if (!length(idx)) next
    cnt_rules[idx] <- cnt_rules[idx] + 1L
    v <- ladder[rr, kk, drop = TRUE]
    ## v can be scalar when K==1
    if (K == 1L) {
      if (is.finite(v)) {
        sum_mat[idx, 1] <- sum_mat[idx, 1] + v
        cnt_mat[idx, 1] <- cnt_mat[idx, 1] + 1L
      }
    } else {
      for (t in seq_len(K)) {
        vt <- v[t]
        if (is.finite(vt)) {
          sum_mat[idx, t] <- sum_mat[idx, t] + vt
          cnt_mat[idx, t] <- cnt_mat[idx, t] + 1L
        }
      }
    }
  }
  ## compute means for each selected ladder cut
  means <- sum_mat / cnt_mat  ## NaN where cnt_mat==0
  ## handle variables absent for a case (match ivarpro semantics)
  if (isTRUE(path$noise.na)) {
    means[cnt_rules == 0L, ] <- NA_real_
  } else {
    means[cnt_rules == 0L, ] <- 0
  }
  ## lower/upper across selected ladder cuts (ignore NA/NaN/Inf)
  lower <- rep(NA_real_, n)
  upper <- rep(NA_real_, n)
  for (t in seq_len(K)) {
    z <- means[, t]
    ok <- is.finite(z)
    if (any(ok)) {
      if (all(is.na(lower))) {
        ## initialize
        lower[ok] <- z[ok]
        upper[ok] <- z[ok]
      } else {
        lower[ok] <- pmin(lower[ok], z[ok], na.rm = TRUE)
        upper[ok] <- pmax(upper[ok], z[ok], na.rm = TRUE)
      }
    }
  }
  out <- data.frame(main = main,
                    lower = lower,
                    upper = upper,
                    n.rules = cnt_rules)
  if (isTRUE(return.matrix)) {
    attr(out, "means") <- means
  }
  attr(out, "cut.ladder") <- cut.ladder[kk]
  attr(out, "var") <- var_name
  out
}
