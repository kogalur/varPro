# Unified base-R iVarPro SHAP summary plot
# ivar_mat: n x p_i matrix of iVarPro values (local gradients) for some features
# dat_mat : n x p_all matrix of original feature values (may have more columns)
#
# - Uses column names to align ivar_mat and dat_mat (typical case: dat_mat is
#   full design matrix, ivar_mat is subset).
# - scale_value = TRUE  : per-feature feature-values are rescaled to [0,1]
#                         for colouring (avoids one big-range feature killing
#                         the colour range for others).
# - style = "blobby"    : quasi-beeswarm style (default)
#   style = "jitter"    : simple vertical jitter per feature

ivarpro.shap <- function(ivar_mat,
                         dat_mat,
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
  
  ## --- Input checks & conversion ---
  stopifnot(is.matrix(ivar_mat) || is.data.frame(ivar_mat))
  stopifnot(is.matrix(dat_mat)  || is.data.frame(dat_mat))

  ## --- remove importance with all missing values, or all zeroes ---
  bad <- sapply(data.frame(ivar_mat), function(x) {all(is.na(x) | x == 0)})
  if (sum(!bad) == 0) stop("all importance values are zero or NA")
  ivar_mat <- ivar_mat[, !bad]
  
  ivar_mat <- as.matrix(ivar_mat)
  dat_mat  <- as.matrix(dat_mat)
  
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
