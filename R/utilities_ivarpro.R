# iVarPro SHAP summary plot
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
## ------------------------------------------------------------
## Internal helper: select target response/class from ivarpro list.
## - target = NULL      : default to first element (with warning)
## - target = integer   : select by position
## - target = character : select by name (e.g., class label)
## Returns the selected data.frame/matrix of gradients.
## ------------------------------------------------------------
.ivarpro_select_target <- function(ivar_list,
                                   target = NULL,
                                   warn = TRUE,
                                   caller = NULL) {
  if (!is.list(ivar_list) || inherits(ivar_list, "data.frame")) {
    stop(".ivarpro_select_target expects a list (multivariate/multiclass ivarpro output).")
  }
  m <- length(ivar_list)
  if (m == 0L) stop("ivarpro list is empty.")
  nm <- names(ivar_list)
  sel_idx <- NULL
  if (is.null(target)) {
    sel_idx <- 1L
  } else if (is.numeric(target) && length(target) == 1L && is.finite(target)) {
    if (target != floor(target) || target < 1L || target > m) {
      stop("target must be an integer index between 1 and ", m, ".")
    }
    sel_idx <- as.integer(target)
  } else if (is.character(target) && length(target) == 1L &&
             !is.na(target) && nzchar(target)) {
    if (is.null(nm)) stop("target was supplied as a name but ivarpro list has no names.")
    sel_idx <- match(target, nm)
    if (is.na(sel_idx)) {
      stop("Unknown target '", target, "'. Available targets: ",
           paste(nm, collapse = ", "))
    }
  } else {
    stop("target must be NULL, a single numeric index, or a single character name.")
  }
  sel_lab <- if (!is.null(nm) && length(nm) >= sel_idx && !is.na(nm[sel_idx]) && nzchar(nm[sel_idx])) nm[sel_idx] else as.character(sel_idx)
  if (isTRUE(warn) && is.null(target)) {
    if (is.null(caller)) caller <- "ivarpro"
    warning(caller, ": ivar is a list (multivariate/multiclass); defaulting to first element (",
            sel_lab, "). Use target= to choose another.", call. = FALSE)
  }
  ivar_list[[sel_idx]]
}
shap.ivarpro <- function(ivar,
                         dat = NULL,
                         feature_names = NULL,
                         max.points   = 5000,
                         max.points.per.feature = NULL,
                         point.alpha  = 1.0,
                         point.size   = 0.35,
                         point.pch    = 16,
                         scale.value  = TRUE,
                         style        = c("blobby", "jitter"),
                         blobby.separation = 3,
                         target = NULL) 
{
  style <- match.arg(style)
  ## special handling for multivariate / multiclass ivarpro output
  if (is.list(ivar) && !inherits(ivar, "data.frame")) {
    if (is.null(dat) && !is.null(attr(ivar, "data"))) {
      dat <- attr(ivar, "data")
    }
    ivar <- .ivarpro_select_target(ivar, target = target, warn = TRUE, caller = "shap.ivarpro")
  }
  ## pull the x,y values from the original call, otherwise x must be supplied
  if (is.null(dat) && !is.null(attr(ivar, "data"))) {
    dat  <- attr(ivar, "data")
  }
  if (is.null(dat)) {
    stop("need to supply data from the original varpro call")
  }
  ## Keep the gradient matrix two-dimensional, including one-feature plots.
  if (!(is.data.frame(ivar) || is.matrix(ivar)) ||
      any(!vapply(as.data.frame(ivar), is.numeric, logical(1)))) {
    stop("ivar must be a numeric data frame or matrix")
  }
  ivar_mat <- as.matrix(ivar)
  dat_mat <- if (is.data.frame(dat)) dat else as.matrix(dat)
  if (nrow(ivar_mat) != nrow(dat_mat)) {
    stop("ivar and dat must have the same number of rows.")
  }
  n <- nrow(ivar_mat)
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
  ## Coerce only the aligned columns, so unused nonnumeric columns in dat
  ## do not change the type of the plotted feature values.
  if (any(!vapply(as.data.frame(dat_mat_sub), is.numeric, logical(1)))) {
    stop("Plotting values must be numeric; use the stored processed data")
  }
  dat_mat_sub <- as.matrix(dat_mat_sub)
  keep.feature <- colSums(is.finite(ivar_mat) & ivar_mat != 0) > 0L
  if (!any(keep.feature)) stop("all importance values are zero or nonfinite")
  ivar_mat <- ivar_mat[, keep.feature, drop = FALSE]
  dat_mat_sub <- dat_mat_sub[, keep.feature, drop = FALSE]
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
       main = "iVarPro summary plot",
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
plot.ivarpro <- function(x,
      var,
      col.var = NULL,
      size.var = NULL,
      data = NULL,
      target = NULL,
      pch = 16,
      cex = 0.8,
      cex.range = c(0.5, 2),
      main = NULL,
      xlab = NULL,
      ylab = "iVarPro gradient",
      legend = TRUE,
      ...) {
  ## ------------------------------------------------------------
  ## Extra plotting controls (kept in ... so old calls don't break)
  ##
  ## col.style        : "auto" (default), "solid", "outline", "binary"
  ## col.outline      : outline/border colour (default "gray10")
  ## col.outline.lwd  : outline width (default 0.4)
  ## col.binary.pch   : pch mapping for 2-level factors (default c(16, 1))
  ## col.binary.lwd   : line width for open symbols in binary mode (default 1)
  ## col.dodge        : fraction of x-range used to offset factor levels
  ##                  (default 0; try 0.01 for dense discrete x)
  ## col.var.discrete.max : if col.var is numeric with <= this many unique
  ##                       finite values, treat it like a factor (categorical).
  ##                       (default 10; set to 2 for legacy behaviour)
  ##
  ## zero.line        : add a dashed reference line at y = 0 (default TRUE)
  ## zero.line.col    : colour for the y=0 line (default "gray60")
  ## zero.line.lty    : line type for the y=0 line (default 2)
  ## zero.line.lwd    : line width for the y=0 line (default 1)
  ##
  ## jitter           : horizontal jitter to reduce overplotting
  ##                  : TRUE (default), FALSE, or a numeric amount (x-units)
  ## jitter.amount    : explicit jitter amount in x-units (overrides jitter=TRUE)
  ## jitter.fraction  : if jitter=TRUE and jitter.amount is NULL, use this
  ##                  : fraction of x-range (default 0.005)
  ## jitter.seed      : optional seed for reproducible jitter (default NULL)
  ##
  ## x.dist           : show marginal x distribution (default "none")
  ##                  : character vector of {"none", "rug", "hist", "density", "auto"}
  ## x.dist.side      : "bottom" (default) or "top"
  ## x.dist.height    : fraction of y-range used for the distribution strip (default 0.12)
  ## x.dist.pad       : fraction of y-range padding from plot edge (default 0.02)
  ## x.dist.bins      : histogram breaks specification (default "FD")
  ## x.dist.adjust    : density bandwidth adjustment factor (default 1)
  ## x.dist.n         : density grid size (default 512)
  ## x.dist.col       : fill colour for histogram/density
  ## x.dist.border    : outline colour (NA disables outline)
  ## x.dist.lwd/lty   : line width/type for density outline
  ## x.dist.rug.col   : rug tick colour
  ## x.dist.rug.lwd   : rug tick line width
  ## x.dist.rug.ticksize : rug tick length as fraction of y-range (default 0.03)
  ## x.dist.rug.max   : maximum number of rug ticks to draw (default 2000)
  ##
  ## smooth           : draw loess smooth curves stratified by col.var
  ##                  : (default TRUE)
  ## smooth.span      : loess span (default 0.75)
  ## smooth.degree    : loess degree (default 2)
  ## smooth.family    : loess family (default "gaussian")
  ## smooth.lwd       : line width (default 2)
  ## smooth.lty       : line type (default 1)
  ## smooth.alpha     : alpha for smooth lines (default 0.9)
  ## smooth.min.n     : minimum points per group to fit a smooth (default 30)
  ## smooth.n.grid    : number of x grid points per smooth (default 200)
  ##
  ## col.legend.probs : quantile probs shown in legend for numeric col.var
  ##                  : default c(0.05, 0.25, 0.5, 0.75, 0.95)
  ## col.legend.n     : alternative to col.legend.probs; if set (>=2), uses
  ##                  : seq(0.05, 0.95, length.out = col.legend.n)
  ##
  ## smooth.probs     : quantile probs used to define numeric col.var strata
  ##                  : for smooth curves (default = col.legend.probs)
  ## smooth.n         : alternative to smooth.probs; if set (>=2), uses
  ##                  : seq(0.05, 0.95, length.out = smooth.n)
  ## ------------------------------------------------------------
  ivar <- x
  dots <- list(...)
  col.style <- dots$col.style
  if (is.null(col.style)) col.style <- "auto"
  col.style <- match.arg(as.character(col.style),
                         c("auto", "solid", "outline", "binary"))
  col.outline <- dots$col.outline
  if (is.null(col.outline)) col.outline <- "gray10"
  col.outline.lwd <- dots$col.outline.lwd
  if (is.null(col.outline.lwd)) col.outline.lwd <- 0.4
  col.outline.lwd <- as.numeric(col.outline.lwd)[1]
  if (!is.finite(col.outline.lwd) || col.outline.lwd <= 0) col.outline.lwd <- 0.4
  col.binary.pch <- dots$col.binary.pch
  if (is.null(col.binary.pch)) col.binary.pch <- c(16, 1)
  col.binary.lwd <- dots$col.binary.lwd
  if (is.null(col.binary.lwd)) col.binary.lwd <- 1
  col.binary.lwd <- as.numeric(col.binary.lwd)[1]
  if (!is.finite(col.binary.lwd) || col.binary.lwd <= 0) col.binary.lwd <- 1
  col.dodge <- dots$col.dodge
  if (is.null(col.dodge)) col.dodge <- 0
  col.dodge <- as.numeric(col.dodge)[1]
  if (!is.finite(col.dodge)) col.dodge <- 0
  col.var.discrete.max <- dots$col.var.discrete.max
  if (is.null(col.var.discrete.max)) col.var.discrete.max <- 10
  col.var.discrete.max <- as.numeric(col.var.discrete.max)[1]
  if (is.na(col.var.discrete.max)) col.var.discrete.max <- 10
  if (is.finite(col.var.discrete.max) && col.var.discrete.max < 0) col.var.discrete.max <- 0
  ## --- New options ---
  zero.line <- dots$zero.line
  if (is.null(zero.line)) zero.line <- TRUE
  zero.line <- as.logical(zero.line)[1]
  if (is.na(zero.line)) zero.line <- TRUE
  zero.line.col <- dots$zero.line.col
  if (is.null(zero.line.col)) zero.line.col <- "gray60"
  zero.line.lty <- dots$zero.line.lty
  if (is.null(zero.line.lty)) zero.line.lty <- 2
  zero.line.lty <- as.numeric(zero.line.lty)[1]
  if (!is.finite(zero.line.lty)) zero.line.lty <- 2
  zero.line.lwd <- dots$zero.line.lwd
  if (is.null(zero.line.lwd)) zero.line.lwd <- 1
  zero.line.lwd <- as.numeric(zero.line.lwd)[1]
  if (!is.finite(zero.line.lwd) || zero.line.lwd <= 0) zero.line.lwd <- 1
  jitter <- dots$jitter
  if (is.null(jitter)) jitter <- TRUE
  jitter.amount <- dots$jitter.amount
  if (!is.null(jitter.amount)) {
    jitter.amount <- as.numeric(jitter.amount)[1]
    if (!is.finite(jitter.amount) || jitter.amount <= 0) jitter.amount <- NULL
  }
  jitter.fraction <- dots$jitter.fraction
  if (is.null(jitter.fraction)) jitter.fraction <- 0.005
  jitter.fraction <- as.numeric(jitter.fraction)[1]
  if (!is.finite(jitter.fraction) || jitter.fraction < 0) jitter.fraction <- 0.005
  jitter.seed <- dots$jitter.seed
  if (!is.null(jitter.seed)) {
    jitter.seed <- as.integer(jitter.seed)[1]
    if (!is.finite(jitter.seed)) jitter.seed <- NULL
  }
  ## x-axis distribution strip (shows the marginal distribution of the
  ## plotted predictor 'var' along the x-axis).
  ##
  ## x.dist can be a character vector, e.g. c("hist", "rug") or c("density","rug").
  ## Supported types: "none" (default), "rug", "hist", "density", "auto".
  x.dist <- dots$x.dist
  if (is.null(x.dist)) x.dist <- dots$x.distribution
  if (is.null(x.dist)) x.dist <- "none"
  if (isTRUE(x.dist)) x.dist <- c("density", "rug")
  if (identical(x.dist, FALSE)) x.dist <- "none"
  if (is.character(x.dist)) {
    x.dist <- tolower(x.dist)
    x.dist <- unlist(strsplit(paste(x.dist, collapse = ","), "[,\\+\\s]+", perl = TRUE))
    x.dist <- x.dist[nchar(x.dist) > 0]
    x.dist[x.dist %in% c("histogram")] <- "hist"
    x.dist[x.dist %in% c("dens")] <- "density"
    x.dist <- unique(x.dist)
  } else {
    x.dist <- "none"
  }
  if (!length(x.dist)) x.dist <- "none"
  ## choose a default for "auto"
  if ("auto" %in% x.dist) {
    x.dist <- setdiff(x.dist, "auto")
    if (!length(x.dist)) {
      x.dist <- c("hist", "rug")
    } else {
      ## If the user already requested specific types, treat "auto" as adding a rug.
      x.dist <- unique(c(x.dist, "rug"))
    }
  }
  x.dist.side <- dots$x.dist.side
  if (is.null(x.dist.side)) x.dist.side <- "bottom"
  x.dist.side <- match.arg(as.character(x.dist.side), c("bottom", "top"))
  x.dist.height <- dots$x.dist.height
  if (is.null(x.dist.height)) x.dist.height <- 0.12
  x.dist.height <- as.numeric(x.dist.height)[1]
  if (!is.finite(x.dist.height) || x.dist.height < 0) x.dist.height <- 0.12
  x.dist.height <- min(x.dist.height, 0.5)
  x.dist.pad <- dots$x.dist.pad
  if (is.null(x.dist.pad)) x.dist.pad <- 0.02
  x.dist.pad <- as.numeric(x.dist.pad)[1]
  if (!is.finite(x.dist.pad) || x.dist.pad < 0) x.dist.pad <- 0.02
  x.dist.pad <- min(x.dist.pad, 0.25)
  x.dist.col <- dots$x.dist.col
  if (is.null(x.dist.col)) {
    x.dist.col <- grDevices::adjustcolor("gray70", alpha.f = 0.35)
  }
  x.dist.border <- dots$x.dist.border
  if (is.null(x.dist.border)) x.dist.border <- NA
  x.dist.lwd <- dots$x.dist.lwd
  if (is.null(x.dist.lwd)) x.dist.lwd <- 1
  x.dist.lwd <- as.numeric(x.dist.lwd)[1]
  if (!is.finite(x.dist.lwd) || x.dist.lwd < 0) x.dist.lwd <- 1
  x.dist.lty <- dots$x.dist.lty
  if (is.null(x.dist.lty)) x.dist.lty <- 1
  x.dist.lty <- as.numeric(x.dist.lty)[1]
  if (!is.finite(x.dist.lty)) x.dist.lty <- 1
  x.dist.bins <- dots$x.dist.bins
  if (is.null(x.dist.bins)) x.dist.bins <- "FD"
  x.dist.adjust <- dots$x.dist.adjust
  if (is.null(x.dist.adjust)) x.dist.adjust <- 1
  x.dist.adjust <- as.numeric(x.dist.adjust)[1]
  if (!is.finite(x.dist.adjust) || x.dist.adjust <= 0) x.dist.adjust <- 1
  x.dist.n <- dots$x.dist.n
  if (is.null(x.dist.n)) x.dist.n <- 512
  x.dist.n <- as.integer(x.dist.n)[1]
  if (!is.finite(x.dist.n) || x.dist.n < 128L) x.dist.n <- 512L
  x.dist.rug.col <- dots$x.dist.rug.col
  if (is.null(x.dist.rug.col)) {
    x.dist.rug.col <- grDevices::adjustcolor("gray20", alpha.f = 0.5)
  }
  x.dist.rug.lwd <- dots$x.dist.rug.lwd
  if (is.null(x.dist.rug.lwd)) x.dist.rug.lwd <- 0.7
  x.dist.rug.lwd <- as.numeric(x.dist.rug.lwd)[1]
  if (!is.finite(x.dist.rug.lwd) || x.dist.rug.lwd <= 0) x.dist.rug.lwd <- 0.7
  x.dist.rug.ticksize <- dots$x.dist.rug.ticksize
  if (is.null(x.dist.rug.ticksize)) x.dist.rug.ticksize <- 0.03
  x.dist.rug.ticksize <- as.numeric(x.dist.rug.ticksize)[1]
  if (!is.finite(x.dist.rug.ticksize) || x.dist.rug.ticksize < 0) x.dist.rug.ticksize <- 0.03
  x.dist.rug.ticksize <- min(x.dist.rug.ticksize, 0.2)
  x.dist.rug.max <- dots$x.dist.rug.max
  if (is.null(x.dist.rug.max)) x.dist.rug.max <- 2000
  x.dist.rug.max <- as.integer(x.dist.rug.max)[1]
  if (!is.finite(x.dist.rug.max) || x.dist.rug.max < 0L) x.dist.rug.max <- 2000L
  smooth <- dots$smooth
  if (is.null(smooth)) smooth <- TRUE
  smooth <- as.logical(smooth)[1]
  if (is.na(smooth)) smooth <- FALSE
  smooth.span <- dots$smooth.span
  if (is.null(smooth.span)) smooth.span <- 0.75
  smooth.span <- as.numeric(smooth.span)[1]
  if (!is.finite(smooth.span) || smooth.span <= 0) smooth.span <- 0.75
  smooth.degree <- dots$smooth.degree
  if (is.null(smooth.degree)) smooth.degree <- 2
  smooth.degree <- as.integer(smooth.degree)[1]
  if (!is.finite(smooth.degree) || !(smooth.degree %in% c(0L, 1L, 2L))) smooth.degree <- 2L
  smooth.family <- dots$smooth.family
  if (is.null(smooth.family)) smooth.family <- "gaussian"
  smooth.lwd <- dots$smooth.lwd
  if (is.null(smooth.lwd)) smooth.lwd <- 2
  smooth.lwd <- as.numeric(smooth.lwd)[1]
  if (!is.finite(smooth.lwd) || smooth.lwd <= 0) smooth.lwd <- 2
  smooth.lty <- dots$smooth.lty
  if (is.null(smooth.lty)) smooth.lty <- 1
  smooth.lty <- as.numeric(smooth.lty)[1]
  if (!is.finite(smooth.lty)) smooth.lty <- 1
  smooth.alpha <- dots$smooth.alpha
  if (is.null(smooth.alpha)) smooth.alpha <- 0.9
  smooth.alpha <- as.numeric(smooth.alpha)[1]
  if (!is.finite(smooth.alpha) || smooth.alpha < 0 || smooth.alpha > 1) smooth.alpha <- 0.9
  smooth.min.n <- dots$smooth.min.n
  if (is.null(smooth.min.n)) smooth.min.n <- 30
  smooth.min.n <- as.integer(smooth.min.n)[1]
  if (!is.finite(smooth.min.n) || smooth.min.n < 3L) smooth.min.n <- 30L
  smooth.n.grid <- dots$smooth.n.grid
  if (is.null(smooth.n.grid)) smooth.n.grid <- 200
  smooth.n.grid <- as.integer(smooth.n.grid)[1]
  if (!is.finite(smooth.n.grid) || smooth.n.grid < 50L) smooth.n.grid <- 200L
  col.legend.probs <- dots$col.legend.probs
  col.legend.n <- dots$col.legend.n
  if (is.null(col.legend.probs) && !is.null(col.legend.n)) {
    nn <- as.integer(col.legend.n)[1]
    if (is.finite(nn) && nn >= 2L) {
      col.legend.probs <- seq(0.05, 0.95, length.out = nn)
    }
  }
  if (is.null(col.legend.probs)) {
    col.legend.probs <- c(0.05, 0.25, 0.5, 0.75, 0.95)
  }
  col.legend.probs <- as.numeric(col.legend.probs)
  col.legend.probs <- col.legend.probs[is.finite(col.legend.probs) &
                                         col.legend.probs >= 0 & col.legend.probs <= 1]
  col.legend.probs <- sort(unique(col.legend.probs))
  if (!length(col.legend.probs)) col.legend.probs <- c(0.05, 0.5, 0.95)
  smooth.probs <- dots$smooth.probs
  smooth.n <- dots$smooth.n
  if (is.null(smooth.probs) && !is.null(smooth.n)) {
    nn <- as.integer(smooth.n)[1]
    if (is.finite(nn) && nn >= 2L) {
      smooth.probs <- seq(0.05, 0.95, length.out = nn)
    }
  }
  if (is.null(smooth.probs)) smooth.probs <- col.legend.probs
  smooth.probs <- as.numeric(smooth.probs)
  smooth.probs <- smooth.probs[is.finite(smooth.probs) & smooth.probs >= 0 & smooth.probs <= 1]
  smooth.probs <- sort(unique(smooth.probs))
  ## Remove our custom args so graphics::plot() doesn't see them.
  dots$col.style <- dots$col.outline <- dots$col.outline.lwd <- NULL
  dots$col.binary.pch <- dots$col.binary.lwd <- NULL
  dots$col.dodge <- NULL
  dots$col.var.discrete.max <- NULL
  dots$zero.line <- dots$zero.line.col <- dots$zero.line.lty <- dots$zero.line.lwd <- NULL
  dots$jitter <- dots$jitter.amount <- dots$jitter.fraction <- dots$jitter.seed <- NULL
  dots$x.dist <- dots$x.distribution <- dots$x.dist.side <- dots$x.dist.height <- dots$x.dist.pad <- NULL
  dots$x.dist.col <- dots$x.dist.border <- dots$x.dist.lwd <- dots$x.dist.lty <- NULL
  dots$x.dist.bins <- dots$x.dist.adjust <- dots$x.dist.n <- NULL
  dots$x.dist.rug.col <- dots$x.dist.rug.lwd <- dots$x.dist.rug.ticksize <- dots$x.dist.rug.max <- NULL
  dots$smooth <- dots$smooth.span <- dots$smooth.degree <- dots$smooth.family <- NULL
  dots$smooth.lwd <- dots$smooth.lty <- dots$smooth.alpha <- NULL
  dots$smooth.min.n <- dots$smooth.n.grid <- NULL
  dots$col.legend.probs <- dots$col.legend.n <- NULL
  dots$smooth.probs <- dots$smooth.n <- NULL
  ## We draw points ourselves, so suppress plot() points.
  if (!is.null(dots$type)) dots$type <- NULL
  ivar_is_list <- is.list(ivar) && !inherits(ivar, "data.frame")
  ## special handling for multivariate / multiclass ivarpro output
  if (ivar_is_list) {
    if (is.null(data) && !is.null(attr(ivar, "data"))) {
      data <- attr(ivar, "data")
    }
    ivar <- .ivarpro_select_target(ivar, target = target, warn = TRUE, caller = "plot.ivarpro")
  }
  ## resolve feature matrix: also add y if possible
  if (is.null(data) && !is.null(attr(ivar, "data"))) {
    data <- attr(ivar, "data")
  }
  if (is.null(data)) {
    stop("need to supply data from the original varpro call")
  }
  if (!(is.data.frame(ivar) || is.matrix(ivar)) ||
      !(is.data.frame(data) || is.matrix(data)) ||
      nrow(ivar) != nrow(data)) {
    stop("x and data must be data frames or matrices with the same number of rows")
  }
  if (is.character(var) && length(var) == 1L && !is.na(var)) {
    var_name <- var
  } else if (is.numeric(var) && length(var) == 1L && is.finite(var) &&
             var == floor(var) && var >= 1 && var <= ncol(ivar)) {
    var_name <- colnames(ivar)[as.integer(var)]
  } else {
    stop("var must be a single variable name or column index")
  }
  if (is.null(var_name) || !(var_name %in% colnames(ivar))) {
    stop("Could not resolve 'var' in ivar columns.")
  }
  if (!(var_name %in% colnames(data))) {
    stop("Plotting requires that 'data' contains the plotted variable.")
  }
  xv <- data[, var_name, drop = TRUE]
  yv <- ivar[, var_name, drop = TRUE]
  if (!is.numeric(xv) || !is.numeric(yv)) {
    stop("The plotted predictor and gradient must be numeric")
  }
  if (!any(is.finite(xv) & is.finite(yv))) {
    stop("No finite predictor/gradient pairs for variable: ", var_name)
  }
  ## colors
  col_pt <- rep("black", length(yv))
  col_grp <- NULL
  col_legend <- NULL
  cv <- NULL
  if (!is.null(col.var)) {
    if (!(col.var %in% colnames(data))) stop("col.var not found in data.")
    cv <- data[, col.var]
    ## If col.var is numeric but has only a few distinct values, it's usually
    ## more readable to treat it as categorical (factor-like) rather than
    ## forcing a continuous colour gradient.
    treat_as_factor <- FALSE
    if (is.numeric(cv)) {
      u <- unique(cv[is.finite(cv)])
      n_unique <- length(u)
      if (is.infinite(col.var.discrete.max) ||
          (is.finite(col.var.discrete.max) && n_unique <= col.var.discrete.max)) {
        treat_as_factor <- TRUE
      }
    }
    if (is.numeric(cv) && !treat_as_factor) {
      ## continuous colour ramp
      pal <- grDevices::colorRampPalette(c("navy", "skyblue", "gold", "firebrick"))(100)
      rng <- range(cv, na.rm = TRUE)
      if (is.finite(rng[1]) && is.finite(rng[2]) && rng[2] > rng[1]) {
        z <- (cv - rng[1]) / (rng[2] - rng[1])
        k <- pmax(1L, pmin(100L, 1L + floor(99 * z)))
        col_pt <- pal[k]
        col_legend <- list(type = "numeric", rng = rng, pal = pal, var = col.var)
      }
    } else {
      ## categorical palette
      if (is.numeric(cv)) {
        lev_num <- sort(unique(cv[is.finite(cv)]))
        ff <- factor(cv, levels = lev_num)
      } else {
        ff <- as.factor(cv)
      }
      lev <- levels(ff)
      pal <- grDevices::rainbow(length(lev))
      col_grp <- as.integer(ff)
      col_pt  <- pal[col_grp]
      col_legend <- list(type = "factor", lev = lev, pal = pal, var = col.var)
    }
  }
  ## sizes
  cex_pt <- rep(cex, length(yv))
  if (!is.null(size.var)) {
    if (!(size.var %in% colnames(data))) stop("size.var not found in data.")
    sv <- data[, size.var]
    if (!is.numeric(sv)) sv <- as.numeric(sv)
    rng <- range(sv, na.rm = TRUE)
    if (is.finite(rng[1]) && is.finite(rng[2]) && rng[2] > rng[1]) {
      z <- (sv - rng[1]) / (rng[2] - rng[1])
      cex_pt <- cex.range[1] + z * (cex.range[2] - cex.range[1])
    }
  }
  ## finite plotting set
  ok <- is.finite(xv) & is.finite(yv)
  xv <- xv[ok]
  yv <- yv[ok]
  col_pt <- col_pt[ok]
  if (!is.null(col_grp)) col_grp <- col_grp[ok]
  cex_pt <- cex_pt[ok]
  if (!is.null(cv)) cv <- cv[ok]
  ## optional: small horizontal dodge when colouring by a factor (helps
  ## discrete x + heavy overlap). col.dodge is interpreted as a fraction
  ## of the x-range (e.g. 0.01 means 1% of range).
  x_plot0 <- xv
  if (!is.null(col_grp) && is.finite(col.dodge) && col.dodge != 0) {
    xr <- range(xv, finite = TRUE)
    dx <- col.dodge * diff(xr)
    nlev <- if (!is.null(col_legend) && col_legend$type == "factor") length(col_legend$lev) else 0L
    if (is.finite(dx) && dx != 0 && nlev > 1L) {
      ctr <- (nlev + 1) / 2
      x_plot0 <- xv + (col_grp - ctr) * dx
    }
  }
  ## optional: horizontal jitter (applied after dodge)
  x_plot <- x_plot0
  jitter_is_numeric <- is.numeric(jitter) && length(jitter) == 1L && is.finite(jitter)
  jitter_is_true <- isTRUE(jitter)
  if (jitter_is_numeric || jitter_is_true) {
    if (jitter_is_numeric) {
      jitter.amount <- abs(as.numeric(jitter)[1])
    }
    if (is.null(jitter.amount)) {
      xr <- range(x_plot0, finite = TRUE)
      jitter.amount <- jitter.fraction * diff(xr)
    }
    if (is.finite(jitter.amount) && jitter.amount > 0) {
      if (!is.null(jitter.seed)) set.seed(jitter.seed)
      x_plot <- x_plot0 + stats::runif(length(x_plot0), -jitter.amount, jitter.amount)
    }
  }
  ## plot
  if (is.null(main)) main <- paste0(var_name, " vs iVarPro gradient")
  if (is.null(xlab)) xlab <- var_name
  ## Draw axes first, then distribution strips, reference line, and points.
  do.call(graphics::plot,
          c(list(x = x_plot, y = yv,
                 xlab = xlab, ylab = ylab,
                 main = main,
                 type = "n"),
            dots))
  ## x-axis distribution strip: uses the original (unjittered, undodged) xv
  ## values for the cases that are actually plotted.
  ## We precompute geometry once and draw in two passes:
  ##   (1) fill behind points, (2) outline/rug on top so it stays visible.
  x_dist_obj <- NULL
  if (!("none" %in% x.dist)) {
    x_dist_vals <- xv[is.finite(xv)]
    if (length(x_dist_vals) >= 2L && length(unique(x_dist_vals)) >= 2L) {
      usr <- graphics::par("usr")
      dy <- diff(usr[3:4])
      if (is.finite(dy) && dy > 0) {
        h <- x.dist.height * dy
        pad <- x.dist.pad * dy
        if (is.finite(h) && h > 0) {
          if (x.dist.side == "bottom") {
            base <- usr[3] + pad
            sign <- 1
          } else {
            base <- usr[4] - pad
            sign <- -1
          }
          x_dist_obj <- list(base = base, sign = sign, h = h, dy = dy,
                             vals = x_dist_vals,
                             hist = NULL, dens = NULL, rug = NULL)
          ## histogram (scaled to strip height)
          if ("hist" %in% x.dist) {
            hh <- tryCatch(
              graphics::hist(x_dist_vals, breaks = x.dist.bins, plot = FALSE),
              error = function(e) NULL
            )
            if (!is.null(hh) && length(hh$counts)) {
              cc <- hh$counts
              mx <- max(cc, na.rm = TRUE)
              if (is.finite(mx) && mx > 0) {
                y_top <- base + sign * (cc / mx) * h
                x_left <- hh$breaks[-length(hh$breaks)]
                x_right <- hh$breaks[-1]
                nz <- which(cc > 0 & is.finite(y_top) & is.finite(x_left) & is.finite(x_right))
                ## outline as a step function (includes empty bins as flat at base)
                m <- length(cc)
                x_outline <- y_outline <- NULL
                if (m >= 1L && length(hh$breaks) == (m + 1L)) {
                  br <- hh$breaks
                  if (m == 1L) {
                    x_outline <- c(br[1], br[1], br[2], br[2])
                    y_outline <- c(base, y_top[1], y_top[1], base)
                  } else {
                    x_outline <- c(br[1], br[1], rep(br[2:m], each = 2), br[m + 1L], br[m + 1L])
                    y_outline <- c(base, rep(y_top, each = 2), base)
                  }
                }
                x_dist_obj$hist <- list(x_left = x_left, x_right = x_right,
                                        y_top = y_top, nz = nz,
                                        x_outline = x_outline, y_outline = y_outline)
              }
            }
          }
          ## density (scaled to strip height)
          if ("density" %in% x.dist) {
            dd <- tryCatch(
              stats::density(x_dist_vals, adjust = x.dist.adjust, n = x.dist.n),
              error = function(e) NULL
            )
            if (!is.null(dd) && length(dd$x) && length(dd$y)) {
              mx <- max(dd$y, na.rm = TRUE)
              if (is.finite(mx) && mx > 0) {
                y_curve <- base + sign * (dd$y / mx) * h
                dens_fill <- !("hist" %in% x.dist)
                x_dist_obj$dens <- list(x = dd$x, y_curve = y_curve, fill = dens_fill)
              }
            }
          }
          ## rug ticks (optionally thinned)
          if ("rug" %in% x.dist && x.dist.rug.ticksize > 0) {
            xx <- x_dist_vals
            if (length(xx) > x.dist.rug.max && x.dist.rug.max > 0L) {
              ord <- order(xx)
              take <- ord[round(seq(1, length(xx), length.out = x.dist.rug.max))]
              xx <- xx[take]
            }
            tick <- min(h, x.dist.rug.ticksize * dy)
            if (is.finite(tick) && tick > 0) {
              x_dist_obj$rug <- list(x = xx, tick = tick)
            }
          }
        }
      }
    }
  }
  ## x-axis distribution strip (fill pass behind points)
  if (!is.null(x_dist_obj)) {
    base <- x_dist_obj$base
    sign <- x_dist_obj$sign
    ## histogram fill
    if (!is.null(x_dist_obj$hist) && length(x_dist_obj$hist$nz)) {
      nz <- x_dist_obj$hist$nz
      y_top <- x_dist_obj$hist$y_top[nz]
      x_left <- x_dist_obj$hist$x_left[nz]
      x_right <- x_dist_obj$hist$x_right[nz]
      y0 <- pmin(base, y_top)
      y1 <- pmax(base, y_top)
      graphics::rect(x_left, y0, x_right, y1,
                     col = x.dist.col,
                     border = NA)
    }
    ## density fill (only when no histogram is drawn)
    if (!is.null(x_dist_obj$dens) && isTRUE(x_dist_obj$dens$fill)) {
      xx <- x_dist_obj$dens$x
      yy <- x_dist_obj$dens$y_curve
      graphics::polygon(c(xx, rev(xx)),
                        c(rep(base, length(xx)), rev(yy)),
                        col = x.dist.col,
                        border = NA)
    }
  }
  ## reference line at gradient = 0
  if (isTRUE(zero.line)) {
    graphics::abline(h = 0, col = zero.line.col, lty = zero.line.lty, lwd = zero.line.lwd)
  }
  ## points
  style_final <- col.style
  if (identical(style_final, "auto")) {
    if (!is.null(col_legend) && identical(col_legend$type, "factor")) {
      style_final <- if (length(col_legend$lev) == 2L) "binary" else "outline"
    } else {
      style_final <- "solid"
    }
  }
  if (identical(style_final, "outline")) {
    ## filled points with a dark border (keeps colours strong without alpha)
    graphics::points(x_plot, yv,
                     pch = 21,
                     bg  = col_pt,
                     col = col.outline,
                     cex = cex_pt,
                     lwd = col.outline.lwd)
  } else if (identical(style_final, "binary") &&
             !is.null(col_grp) &&
             !is.null(col_legend) &&
             identical(col_legend$type, "factor") &&
             length(col_legend$lev) == 2L) {
    ## 2-level colouring: draw one group with a filled symbol and the other
    ## with an open/line symbol so overlapping points remain discernible.
    pch_map <- col.binary.pch
    if (length(pch_map) < 2L) pch_map <- rep(pch_map[1], 2L)
    if (length(pch_map) > 2L) pch_map <- pch_map[1:2]
    is_open_symbol <- function(p) {
      is.finite(p) && p >= 0 && p <= 14
    }
    open_flag <- vapply(pch_map, is_open_symbol, logical(1))
    ## Prefer: filled first, open second. If unclear, draw the larger group first.
    if (sum(open_flag) == 1L) {
      grp_order <- c(which(!open_flag), which(open_flag))
    } else {
      tab <- tabulate(col_grp, nbins = 2L)
      grp_order <- order(tab, decreasing = TRUE)
    }
    for (g in grp_order) {
      idx <- which(col_grp == g)
      if (!length(idx)) next
      pg <- pch_map[g]
      if (pg %in% 21:25) {
        graphics::points(x_plot[idx], yv[idx],
                         pch = pg,
                         bg  = col_pt[idx],
                         col = col.outline,
                         cex = cex_pt[idx],
                         lwd = col.outline.lwd)
      } else {
        graphics::points(x_plot[idx], yv[idx],
                         pch = pg,
                         col = col_pt[idx],
                         cex = cex_pt[idx],
                         lwd = col.binary.lwd)
      }
    }
  } else {
    ## default/legacy behaviour
    graphics::points(x_plot, yv, pch = pch, col = col_pt, cex = cex_pt)
  }
  ## smooth curves (loess), drawn on top of points so they're easy to see
  if (isTRUE(smooth)) {
    draw_loess <- function(xg, yg, col_line) {
      if (length(xg) < smooth.min.n) return(invisible(FALSE))
      if (length(unique(xg[is.finite(xg)])) < 2L) return(invisible(FALSE))
      df <- data.frame(x = xg, y = yg)
      fit <- tryCatch(
        suppressWarnings(stats::loess(y ~ x, data = df,
                                     span = smooth.span,
                                     degree = smooth.degree,
                                     family = smooth.family,
                                     na.action = stats::na.exclude)),
        error = function(e) NULL
      )
      if (is.null(fit)) return(invisible(FALSE))
      xr <- range(xg, finite = TRUE)
      if (!is.finite(xr[1]) || !is.finite(xr[2]) || xr[2] <= xr[1]) return(invisible(FALSE))
      xseq <- seq(xr[1], xr[2], length.out = smooth.n.grid)
      yhat <- tryCatch(
        suppressWarnings(stats::predict(fit, newdata = data.frame(x = xseq))),
        error = function(e) rep(NA_real_, length(xseq))
      )
      okp <- is.finite(yhat) & is.finite(xseq)
      if (sum(okp) < 2L) return(invisible(FALSE))
      col_line <- grDevices::adjustcolor(col_line, alpha.f = smooth.alpha)
      graphics::lines(xseq[okp], yhat[okp], col = col_line, lwd = smooth.lwd, lty = smooth.lty)
      invisible(TRUE)
    }
    ## use unjittered x for smoothing (keeps the trend stable)
    x_smooth <- x_plot0
    if (is.null(col_legend) || is.null(col.var)) {
      ## no grouping var: draw a single smooth
      draw_loess(x_smooth, yv, "black")
    } else if (identical(col_legend$type, "factor") && !is.null(col_grp)) {
      ## one smooth per factor level
      nlev <- length(col_legend$lev)
      for (g in seq_len(nlev)) {
        idx <- which(col_grp == g)
        if (!length(idx)) next
        draw_loess(x_smooth[idx], yv[idx], col_legend$pal[g])
      }
    } else if (identical(col_legend$type, "numeric") && is.numeric(cv)) {
      ## numeric col.var: define strata around requested quantiles
      probs_use <- smooth.probs
      if (!length(probs_use)) probs_use <- col.legend.probs
      qv <- tryCatch(
        stats::quantile(cv, probs = probs_use, na.rm = TRUE, names = FALSE),
        error = function(e) rep(NA_real_, length(probs_use))
      )
      qv <- qv[is.finite(qv)]
      qv <- sort(unique(as.numeric(qv)))
      if (length(qv) == 0L) {
        ## cannot stratify; fall back to one smooth
        draw_loess(x_smooth, yv, "black")
      } else {
        ## group assignment by midpoints between quantile values
        if (length(qv) == 1L) {
          grp <- rep(1L, length(cv))
        } else {
          mids <- (qv[-1] + qv[-length(qv)]) / 2
          br <- c(-Inf, mids, Inf)
          grp <- suppressWarnings(cut(cv, breaks = br, labels = FALSE, include.lowest = TRUE))
        }
        ## map a numeric value to the same palette used for points
        map_to_pal <- function(val) {
          rng <- col_legend$rng
          if (!is.finite(rng[1]) || !is.finite(rng[2]) || rng[2] <= rng[1]) return("black")
          z <- (val - rng[1]) / (rng[2] - rng[1])
          if (!is.finite(z)) z <- 0.5
          z <- max(0, min(1, z))
          kk <- 1L + floor(99 * z)
          kk <- max(1L, min(100L, kk))
          col_legend$pal[kk]
        }
        for (g in seq_len(length(qv))) {
          idx <- which(grp == g & is.finite(cv))
          if (!length(idx)) next
          draw_loess(x_smooth[idx], yv[idx], map_to_pal(qv[g]))
        }
      }
    }
  }
  ## x-axis distribution strip (outline + rug pass on top)
  if (!is.null(x_dist_obj)) {
    base <- x_dist_obj$base
    sign <- x_dist_obj$sign
    line_col <- x.dist.border
    if (is.na(line_col)[1]) {
      line_col <- grDevices::adjustcolor("gray30", alpha.f = 0.8)
    }
    ## histogram outline (step)
    if (!is.null(x_dist_obj$hist) &&
        !is.null(x_dist_obj$hist$x_outline) &&
        !is.null(x_dist_obj$hist$y_outline) &&
        is.finite(x.dist.lwd) && x.dist.lwd > 0) {
      graphics::lines(x_dist_obj$hist$x_outline,
                      x_dist_obj$hist$y_outline,
                      col = line_col,
                      lwd = x.dist.lwd,
                      lty = x.dist.lty)
    }
    ## density outline (always drawn if requested)
    if (!is.null(x_dist_obj$dens) &&
        is.finite(x.dist.lwd) && x.dist.lwd > 0) {
      graphics::lines(x_dist_obj$dens$x,
                      x_dist_obj$dens$y_curve,
                      col = line_col,
                      lwd = x.dist.lwd,
                      lty = x.dist.lty)
    }
    ## rug
    if (!is.null(x_dist_obj$rug)) {
      xx <- x_dist_obj$rug$x
      tick <- x_dist_obj$rug$tick
      graphics::segments(xx, base, xx, base + sign * tick,
                         col = x.dist.rug.col,
                         lwd = x.dist.rug.lwd)
    }
  }
  ## legend
  if (isTRUE(legend) && !is.null(col_legend)) {
    if (col_legend$type == "factor") {
      if (identical(style_final, "outline")) {
        graphics::legend("topright",
                         legend = col_legend$lev,
                         pch = 21,
                         pt.bg = col_legend$pal,
                         col = col.outline,
                         pt.lwd = col.outline.lwd,
                         bty = "n",
                         title = col_legend$var)
      } else if (identical(style_final, "binary") && length(col_legend$lev) == 2L) {
        pch_map <- col.binary.pch
        if (length(pch_map) < 2L) pch_map <- rep(pch_map[1], 2L)
        if (length(pch_map) > 2L) pch_map <- pch_map[1:2]
        is_fill_bg <- pch_map %in% 21:25
        col_leg    <- ifelse(is_fill_bg, col.outline, col_legend$pal)
        bg_leg     <- ifelse(is_fill_bg, col_legend$pal, NA)
        lwd_leg    <- ifelse(is_fill_bg, col.outline.lwd, col.binary.lwd)
        graphics::legend("topright",
                         legend = col_legend$lev,
                         pch = pch_map,
                         col = col_leg,
                         pt.bg = bg_leg,
                         pt.lwd = lwd_leg,
                         bty = "n",
                         title = col_legend$var)
      } else {
        graphics::legend("topright",
                         legend = col_legend$lev,
                         col = col_legend$pal, pch = 16, bty = "n",
                         title = col_legend$var)
      }
    } else if (col_legend$type == "numeric") {
      ## numeric legend using configurable quantiles
      probs <- col.legend.probs
      qs <- tryCatch(
        stats::quantile(cv, probs = probs, na.rm = TRUE),
        error = function(e) rep(NA_real_, length(probs))
      )
      qs <- qs[is.finite(qs)]
      if (length(qs)) {
        kk <- pmax(1L, pmin(100L, 1L + floor(99 * (qs - col_legend$rng[1]) /
                                             (col_legend$rng[2] - col_legend$rng[1]))))
        if (identical(style_final, "outline")) {
          graphics::legend("topright",
                           legend = sprintf("%s = %.3g", names(qs), as.numeric(qs)),
                           pch = 21,
                           pt.bg = col_legend$pal[kk],
                           col = col.outline,
                           pt.lwd = col.outline.lwd,
                           bty = "n",
                           title = col_legend$var)
        } else {
          graphics::legend("topright",
                           legend = sprintf("%s = %.3g", names(qs), as.numeric(qs)),
                           col = col_legend$pal[kk],
                           pch = 16, bty = "n",
                           title = col_legend$var)
        }
      }
    }
  }
  invisible(TRUE)
}
