plot.partialpro <- function(x, xvar.names, nvar,
        parametric = FALSE, se = TRUE,
        causal = FALSE, subset = NULL,
        plot.it = TRUE, ...) {
  ## ------------------------------------------------------------------------
  ##
  ## initial processing
  ##
  ## ------------------------------------------------------------------------
  ## Resolve positions to predictor names before applying the plot limit.
  if (!is.list(x) || is.data.frame(x)) {
    stop("x must be a partialpro object", call. = FALSE)
  }
  if (missing(xvar.names)) {
    xvar.names <- names(x)
  } else if (is.numeric(xvar.names)) {
    if (anyNA(xvar.names) || any(!is.finite(xvar.names)) ||
        any(xvar.names != floor(xvar.names)) ||
        any(xvar.names < 1L | xvar.names > length(x))) {
      stop("xvar.names indices must be positions in x", call. = FALSE)
    }
    xvar.names <- names(x)[xvar.names]
  } else if (!is.character(xvar.names) || anyNA(xvar.names)) {
    stop("xvar.names must contain predictor names or integer positions", call. = FALSE)
  }
  if (!missing(nvar)) {
    if (!is.numeric(nvar) || length(nvar) != 1L || is.na(nvar) ||
        nvar < 0 || (is.finite(nvar) && nvar != floor(nvar))) {
      stop("nvar must be a nonnegative integer or Inf", call. = FALSE)
    }
    xvar.names <- xvar.names[seq_len(min(length(xvar.names), nvar))]
  }
  unavailable <- setdiff(xvar.names, names(x))
  if (length(unavailable)) {
    warning("plot.partialpro(): skipping unavailable predictors: ",
            paste(unavailable, collapse = ", "), call. = FALSE)
    xvar.names <- xvar.names[xvar.names %in% names(x)]
  }
  if (!length(xvar.names)) return(invisible(NULL))
  o <- x[xvar.names]
  ## Remove method-specific controls before forwarding graphics arguments.
  dots <- list(...)
  weights.power <- if (!is.null(dots$weights.power)) dots$weights.power else 10
  weights.tolerance <- if (!is.null(dots$weights.tolerance)) dots$weights.tolerance else 1e-6
  if (!is.numeric(weights.power) || length(weights.power) != 1L ||
      !is.finite(weights.power) || weights.power < 0 ||
      !is.numeric(weights.tolerance) || length(weights.tolerance) != 1L ||
      !is.finite(weights.tolerance) || weights.tolerance < 0) {
    stop("smoothing weights require finite, nonnegative controls", call. = FALSE)
  }
  dots$weights.power <- dots$weights.tolerance <- NULL
  dots.base <- dots
  ## Calculate column summaries without treating unavailable profiles as zero.
  .profile.mean <- function(z) {
    z[!is.finite(z)] <- NA_real_
    ans <- colMeans(z, na.rm = TRUE)
    ans[!is.finite(ans)] <- NA_real_
    ans
  }
  .profile.se <- function(z, frequency) {
    z[!is.finite(z)] <- NA_real_
    nfinite <- colSums(!is.na(z))
    ysd <- apply(z, 2L, sd, na.rm = TRUE)
    ok <- nfinite >= 2L & is.finite(ysd) & frequency > 0
    ans <- setNames(rep(NA_real_, ncol(z)), colnames(z))
    ## Preserve the existing small-spread floor for estimable columns.
    if (any(ok) && all(ysd[ok] <= 1e-10)) ysd[ok] <- 1e-10
    ans[ok] <- ysd[ok] / sqrt(frequency[ok])
    ans[!is.finite(ans)] <- NA_real_
    ans
  }
  ## User graphics settings replace defaults without duplicate arguments.
  .plot.args <- function(defaults, supplied) {
    for (nm in names(supplied)) defaults[nm] <- supplied[nm]
    defaults
  }
  ## ------------------------------------------------------------------------
  ## Summarize and display each requested predictor.
  ## ------------------------------------------------------------------------
  rO <- lapply(seq_along(xvar.names), function(j) {
    ## failure checks
    if (is.null(o[[j]])) {
      return(NULL)
    }
    ## local copy of graphical arguments for this variable
    dots <- dots.base
    ## extract necessary items
    case <- o[[j]]$case
    xorg <- o[[j]]$xorg
    nxorg <- length(unique(xorg))
    xvirtual <- o[[j]]$xvirtual
    goodvt <- o[[j]]$goodvt
    yhat.par <- o[[j]]$yhat.par
    yhat.nonpar <- o[[j]]$yhat.nonpar
    yhat.causal <- o[[j]]$yhat.causal
    ## is this continuous or binary?
    binary.variable <- nxorg == 2
    ## determine type of plot
    if (!parametric || causal) {
      if (!causal) {
        type <- "nonparametric"
      }
      else {
        type <- "causal"
      }
    }
    else {
      type <- "parametric"      
    }
    ## Grouping and row selection apply to all three profile summaries.
    ##---------------------------------------------------
    ##
    ## conditional analysis?
    ##
    ##---------------------------------------------------
    ## user specified conditioning 
    if (!is.null(subset) && is.factor(subset)) {
      ## identify cases
      idx.lst <- lapply(levels(subset), function(lv) {
        idx <- intersect(which(subset == lv), case)
        if (length(idx) == 0L) {
          return(NULL)
        }
        which(case %in% idx)
      })
      names(idx.lst) <- levels(subset)
      idx.lst <- idx.lst[!vapply(idx.lst, is.null, logical(1))]
      if (length(idx.lst) == 0) {
        return(NULL)
      }
      cflag <- TRUE
    }
    ## no conditioning done
    else {
      idx.lst <- list()
      cflag <- FALSE
      ## default case: no subsetting
      if (is.null(subset)) {
        idx.lst[[1]] <- seq_along(case)
      }
      ## user has specified a non-standard subset
      else {
        ##process subset
        if (is.logical(subset)) {
          idx <- which(subset)
        }
        else if (is.numeric(subset)) {
          idx <- subset
        }
        else {
          stop("subset not set correctly\n")
        }
        ## confirm there is enough data
        if (length(intersect(idx, case)) == 0) {
          return(NULL)
        }
        ## match idx to cases
        idx.lst[[1]] <- which(case %in% idx)
      }
    }
    ##---------------------------------------------------
    ##
    ## ESTIMATION+STANDARD ERRORS: continuous variables
    ##
    ##---------------------------------------------------
    if (!binary.variable) {
      plotO <- lapply(idx.lst, function(sub) {
        if (!length(sub)) return(NULL)
        ## Keep the support counts two-dimensional even for a single case.
        frq <- colSums(!is.na(goodvt[sub, , drop = FALSE]))
        if (!length(frq) || !any(frq > 0)) return(NULL)
        weights <- (frq / max(frq)) ^ weights.power
        pt.support <- is.finite(weights) & weights > weights.tolerance
        if (!any(pt.support)) return(NULL)
        ## Preserve the original SE source for the polynomial display.
        ysrc <- if (type == "causal") yhat.causal else yhat.nonpar
        y.se <- if (se) .profile.se(ysrc[sub, , drop = FALSE], frq) else 0
        if (type == "nonparametric" || type == "causal") {
          avg <- .profile.mean(ysrc[sub, , drop = FALSE])
          pt.tolerance <- pt.support & is.finite(xvirtual) & is.finite(avg)
          if (!any(pt.tolerance)) return(NULL)
          loessControl <- loess.control(
            trace.hat = if (length(sub) > 500) "approximate" else "exact"
          )
          o.loess <- tryCatch({
            suppressWarnings(loess(
              y ~ x,
              data.frame(y = avg, x = xvirtual)[pt.tolerance, , drop = FALSE],
              weights = weights[pt.tolerance], control = loessControl
            ))
          }, error = function(ex) NULL)
          if (is.null(o.loess)) return(NULL)
          x <- as.numeric(o.loess$x)
          y <- o.loess$fitted
          if (se) y.se <- y.se[pt.tolerance]
        } else {
          x <- xvirtual
          y <- .profile.mean(yhat.par[sub, , drop = FALSE])
        }
        if (!any(is.finite(x) & is.finite(y))) return(NULL)
        y[!is.finite(y)] <- NA_real_
        list(x = x, y = y, y.se = y.se)
      })
      names(plotO) <- names(idx.lst)
    } else {
      ## Binary variables: summarize the predictions at the two values.
      plotO <- lapply(idx.lst, function(sub) {
        if (!length(sub)) return(NULL)
        ysrc <- if (type == "causal") yhat.causal else yhat.nonpar
        z <- ysrc[sub, , drop = FALSE]
        frq <- colSums(is.finite(z))
        y <- .profile.mean(z)
        if (!any(is.finite(y))) return(NULL)
        y.se <- if (se) .profile.se(z, frq) else 0
        list(x = xvirtual, y = y, y.se = y.se)
      })
      names(plotO) <- names(idx.lst)
    }
    ##---------------------------------------------------
    ##
    ## remove NULL entries: exit if nothing 
    ##
    ##---------------------------------------------------
    plotO <- plotO[!vapply(plotO, is.null, logical(1))]
    if (length(plotO) == 0) {
      return(NULL)
    }
    if (plot.it) {
      ##---------------------------------------------------
      ##
      ## PLOTS: graphical options
      ##
      ##---------------------------------------------------
      if (!is.null(dots$nmax)) {
        nmax <- dots$nmax[min(length(dots$nmax), j)]
      } else {
        nmax <- 250
      }
      if (!is.numeric(nmax) || length(nmax) != 1L || is.na(nmax) ||
          nmax < 0 || (is.finite(nmax) && nmax != floor(nmax))) {
        stop("nmax must contain nonnegative integers or Inf", call. = FALSE)
      }
      dots$nmax <- NULL
      if (is.null(dots$ylab)) {
        dots$ylab <- if (type != "causal") "partial effect" else "change from baseline"
      } else {
        dots$ylab <- dots$ylab[min(length(dots$ylab), j)]
      }
      if (is.null(dots$xlab)) {
        dots$xlab <- xvar.names[j]
      } else {
        dots$xlab <- dots$xlab[min(length(dots$xlab), j)]
      }
      if (is.list(dots$ylim)) {
        dots$ylim <- dots$ylim[[min(length(dots$ylim), j)]]
      }
      if (is.null(dots$ylim) && !binary.variable) {
        yy <- unlist(lapply(plotO, function(oo) {
          ## Missing SEs do not hide an otherwise available mean curve.
          serr <- oo$y.se
          serr[!is.finite(serr)] <- 0
          c(oo$y - 2 * serr, oo$y + 2 * serr)
        }))
        yy <- yy[is.finite(yy)]
        if (!length(yy)) return(NULL)
        dots$ylim <- range(yy)
      }
      ##---------------------------------------------------
      ##
      ## PLOTS: smoothed plot for continuous case
      ##
      ##---------------------------------------------------
      if (!binary.variable) {
        ## form long vector of x and y 
        x <- unlist(lapply(plotO, function(oo){oo$x}))
        y <- unlist(lapply(plotO, function(oo){oo$y}))
        ## Generate axes; the mean and guides are added below.
        dots$x <- x
        dots$y <- y
        dots$type <- "n"
        do.call(plot, dots)
        if (cflag) {
          nullO <- lapply(seq_along(plotO), function(j) {
            oo <- plotO[[j]]
            lines(oo$x, oo$y, col = j, lwd = if (is.null(dots$lwd)) 1.5 else dots$lwd)
            if (se) {
              lines(oo$x, oo$y + 2 * oo$y.se, lty = 3, col = j)
              lines(oo$x, oo$y - 2 * oo$y.se, lty = 3, col = j)
            }
          })
        }
        else {
          lines(plotO[[1]]$x, plotO[[1]]$y, col = 1, lwd = if (is.null(dots$lwd)) 1.5 else dots$lwd)
          if (se) {
            lines(plotO[[1]]$x, plotO[[1]]$y + 2 * plotO[[1]]$y.se, lty = 3, col = 2)
            lines(plotO[[1]]$x, plotO[[1]]$y - 2 * plotO[[1]]$y.se, lty = 3, col = 2)
          }
        }
        if (nxorg > nmax) {
          suppressWarnings(rug(sample(xorg, size = nmax, replace = FALSE), ticksize = 0.03))
        }
        else {
          suppressWarnings(rug(xorg, ticksize = 0.03))
        }
        if (cflag) {
          legend("topright", legend = names(plotO), fill = seq_along(plotO))
        }
      }
      ##---------------------------------------------------
      ##
      ## PLOTS: boxplot for binary case
      ##
      ##---------------------------------------------------
      else {
        ## Build one box from each mean and its two SE limits. Keep the
        ## predictor labels and group identities separate from box names.
        boxes <- list()
        box.labels <- character()
        box.groups <- integer()
        for (g in seq_along(plotO)) {
          oo <- plotO[[g]]
          positions <- seq_along(oo$x)
          if (!cflag && type == "causal" && length(positions) > 1L) {
            positions <- positions[-1L]
          }
          serr <- rep_len(oo$y.se, length(oo$y))
          for (k in positions) {
            if (!is.finite(oo$y[k])) next
            values <- oo$y[k]
            if (se && is.finite(serr[k])) {
              values <- c(values, values - 2 * serr[k], values + 2 * serr[k])
            } else {
              ## No uncertainty guide is available; show only the mean.
              values <- rep(values, 3L)
            }
            boxes[[length(boxes) + 1L]] <- values
            box.labels <- c(box.labels, format(oo$x[k], trim = TRUE, digits = 4))
            box.groups <- c(box.groups, g)
          }
        }
        if (!length(boxes)) return(NULL)
        if (is.null(dots$ylim)) dots$ylim <- range(unlist(boxes), finite = TRUE)
        bp <- boxplot(boxes, plot = FALSE, names = box.labels)
        defaults <- list(z = bp, outline = FALSE, xaxt = "n",
                         boxfill = if (cflag) box.groups else "lightblue")
        ## Draw labels separately, with the complete predictor values.
        bxp.args <- .plot.args(defaults, dots)
        bxp.args$xaxt <- "n"
        do.call("bxp", bxp.args)
        if (!identical(dots$axes, FALSE) && !identical(dots$xaxt, "n")) {
          axis.names <- c("cex.axis", "col.axis", "font.axis", "las", "tck", "tcl",
                          "lwd", "lwd.ticks", "col", "col.ticks", "hadj", "padj")
          axis.args <- dots[intersect(names(dots), axis.names)]
          do.call("axis", c(list(side = 1, at = seq_along(boxes),
                                labels = box.labels, tick = TRUE), axis.args))
        }
        if (cflag) {
          shown <- unique(box.groups)
          fills <- if (is.null(bxp.args$boxfill)) shown else
            rep_len(bxp.args$boxfill, length(box.groups))[match(shown, box.groups)]
          legend("topright", legend = names(plotO)[shown], fill = fills)
        }
      }
      ###----------------------------------------------------------------
      ###
      ### finished plot: exit with NULL
      ###
      ### ----------------------------------------------------------------
      NULL
    }##ends plotting
    ## user has requested no plots
    else {
      plotO
    }
  })
  ##---------------------------------------------------
  ##
  ## RETURN THE PLOT OBJECT IF NO PLOTS REQUESTED
  ##
  ##---------------------------------------------------
  if (!plot.it) {
    names(rO) <- xvar.names
    ## list of list, so unwind it a little 
    unlist(rO, recursive = FALSE)
  }
}
