plot.partialpro <- function(x, xvar.names, nvar,
        parametric = FALSE, se = TRUE,
        causal = FALSE, subset = NULL,
        plot.it = TRUE, ...) {
  ## ------------------------------------------------------------------------
  ##
  ## initial processing
  ##
  ## ------------------------------------------------------------------------
  ## identify target variables
  if (missing(xvar.names)) {
    xvar.names <- names(x)
    if (!missing(nvar)) {
      xvar.names <- xvar.names[1:min(length(xvar.names), nvar)]
    }
  }
  ## extract the object
  o <- x[xvar.names]
  ## user specified hidden options
  dots <- list(...)
  ## specify hidden options
  if (is.null(dots$weights.power)) {
    weights.power <- 10
  }
  if (is.null(dots$weights.tolerance)) {
    weights.tolerance <- 1e-6
  }
  dots$weights.power <- dots$weights.tolerance <- NULL
  ## ------------------------------------------------------------------------
  ##
  ## loop over each variable, generating the requested plot
  ##
  ## ------------------------------------------------------------------------
  rO <- lapply(1:length(xvar.names), function(j) {
    ## failure checks
    if (is.null(o[[j]])) {
      return(NULL)
    }
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
    ## subset analysis not allowed for parametric model
    if (type == "parametric") {
      subset <- NULL
    }
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
        if (length(idx) == 0) {
          NULL
        }
        which(case %in% idx)
      })
      names(idx.lst) <- levels(subset)
      idx.lst <- idx.lst[!sapply(idx.lst, is.null)]
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
        idx.lst[[1]] <- 1:length(case)
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
        ## obtain frequencies/weights for s.e./smoothing
        frq <- !apply(goodvt[sub,, drop=FALSE], 2, is.na)
        if (is.null(dim(frq))) {
          return(NULL)
        }
        frq <- colSums(frq)
        weights <- (frq / max(frq)) ^ weights.power
        pt.tolerance <- weights > weights.tolerance
        if (sum(pt.tolerance) == 0) {
          return(NULL)
        }
        ## standard error estimate
        ysd <- apply(yhat.nonpar[sub,, drop=FALSE], 2, sd, na.rm = TRUE)
        if (all(is.na(ysd)) || all(ysd <= 1e-10)) {
          ysd <- 1e-10
        }
        y.se <- ysd / sqrt(frq)
        if (!parametric) {
          y.se <- y.se[pt.tolerance]
        }
        ## over-ride se
        if (!se) {
          y.se <- 0
        }
        ## loess control parameters
        loessControl <- loess.control(trace.hat = if (length(sub) > 500) "approximate" else "exact")
        ## -------------------------------------------------------------
        ##
        ## nonparametric smoothing estimator
        ##
        ## -------------------------------------------------------------
        if (type == "nonparametric" || type == "causal") {
          if (type ==  "nonparametric") {
            o.loess <- tryCatch({suppressWarnings(loess(y ~ x,
              data.frame(y = colMeans(yhat.nonpar[sub,, drop=FALSE], na.rm = TRUE), x = xvirtual)[pt.tolerance,, drop = FALSE],
              weights = weights[pt.tolerance], control=loessControl))}, error = function(ex){NULL})
          }
          else {
            o.loess <- tryCatch({suppressWarnings(loess(y ~ x,
              data.frame(y = colMeans(yhat.causal[sub,, drop = FALSE], na.rm = TRUE), x = xvirtual)[pt.tolerance,, drop = FALSE],
              weights = weights[pt.tolerance], control=loessControl))}, error = function(ex){NULL})
          }
          ## -------------------------------
          ##
          ## return estimator
          ##
          ## -------------------------------
          if (is.null(o.loess)) {
            return(NULL)
          }
          x <- c(o.loess$x)
          y <- o.loess$fitted
        }
        ## -------------------------------------------------------------
        ##
        ## parametric (smoothed) estimator
        ##
        ## -------------------------------------------------------------
        else {
          x <- xvirtual
          y <- colMeans(yhat.par, na.rm=TRUE)
        }
        ## -------------------------------------------------------------
        ##
        ## return goodies
        ##
        ## -------------------------------------------------------------
        list(x = x, y = y, y.se = y.se)
      })
      names(plotO) <- names(idx.lst)
    }
    ##---------------------------------------------------
    ##
    ## ESTIMATION+STANDARD ERRORS: binary variables
    ##
    ##---------------------------------------------------
    else {
      plotO <- lapply(idx.lst, function(sub) {
        ## obtain frequecies for s.e.
        frq <- !apply(yhat.nonpar[sub,, drop=FALSE], 2, is.na)
        if (is.null(dim(frq))) {
          return(NULL)
        }
        frq <- colSums(frq)
        ## standard error estimate
        ysd <- apply(yhat.nonpar[sub,, drop=FALSE], 2, sd, na.rm = TRUE)
        if (all(is.na(ysd)) || all(ysd <= 1e-10)) {
          ysd <- 1e-10
        }
        y.se <- ysd / sqrt(frq)
        ## over-ride se
        if (!se) {
          y.se <- 0
        }
        ## mean estimators
        if (type == "causal") {
          y <- colMeans(yhat.causal[sub,, drop=FALSE], na.rm = TRUE)
        }
        ## there is no parametric estimator for binary case
        else {
          y <- colMeans(yhat.nonpar[sub,, drop=FALSE], na.rm = TRUE)
        }
        ## return goodies
        list(x = xvirtual, y = y, y.se = y.se)
      })
      names(plotO) <- names(idx.lst)
    }
    ##---------------------------------------------------
    ##
    ## remove NULL entries: exit if nothing 
    ##
    ##---------------------------------------------------
    plotO <- plotO[!sapply(plotO, is.null)]
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
      }
      else {
        nmax <- 250
      }
      if (is.null(dots$ylab)) {
        if (type != "causal") {
          dots$ylab <- "partial effect"
        }
        else {
          dots$ylab <- "causal effect"
        }
      }
      else {
        dots$ylab <- dots$ylab[min(length(dots$ylab), j)]
      }
      if (is.null(dots$xlab)) {
        dots$xlab <- xvar.names[j]
      }
      else {
        dots$xlab <- dots$xlab[min(length(dots$xlab), j)]
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
        ## set the ylim range
        if (is.null(dots$ylim)) {
          dots$ylim <- range(unlist(lapply(plotO, function(oo) {
              c(oo$y - 2 * oo$y.se, oo$y + 2 * oo$y.se)
            })), na.rm = TRUE)
        }
        else {
          if (is.list(dots$ylim)) {
            dots$ylim <- dots$ylim[[min(length(dots$ylim), j)]]
          }
          else {
            dots$ylim <- dots$ylim
          }
        }
        ## generate the plot
        suppressWarnings(do.call(plot, c(list(x=x, y=y, type = "n"), dots)))
        if (cflag) {
          nullO <- lapply(1:length(plotO), function(j) {
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
          legend("topright", legend = names(plotO), fill = 1:length(plotO))
        }
      }
      ##---------------------------------------------------
      ##
      ## PLOTS: boxplot for binary case
      ##
      ##---------------------------------------------------
      else {
        ## -------------------------------------------------
        ##
        ## no conditioning 
        ##
        ## -------------------------------------------------
        if (!cflag) {
          ## pull the data
          x <- plotO[[1]]$x
          y <- plotO[[1]]$y
          y.se <- plotO[[1]]$y.se
          ## graphical niceties
          dots$ylim <- NULL
          ## don't need baseline value for causal plots of binary variables
          if (type == "causal") {
            if (length(x) > 1) {
              x <- x[-1]
              y <- y[-1]
              y.se <- max(y.se)
            }
            else {
              y.se <- 0
            }
          }
          ## boxplot
          bp <- boxplot(c(y, y-2*y.se, y+2*y.se)~rep(x, 3), names = rep("", length(x)), plot = FALSE)
          y.se <- .0001
          do.call("bxp", c(list(z = bp, outline = FALSE, range = 2,
                                boxfill = "lightblue",
                                ylim = c(min(bp$stats[1,], na.rm = TRUE) * ( 1 - 2 * y.se ),
                                         max(bp$stats[5,], na.rm = TRUE) * ( 1 + 2 * y.se )),
                                xaxt = "n"), dots))
          do.call("axis", c(list(side = 1, at = 1:length(x),
                                 labels = format(x, trim = TRUE, digits = 4),
                                 tick = TRUE), dots))
        }
        ## -------------------------------------------------
        ##
        ## conditioning
        ##
        ## -------------------------------------------------
        else {
          ## pull the data
          bxp.dta <- do.call(rbind, lapply(1:length(plotO), function(j) {
            oo <- plotO[[j]]
            rbind(data.frame(x=oo$x, y=oo$y,        sub=names(plotO)[j]),
                  data.frame(x=oo$x, y=oo$y-2*oo$y.se, sub=names(plotO)[j]),
                  data.frame(x=oo$x, y=oo$y+2*oo$y.se, sub=names(plotO)[j]))
          }))
          bxp.dta <- data.frame(bxp.dta)
          bxp.dta$sub <- factor(bxp.dta$sub)
          ## removing missing values
          bxp.dta <- na.omit(bxp.dta)
          if (nrow(bxp.dta) == 0) {
            return(NULL)
          }
          ## set up axis values, colors and legend
          bxp <- boxplot(y~x:sub, bxp.dta, plot=FALSE)$names
          xv <- sapply(strsplit(bxp, ""), function(ss) {ss[1]})
          cv <- sapply(strsplit(bxp, ""), function(ss) {paste(ss[-(1:2)], collapse="")})
          clr <- as.numeric(factor(cv))
          ## boxplot
          boxplot(y~x:sub, bxp.dta, boxfill = clr, xaxt = "n", ylab = dots$ylab, xlab = dots$xlab)
          axis(side = 1, at = 1:length(xv), labels = xv)
          legend("topright", legend = levels(bxp.dta$sub), fill = 1:length(unique(clr)))
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
