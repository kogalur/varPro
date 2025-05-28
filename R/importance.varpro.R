##################################################################
### 
### 
### 
###  importance utility
###  extracts varpro importance
###  plots results (can be separated into another routine)
###
###    
###
####################################################################
importance.varpro <- function(o, local.std = TRUE, y.external = NULL,
                              cutoff = 0.79, trim = 0.1,
                              plot.it = FALSE, conf = TRUE, sort = TRUE,
                              ylab = if (conf) "Importance" else "Standardized Importance",
                              max.rules.tree, max.tree,
                              papply = mclapply,
                              ...)
{
  ## ------------------------------------------------------------------------
  ##
  ## coherence of incoming object 
  ##
  ## ------------------------------------------------------------------------
  if (!(inherits(o, "varpro") || inherits(o, "uvarpro"))) {
    stop("this function only works for varpro, uvarpro objects")
  }
  if (inherits(o, "uvarpro")) {
    local.std <- FALSE
    y.external <- NULL
  }
  ## ------------------------------------------------------------------------
  ##
  ## call varpro.strength?
  ## applies under various settings
  ##
  ## ------------------------------------------------------------------------
  if (local.std || !is.null(y.external) || (!missing(max.rules.tree) || !missing(max.tree))) {
    ## update varpro parameters if they are supplied
    if (missing(max.rules.tree)) {
      max.rules.tree <- o$max.rules.tree
    }
    if (missing(max.tree)) {
      max.tree <- o$max.tree
    }
    ## call varpro strength
    oo <- get.varpro.strength(object = o,
                      max.rules.tree = max.rules.tree,
                      max.tree = max.tree,
                      membership = TRUE)
    ## updated results
    results <- get.varpro.strengthArray(oo$strengthArray, o$family, o$y)
    ## replaces original varpro statistic with locally standardize values 
    if (local.std) {
      ## over-write original importance values
      imp.names.pt <- grepl("imp", colnames(results))
      results[, imp.names.pt] <- NA
       ## identify useful rules and variables at play
       keep.rules <- which(oo$strengthArray$oobCT > 0 & oo$strengthArray$compCT > 0)
       ## membership lists 
       oobMembership <- oo$oobMembership
       compMembership <- oo$compMembership
       ## add attributes to y - used for importance calculations
       y <- o$y
       if (!is.null(y.external)) {
         y <- y.external
         attr(y, "y.org") <- y.external
       }
       else {
         attr(y, "y.org") <- o$y.org
       }
       attr(y, "family") <- o$family
       ## loop for calculations
       if (length(keep.rules) > 0) {
         imp <- papply(keep.rules, function(i) {
           local.importance(y, oobMembership[[i]], compMembership[[i]])
         })
         if (sum(imp.names.pt) == 1) {
           results[keep.rules, imp.names.pt] <- unlist(imp)
         }
         else {
           results[keep.rules, imp.names.pt] <- do.call(rbind, imp)
         }
         results <- results[keep.rules,,drop=FALSE]##added 01/04/2025
       }
    }
    ## over-ride original object with updated information
    o$results <- results
    o$max.rules.tree <- max.rules.tree
    o$max.tree <- max.tree
  }
  ## ------------------------------------------------------------------------
  ##
  ## regression, classification
  ##
  ## ------------------------------------------------------------------------
  if (o$family != "regr+") {
    importance.varpro.workhorse(o = o,
                                cutoff = cutoff,
                                trim = trim,
                                plot.it = plot.it,
                                conf = conf,
                                sort = sort,
                                ylab = ylab,
                                papply = papply,
                                local.std = local.std,
                                ...)
  }
  ## ------------------------------------------------------------------------
  ##
  ## mv-regression (i.e. survival with rmst vector)
  ##
  ## ------------------------------------------------------------------------
  else {
    lapply(1:ncol(o$y), function(j) {
      o$results <- o$results[, c((1:4), 4+j)]
      importance.varpro.workhorse(o = o,
                                  cutoff = cutoff,
                                  trim = trim,
                                  plot.it = FALSE,
                                  sort = sort,
                                  papply = papply,
                                  local.std = local.std,
                                  ...)
    })
  }
}
importance <- importance.varpro 
importance.varpro.workhorse <- function(o, cutoff, trim, plot.it, conf, sort,
              ylab, papply, local.std, ...) {
  ## ------------------------------------------------------------------------
  ##
  ## extract desired quantities from the varpro object
  ##
  ## ------------------------------------------------------------------------
  dta <- o$results
  xvar.names <- o$xvar.names
  ## ------------------------------------------------------------------------
  ##
  ## extra alpha from the cutoffset the one-sided cut off value
  ##
  ## ------------------------------------------------------------------------
  zcut <- cutoff
  alpha <- pnorm(zcut, lower.tail = FALSE)
  ## ------------------------------------------------------------------------
  ##
  ##  legacy: set the weight flag 
  ##
  ## ------------------------------------------------------------------------
  wt.flag <- FALSE
  ## ------------------------------------------------------------------------
  ##
  ##  for classification families we need to also process conditional importance
  ##
  ## ------------------------------------------------------------------------
  if (o$family == "class") {
    ylevels <- levels(o$y)
    J <- length(ylevels)
  }
  ## ------------------------------------------------------------------------
  ##
  ##  convert variable number to variable name
  ##
  ## ------------------------------------------------------------------------
  dta$variable <- factor(xvar.names[dta$variable])
  ## ------------------------------------------------------------------------
  ##
  ## optional variable weight equal to number of times variable splits
  ## this can be used as additional weight to define the final importance
  ## - the default = 1 i.e. no weighting
  ## - weighting can lead to biased results (legacy)
  ##
  ## ------------------------------------------------------------------------
  if (wt.flag) {
    avgwt <- tapply(dta$variable, dta$variable, length)
    avgwt <- avgwt / max(avgwt, na.rm = TRUE)
  }
  else {
    avgwt <- rep(1, length(levels(dta$variable)))
  }
  ## ------------------------------------------------------------------------
  ##
  ## summary values for each variable, averagd across trees
  ##
  ## ------------------------------------------------------------------------
  rO <- data.frame(
    n.oob = tapply(dta$n.oob, dta$variable, mean, na.rm=TRUE),
    wt    = avgwt
  )
  ## ------------------------------------------------------------------------
  ##
  ## aquire the tree bootstrap estimator for each tree
  ##
  ## ------------------------------------------------------------------------
  xvarused.names <- rownames(rO)
  p <- length(xvarused.names)
  ## acquire the tree importance for each variable
  imp.tree = do.call(rbind, papply(split(dta, dta$tree), function(dd) {
    imp <- rep(0, p)
    names(imp) <- xvarused.names
    ## extract the rule based importance, converting it to a fraction, then apply weighting
    ## do not count statistics with sample size of zero (set them to NA)
    imp.numer <- dd$imp * dd$n.oob
    imp.denom <- dd$n.oob
    imp.denom[imp.denom == 0] <- NA
    imptr <- (tapply(imp.numer, dd$variable, sum, na.rm = TRUE) /
        tapply(imp.denom, dd$variable, sum, na.rm = TRUE))
    imptr[is.na(imptr)] <- 0
    imp[names(imptr)] <- avgwt * as.numeric(imptr)
    ## additional processing for classification, unconditional importance
    ## do not count statistics with sample size of zero (set them to NA)
    if (o$family == "class") {
      ## "pure" conditional importance
      impC.numer <- do.call(cbind, lapply(1:J, function(j) {
        dd[, paste0("imp.", j)] * dd[, paste0("n.oob.", j)] 
      }))
      ## conditional overall importance, conditioned by class label as in rfsrc
      #impC.numer <- do.call(cbind, lapply(1:J, function(j) {
      #  imp.numer * dd[, paste0("n.oob.", j)] 
      #}))
      impC.denom <- do.call(cbind, lapply(1:J, function(j) {
        denom <- dd[, paste0("n.oob.", j)]
        denom[denom == 0] <- NA
        denom
      }))
      ## string out the conditional importance as a long vector, with class label values
      ## in order: class 1 importance followed by class 2 and so forth to class J
      impCtr <- unlist(lapply(1:J, function(j) {
        impCj <- rep(0, p)
        names(impCj) <- xvarused.names
        impCtrj <- unlist(tapply(impC.numer[, j], dd$variable, sum, na.rm = TRUE) /
                    tapply(impC.denom[, j], dd$variable, sum, na.rm = TRUE))
        impCtrj[is.na(impCtrj)] <- 0
        impCj[names(impCtrj)] <- avgwt * as.numeric(impCtrj)
        impCj
      }))
    }
    ## return the goodies
    if (o$family != "class") {
      imp
    }
    else {
      ## since the conditional importance is a long vector, give names to identify it
      imp <- c(imp, impCtr)
      names(imp) <- c(xvarused.names, paste0(xvarused.names, ".", rep(1:J, each = p)))
      imp
    }
   }))
  ## ------------------------------------------------------------------------
  ##
  ## extract conditional importance for use later
  ##
  ## ------------------------------------------------------------------------
  if (o$family == "class") {
    impC.tree <- imp.tree[, -(1:p), drop = FALSE]
  }
  ## ------------------------------------------------------------------------
  ##
  ## process unconditional importance --> primary analysis based on this
  ## bootstrap importance estimator, standard error and Z-statistic - use winsorization
  ##
  ## ------------------------------------------------------------------------
  imp.tree <- imp.tree[, (1:p), drop = FALSE]
  impwt.mn <- apply(imp.tree, 2, winsorize.mean, trim = trim)
  impwt.sd <- apply(imp.tree, 2, winsorize.sd, trim = trim)
  if (local.std) {
    impwt.sd <- 1
  }
  ## clean up and get ready for output
  rO$mean <- impwt.mn
  rO$std <- impwt.sd
  rO$z <- impwt.mn / impwt.sd
  rO$zcenter <- rO$z - zcut
  rO$selected <- 1 * (rO$zcenter >= 0)
  rO$wt <- rO$n.oob <- NULL
  ## remove rows with all NA's
  allNA <- apply(rO, 1, function(x){all(is.na(x))})
  rO <- rO[!allNA,, drop = FALSE]
  rO$selected[is.na(rO$selected)] <- 0
  ## sort 
  if (sort) {
    rO <- rO[order(rO$zcenter, decreasing = TRUE),, drop = FALSE]
  }
  else {
    nms <- intersect(xvar.names, rownames(rO))
    rO <- rO[nms,, drop = FALSE]
  }
  ## ------------------------------------------------------------------------
  ##
  ## process conditional importance --> secondary analysis for classificaiton
  ## return the centered z statistic and final decision 
  ##
  ## ------------------------------------------------------------------------
  if (o$family == "class") {
    ## acquire z/zcenter
    rOC.z <- do.call(cbind, lapply(1:J, function(j) {
      impCj.tree <- impC.tree[, colnames(impC.tree) %in% paste0(xvarused.names, ".", j), drop = FALSE]
      impCwtj.mn <- apply(impCj.tree, 2, winsorize.mean, trim = trim)
      if (!local.std) {
        impCwtj.sd <- apply(impCj.tree, 2, winsorize.sd, trim = trim)
        impCwtj.mn / impCwtj.sd
      }
      else {
        impCwtj.mn
      }
    }))
    rownames(rOC.z) <- xvarused.names
    colnames(rOC.z) <- ylevels
    ## remove rows with all NA's
    allNA <- apply(rOC.z, 1, function(x){all(is.na(x))})
    rOC.z <- rOC.z[!allNA,, drop = FALSE]
    ## sort the data in same order as unconditional importance
    rOC.z <- as.matrix(rOC.z[intersect(rownames(rO), rownames(rOC.z)),, drop = FALSE])
    rOC.zcenter <-  rOC.z - zcut
    ## remove rows with all NA's
    allNA <- apply(rOC.zcenter, 1, function(x){all(is.na(x))})
    rOC.zcenter <- rOC.zcenter[!allNA,, drop = FALSE]
    ## acquire final decision
    rOC.selected <- 1 * (rOC.zcenter >= 0)
    ## remove rows with all NA's
    allNA <- apply(rOC.selected, 1, function(x){all(is.na(x))})
    rOC.selected <- rOC.selected[!allNA,, drop = FALSE]
    rOC.selected[is.na(rOC.selected)] <- 0
  }
  ## ------------------------------------------------------------------------
  ##
  ## plot
  ##
  ## ------------------------------------------------------------------------
  if (plot.it && nrow(imp.tree) > 1) {
    mn <- rO$mean
    se <- rO$std
    z <- rO$z
    imp.tree <- imp.tree[, rownames(rO), drop = FALSE]
    clm <- mn - qnorm(1-alpha) * se
    cum <- mn + qnorm(1-alpha) * se
    minm <- pmin(clm, apply(imp.tree, 2, function(x){min(winsorize(x, trim = trim))}))
    maxm <- pmax(cum, apply(imp.tree, 2, function(x){max(winsorize(x, trim = trim))}))
    ylim <- range(c(minm, maxm), na.rm = TRUE)
    if (conf) {
      bp <- boxplot(imp.tree, plot = FALSE)
      bp$stats <- matrix(c(
        minm,
        clm,
        mn,
        cum,
        maxm
      ), nrow = 5, byrow = TRUE)
      bxp(bp, ylim = ylim,
          boxfill = c("lightblue", "red")[1+rO$selected],
          las = 2, outline = FALSE, ylab = ylab,
          ...)
    }
    else {
      names(z) <- rownames(rO)
      barplot(z,
              ylab = ylab,
              las = 2,
              col = c("lightblue", "red")[1+rO$selected], ...)
    }
  }
  ## ------------------------------------------------------------------------
  ##
  ## return the goodies
  ##
  ## ------------------------------------------------------------------------
  ## deprecated
  rO$zcenter <- rO$selected <- NULL
  ## mean, std depracated for local.std
  if (local.std) {
    rO$mean <- rO$std <- NULL
  }
  if (o$family != "class") {
    rO
  }
  else {
    list(unconditional = rO, conditional.z = rOC.z)
  }
}
