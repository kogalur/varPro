get.splitweight.custom <- function(f, data, namedvec = NULL) {
  x <- get.hotencode(get.stump(f, data)$xvar)
  swt <- rep(1, ncol(x))
  names(swt) <- colnames(x)
  if (!is.null(namedvec) && !is.null(names(namedvec))) {
    common <- intersect(names(namedvec), names(swt))
    print(common)
    if (length(common) > 0) {
      swt[common] <- namedvec[common]
    }
  }
  swt
}
get.orgvimp <- function(o, pretty = TRUE, local.std = TRUE, vmp = NULL) {
  ## input value must be a varpro, cv.varpro or uvarpro object
  if (!(inherits(o, "varpro") ||
        inherits(o, "cv.varpro") ||
        inherits(o, "uvarpro")))  {
    stop("object must be a varpro, cv.varpro or uvarpro object")
  }
  ## first deal with cv.varpro since it's already encoded for original variables
  ## (to get hot-encoded importance we use get.vimp()
  if (inherits(o, "cv.varpro")) {
    ## nothing to do unless pretty = FALSE
    if (pretty) {
      return(o)
    }
    else {
      nms <- attr(o, "xvar.org.names")
      z.min <- z.conserve <- z.liberal <- rep(0, length(nms))
      names(z.min) <- names(z.conserve) <- names(z.liberal) <- nms
      z.min[o$imp$variable] <- o$imp$z
      z.conserve[o$imp.conserve$variable] <- o$imp.conserve$z
      z.liberal[o$imp.liberal$variable] <- o$imp.liberal$z
      return(data.frame(imp=z.min, imp.conserve=z.conserve, imp.liberal=z.liberal))
    }
  }
  ## hereafter we are dealing with a varpro object  
  ## extract the vimp 
  vmp <- importance(o, local.std = local.std)
  if (o$family == "regr+") {
      vmp <- do.call(rbind, vmp)
  }
  if (o$family == "class") {
    vmp <- vmp$unconditional
  }
  ## pull original xvar names 
  xvar.org.names <- o$xvar.org.names
  ## we are finished if: i) data not hotencoded; and (ii) not a regr+ family
  if (!attr(o$x, "hotencode") && o$family != "regr+") {
    vars <- rownames(vmp)
    vars.z <- vmp$z
  }
  ## data was hotencoded or family is regr+ ... so we need to map names appropriately
  else {
    ## we only need the rownames for vimp from the varpro object hereafter
    rownms <- rownames(vmp)
    ## match original variable names to varpro names which uses hot encode data
    vars <- xvar.org.names[which(unlist(lapply(xvar.org.names, function(nn) {
      if (any(grepl(nn, rownms))) {
        TRUE
      }
      else {
        FALSE
      }
    })))]
    ## obtain z for mapped variables
    vars.z <- lapply(xvar.org.names, function(nn) {
      if (any((pt <- grepl(nn, rownms)))) {
        if (!all(is.na(vmp[pt, "z"]))) {
          max(vmp[pt, "z"], na.rm = TRUE)
        }
        else {
          0
        }
      }
      else {
        NULL
      }
    })
    ## remove NULL entries
    vars.z <- unlist(vars.z[!sapply(vars.z, is.null)])
  }
  ## make nice table for return
  if (pretty) {
    topvars <- data.frame(variable = vars, z = vars.z)
    topvars[order(topvars$z, decreasing = TRUE),, drop = FALSE]
  }
  ## return named vector with z values (0 if not selected)
  else {
    z <- rep(0, length(xvar.org.names))
    names(z) <- xvar.org.names
    z[vars] <- vars.z
    z
  }
}
## extract names of signal variables from varpro analysis
get.topvars <- function(o, local.std = TRUE) {
  ## input value must be a varpro or uvarpro object
  if (!(inherits(o, "varpro") || inherits(o, "uvarpro"))) {
    stop("object must be a varpro or uvarpro object")
  }
  ## extract the vimp and names
  vmp <- importance(o, local.std = local.std)
  ## mv-regression
  if (o$family == "regr+") {
    return(unique(unlist(lapply(vmp, function(o){rownames(o)}))))
  }
  ## classification
  if (o$family == "class") {
    vmp <- vmp$unconditional
  }
  ## return the goodies
  rownames(vmp)
}
## extract vimp
get.vimp <- function(o, pretty = TRUE, local.std = TRUE) {
  ## input value must be a varpro, cv.varpro or uvarpro object
  if (!(inherits(o, "varpro") ||
        inherits(o, "cv.varpro") ||
        inherits(o, "uvarpro")))  {
    stop("object must be a varpro, cv.varpro or uvarpro object")
  }
  ## varpro, uvarpro object
  if (inherits(o, "varpro") || inherits(o, "uvarpro")) {
    ## extract vimp + names
    vmp <- importance(o, local.std = local.std)
    ## mv-regression
    if (o$family == "regr+") {
      vmp <- do.call(rbind, lapply(1:length(vmp), function (j) {
        data.frame(vmp[[j]], names=rownames(vmp[[j]]), outcome=j)
      }))
    }
    ## other families
    else {
      if (o$family == "class") {
        vmp <- vmp$unconditional
      }
      vmp$names <- rownames(vmp)
      vmp$outcome <- 1
    }
    ##return the goodies
    if (pretty) {
      z <- vmp$z
      names(z) <- vmp$names
      z[is.na(z)] <- 0
      if (o$family == "regr+") {
        split(z, vmp$outcome)
      }
      else {
        z
      }
    }
    else {## not seleted variables are mapped to 0
      zO <- lapply(split(vmp, vmp$outcome), function(v) {
        z <- rep(0, ncol(o$x))
        names(z) <- colnames(o$x)
        z[v$names] <- v$z
        z[is.na(z)] <- 0
        z
      })
      if (length(zO) == 1) {
        zO[[1]]
      }
      else {
        zO
      }
    }
  }
  ## cv.varpro object
  else {
    ## pull the original vimp
    vmp <- attr(o, "imp.org")   
    ## mv-regression
    if (attr(o, "family") == "regr+") {
      vmp <- do.call(rbind, lapply(1:length(vmp), function (j) {
        data.frame(vmp[[j]], names=rownames(vmp[[j]]), outcome=j)
      }))
    }
    ## other families
    else {
      if (attr(o, "family") == "class") {
        vmp <- vmp$unconditional
      }
      vmp$names <- rownames(vmp)
      vmp$outcome <- 1
    }
    ## threshold using cv zcut values
    zO <- lapply(split(vmp, vmp$outcome), function(v) {
      v$outcome <- NULL
      rownames(v) <- v$names
      if (pretty) {
        v$names <- NULL
      }
      v.min <- v[v$z >= o$zcut,, drop = FALSE]
      v.conserve <- v[v$z >= o$zcut.conserve,, drop = FALSE]
      v.liberal <- v[v$z >= o$zcut.liberal,, drop = FALSE]
      ## return the goodies
      if (pretty) {        
        list(imp = v.min,
             imp.conserve = v.conserve,
             imp.liberal = v.liberal)
      }
      else {
        nms <- attr(o, "xvar.names")
        z.min <- z.conserve <- z.liberal <- rep(0, length(nms))
        names(z.min) <- names(z.conserve) <- names(z.liberal) <- nms
        z.min[v.min$names] <- v.min$z
        z.conserve[v.conserve$names] <- v.conserve$z
        z.liberal[v.liberal$names] <- v.liberal$z
        data.frame(imp=na.omit(z.min),
                   imp.conserve=na.omit(z.conserve),
                   imp.liberal=na.omit(z.liberal))
      }
    })
    if (length(zO) == 1) {
      zO[[1]]
    }
    else {
      zO
    }
  }
}
##  winsorized statistics
winsorize <- function (x, trim = 0.1, na.rm = TRUE) {
  if ((trim < 0) | (trim > 0.5)) 
    stop("trimming must be reasonable")
  qtrim <- quantile(x, c(trim, 0.5, 1 - trim), na.rm = na.rm)
  xbot <- qtrim[1]
  xtop <- qtrim[3]
  if (trim < 0.5) {
    x[x < xbot] <- xbot
    x[x > xtop] <- xtop
  }
  else {
    x[!is.na(x)] <- qtrim[2]
  }
  return(x)
}
winsorize.sd <- function (x, trim = 0.1, na.rm = TRUE) {
  if ((trim < 0) | (trim >= 0.5)) {
    stop("trimming must be reasonable")
  }
  sqrt(var(winsorize(x, trim = trim, na.rm = na.rm), na.rm = na.rm))
}
winsorize.mean <- function (x, trim = 0.1, na.rm = TRUE) {
  if ((trim < 0) | (trim > 0.5)) 
    stop("trimming must be reasonable")
  if (trim < 0.5) {
    return(mean(winsorize(x, trim = trim, na.rm = na.rm), na.rm = na.rm))
  }
  else {
    return(median(x, na.rm = na.rm))
  }
}
##################################################################
### 
### 
### 
### custom locally standardized importance 
###
###    
###
####################################################################
local.importance <- function(y, idx1, idx2, local.std = TRUE) {
  ## local estimator reverts to mortality importance (t-test)
  ## used due to low power of log-rank
  if (attr(y, "family") == "surv" && local.std) {
    attr(y, "family") <- "regr"
  }
  ## all families except multivariate
  if (attr(y, "family") != "regr+") {
    local.importance.workhorse(y, idx1, idx2, local.std)
  }
  ## regr+ requires separate calls for each y outcome
  else {
    do.call(cbind, lapply(1:ncol(y), function(j) {
      yj <- y[, j]
      attr(yj, "family") <- "regr"
      local.importance.workhorse(yj, idx1, idx2, local.std)
    }))
  }
}
local.importance.workhorse <- function(y, idx1, idx2, local.std) {
  ## key attributes/dimensions
  n <- length(y[idx1])
  family <- attr(y, "family")
  y.org <- attr(y, "y.org")
  ## ---------------------------------------------------------------------
  ## regression
  ## y is real-valued ---> mse
  if (family == "regr" || family == "regr+") {
    ## bail out if subsetted y has zero length
    if (n == 0) {
      return(NA)
    }
    ## local importance 
    if (local.std) {
      ## Welch t-statistic (same statistic as stats::t.test default),
      ## computed directly for speed (t.test() object construction is expensive).
      x1 <- y[idx1]
      x2 <- y[idx2]
      x1 <- x1[!is.na(x1)]
      x2 <- x2[!is.na(x2)]
      n1 <- length(x1)
      n2 <- length(x2)
      if (n1 < 2 || n2 < 2) {
        NA
      }
      else {
        m1 <- mean(x1)
        m2 <- mean(x2)
        v1 <- var(x1)
        v2 <- var(x2)
        se <- sqrt(v1 / n1 + v2 / n2)
        if (!is.finite(se) || se <= 0) {
          NA
        }
        else {
          abs((m1 - m2) / se)
        }
      }
    }
    ## canonical importance
    else {
      abs(mean(y[idx2], na.rm = TRUE) - mean(y[idx1], na.rm = TRUE)) / sd(y, na.rm = TRUE)
    }
  }
  ## ---------------------------------------------------------------------
  ## classification
  ## y is a factor --> get "all" performance and J-class performance, a J+1 vector
  ## classification
## y is a factor --> get "all" performance and J-class performance, a J+1 vector
else if (family == "class") {
  ## number of class labels
  J <- length(levels(y))
  ## bail out if subsetted y has zero length
  if (n == 0) {
    return(rep(NA, 1 + J))
  }
  ## frequency counts (fast tabulate instead of tapply)
  yint <- as.integer(y)
  y1 <- yint[idx1]
  y2 <- yint[idx2]
  ## drop NA (tabulate does not accept NA)
  y1 <- y1[!is.na(y1)]
  y2 <- y2[!is.na(y2)]
  frq1.full <- tabulate(y1, nbins = J)
  frq2.full <- tabulate(y2, nbins = J)
  names(frq1.full) <- names(frq2.full) <- 1:J
  ## local importance 
  if (local.std) {
    ## build frequencies for all cells, but keep track of 0/0 cells
    nonzero <- frq1.full > 0 | frq2.full > 0
    frq1 <- frq1.full[nonzero]
    frq2 <- frq2.full[nonzero]
    J.nonzero <- sum(nonzero)
    ## overall chi-square statistic (stats::chisq.test default, including
    ## Yates correction for 2x2 tables)
    r1 <- sum(frq1)
    r2 <- sum(frq2)
    ntot <- r1 + r2
    if (ntot > 0 && r1 > 0 && r2 > 0 && J.nonzero > 1) {
      coltot <- frq1 + frq2
      e1 <- r1 * coltot / ntot
      e2 <- r2 * coltot / ntot
      if (J.nonzero == 2) {
        ## 2x2 table: apply Yates correction (chisq.test(correct=TRUE) default)
        O <- c(frq1[1], frq1[2], frq2[1], frq2[2])
        E <- c(e1[1],  e1[2],  e2[1],  e2[2])
        Y <- pmin(0.5, abs(O - E))
        chisq <- sum((abs(O - E) - Y)^2 / E)
      } else {
        ## general 2 x k Pearson chi-square
        valid <- (e1 > 0) & (e2 > 0)
        chisq <- sum((frq1[valid] - e1[valid])^2 / e1[valid] +
                     (frq2[valid] - e2[valid])^2 / e2[valid])
      }
      perf.all <- sqrt(chisq / max(1, J.nonzero - 1))
    } else {
      perf.all <- NA_real_
    }
    ## class-specific test (binomial z, separate variance)
    perf.class <- rep(NA_real_, J)
    if (J.nonzero > 0 && r1 > 0 && r2 > 0) {
      p1 <- frq1 / r1
      p2 <- frq2 / r2
      se <- sqrt(p1 * (1 - p1) / r1 + p2 * (1 - p2) / r2)
      z <- rep(NA_real_, length(se))
      ok <- se > 0
      z[ok] <- abs(p1[ok] - p2[ok]) / se[ok]
      perf.class[nonzero] <- z
    }
  }
  ## canonical importance
  else {
    ## conditional probability calculations
    prb1 <- frq1.full / max(1, length(idx1))
    prb2 <- frq2.full / max(1, length(idx2))
    perf.all <- mean(abs(prb2 - prb1), na.rm = TRUE)
    perf.class <- abs(prb2 - prb1)
    majority.class <- resample(which(perf.class == max(perf.class)), 1)
    perf.class[-majority.class] <- 0
  }
  ## return the performance
  c(perf.all, perf.class)
}
  ## survival
  else {
    ## bail out if subsetted y has zero length
    if (n == 0) {
      return(NA)
    }
    ## local importance 
    if (local.std) {
      ## build the survival data
      time <- c(y.org[idx1, 1], y.org[idx2, 1])
      status <- c(y.org[idx1, 2], y.org[idx2, 2])
      group <- factor(c(rep(1, length(idx1)),rep(2, length(idx2))))
      d <- cbind(time, status, group)
      ## log-rank test
      test <- tryCatch({suppressWarnings(survdiff(Surv(d[,1], d[,2]) ~ d[,3]))},
                       error=function(ex){NULL})
      if (!is.null(test)) {
        sqrt(test$chisq)
      }
      else {
        NA
      }
    }
    ## canonical importance
    else {
      abs(mean(y[idx2], na.rm = TRUE) - mean(y[idx1], na.rm = TRUE)) / sd(y, na.rm = TRUE)
    }
  }
}
