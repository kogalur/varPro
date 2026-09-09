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
  if (!(inherits(o, "varpro") || inherits(o, "cv.varpro") ||
        inherits(o, "uvarpro"))) {
    stop("object must be a varpro, cv.varpro or uvarpro object")
  }
  if (inherits(o, "cv.varpro")) {
    if (pretty) return(o)
    nms <- attr(o, "xvar.org.names")
    out <- lapply(c("imp", "imp.conserve", "imp.liberal"), function(nm) {
      z <- setNames(numeric(length(nms)), nms)
      v <- o[[nm]]
      if (!is.null(v) && nrow(v) > 0L) {
        z[v$variable] <- v$z
      }
      z[is.na(z)] <- 0
      z
    })
    names(out) <- c("imp", "imp.conserve", "imp.liberal")
    return(as.data.frame(out))
  }
  ## Reuse a supplied summary, including its local-standardization settings.
  if (is.null(vmp)) vmp <- importance(o, local.std = local.std)
  v <- .get.vimp.rows(vmp, o$family)
  original <- o$xvar.org.names
  map <- .get.hotencode.map(o$x)
  pos <- match(v$names, names(map))
  if (anyNA(pos)) {
    stop("importance variable names do not match the processed predictors")
  }
  source <- unname(map[pos])
  if (!all(source %in% original)) {
    stop("hot-encoding map does not match the original predictors")
  }
  ## Keep variable identity separate from row names when pooling outcomes.
  vars <- original[original %in% source]
  z <- vapply(vars, function(nn) {
    values <- v$z[source == nn]
    if (all(is.na(values))) 0 else max(values, na.rm = TRUE)
  }, numeric(1))
  if (pretty) {
    out <- data.frame(variable = vars, z = unname(z))
    out[order(out$z, decreasing = TRUE), , drop = FALSE]
  } else {
    out <- setNames(numeric(length(original)), original)
    out[vars] <- z
    out
  }
}
## Flatten an importance summary without relying on rbind row-name suffixes.
## Retain all score columns, the exact predictor name, and outcome identity.
.get.vimp.rows <- function(vmp, family) {
  if (family == "class") vmp <- vmp$unconditional
  summaries <- if (family == "regr+") vmp else list(vmp)
  rows <- lapply(seq_along(summaries), function(j) {
    v <- as.data.frame(summaries[[j]])
    if (!("z" %in% names(v))) stop("importance summary must contain a 'z' column")
    v$names <- rownames(v)
    v$outcome <- rep.int(j, nrow(v))
    v
  })
  if (!length(rows)) {
    return(data.frame(z = numeric(), names = character(), outcome = integer()))
  }
  do.call(rbind, rows)
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
  if (!(inherits(o, "varpro") || inherits(o, "cv.varpro") ||
        inherits(o, "uvarpro"))) {
    stop("object must be a varpro, cv.varpro or uvarpro object")
  }
  is.cv <- inherits(o, "cv.varpro")
  family <- if (is.cv) attr(o, "family") else o$family
  vmp <- if (is.cv) attr(o, "imp.org") else importance(o, local.std = local.std)
  outcomes <- if (family == "regr+") seq_along(vmp) else 1L
  vmp <- .get.vimp.rows(vmp, family)
  result <- lapply(outcomes, function(j) {
    v <- vmp[vmp$outcome == j, , drop = FALSE]
    rownames(v) <- v$names
    if (!is.cv) {
      if (pretty) {
        z <- setNames(v$z, v$names)
      } else {
        z <- setNames(numeric(ncol(o$x)), colnames(o$x))
        z[v$names] <- v$z
      }
      z[is.na(z)] <- 0
      return(z)
    }
    cuts <- c(o$zcut, o$zcut.conserve, o$zcut.liberal)
    selections <- c("imp", "imp.conserve", "imp.liberal")
    values <- lapply(seq_along(selections), function(k) {
      ## A NULL/empty selection remains empty even when its cutoff is zero.
      active <- !is.null(o[[selections[k]]]) && NROW(o[[selections[k]]]) > 0L
      cv.info <- attr(o[[selections[k]]], "cv")
      if (active && isTRUE(cv.info$fallback)) {
        ## A forced one-variable model is defined by its original-variable
        ## identity, not by a cutoff (which could also select tied variables).
        map <- attr(o, "xvar.map")
        pos <- match(v$names, names(map))
        if (is.null(map) || anyNA(pos)) {
          stop("CV fallback extraction requires the encoded-to-original variable map")
        }
        keep <- which(!is.na(v$z) &
                      unname(map[pos]) %in% o[[selections[k]]]$variable)
      } else {
        keep <- if (active) which(!is.na(v$z) & v$z >= cuts[k]) else integer()
      }
      if (pretty) {
        v[keep, setdiff(names(v), c("names", "outcome")), drop = FALSE]
      } else {
        nms <- attr(o, "xvar.names")
        z <- setNames(numeric(length(nms)), nms)
        z[v$names[keep]] <- v$z[keep]
        z
      }
    })
    names(values) <- selections
    if (pretty) values else as.data.frame(values)
  })
  names(result) <- as.character(outcomes)
  ## Match the existing single-response and multivariate return structures.
  if (!is.cv && family == "regr+" && pretty) return(result)
  if (length(result) == 1L) result[[1L]] else result
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
