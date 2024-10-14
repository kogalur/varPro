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
get.orgvimp <- function(o, papply = mclapply, pretty = TRUE, local.std = TRUE) {
  ## input value must be a varpro, cv.varpro or unsupv object
  if (!(inherits(o, "varpro") ||
        inherits(o, "cv.varpro") ||
        inherits(o, "unsupv")))  {
    stop("object must be a varpro, cv.varpro or unsupv object")
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
  vmp <- importance(o, papply = papply, local.std = local.std)
  if (o$family == "regr+") {
      vmp <- do.call(rbind, vmp)
  }
  if (o$family == "class") {
    vmp <- vmp$unconditional
  }
  ## pull original xvar names 
  xvar.org.names <- o$xvar.org.names
  ## we are finished if the data was not hotencoded
  if (!attr(o$x, "hotencode")) {
    vars <- rownames(vmp)
    vars.z <- vmp$z
  }
  ## data was hotencoded, so we need to map names appropriately
  else {
    ## we only need the rownames for vimp from the varpro object hereafter
    o <- rownames(vmp)
    ## match original variable names to varpro names which uses hot encode data
    vars <- xvar.org.names[which(unlist(lapply(xvar.org.names, function(nn) {
      if (any(grepl(nn, o))) {
        TRUE
      }
      else {
        FALSE
      }
    })))]
    ## obtain z for mapped variables
    vars.z <- lapply(xvar.org.names, function(nn) {
      if (any((pt <- grepl(nn, o)))) {
        if (!all(is.na(vmp[pt, 3]))) {
          max(vmp[pt, 3], na.rm = TRUE)
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
get.topvars <- function(o, papply = mclapply, local.std = TRUE) {
  ## input value must be a varpro or unsupv object
  if (!(inherits(o, "varpro") || inherits(o, "unsupv"))) {
    stop("object must be a varpro or unsupv object")
  }
  ## extract the vimp and names
  vmp <- importance(o, papply = papply, local.std = local.std)
  ## mv-regression
  if (o$family == "regr+") {
      vmp <- do.call(rbind, vmp)
  }
  ## classification
  if (o$family == "class") {
    vmp <- vmp$unconditional
  }
  ## return the goodies
  rownames(vmp)
}
## extract vimp
get.vimp <- function(o, papply = mclapply, pretty = TRUE, local.std = TRUE) {
  ## input value must be a varpro, cv.varpro or unsupv object
  if (!(inherits(o, "varpro") ||
        inherits(o, "cv.varpro") ||
        inherits(o, "unsupv")))  {
    stop("object must be a varpro, cv.varpro or unsupv object")
  }
  ## varpro, unsupv object
  if (inherits(o, "varpro") || inherits(o, "unsupv")) {
    ## extract the vimp and names
    vmp <- importance(o, papply = papply, local.std = local.std)
    ## mv-regression
    if (o$family == "regr+") {
      vmp <- do.call(rbind, vmp)
    }
    ## classification
    if (o$family == "class") {
    vmp <- vmp$unconditional
    }
    ##return the goodies
    if (pretty) {
      z <- vmp$z
      names(z) <- rownames(vmp)
      z[is.na(z)] <- 0
      z
    }
    else {
      ## not seleted is mapped to 0
      z <- rep(0, ncol(o$x))
      names(z) <- colnames(o$x)
      z[rownames(vmp)] <- vmp$z
      z[is.na(z)] <- 0
      z
    }
  }
  ## cv.varpro object
  else {
    ## pull the original vimp
    vmp <- attr(o, "imp.org")   
    if (attr(o, "family") == "regr+") {
      vmp <- do.call(rbind, vmp)
    }
    if (attr(o, "family") == "class") {
      vmp <- vmp$unconditional
    }
    ## threshold using cv zcut values
    v.min <- vmp[vmp$z >= o$zcut,, drop = FALSE]
    v.conserve <- vmp[vmp$z >= o$zcut.conserve,, drop = FALSE]
    v.liberal <- vmp[vmp$z >= o$zcut.liberal,, drop = FALSE]
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
      z.min[rownames(na.omit(v.min))] <- na.omit(v.min)$z
      z.conserve[rownames(na.omit(v.conserve))] <- na.omit(v.conserve)$z
      z.liberal[rownames(na.omit(v.liberal))] <- na.omit(v.liberal)$z
      data.frame(imp=z.min, imp.conserve=z.conserve, imp.liberal=z.liberal)
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
      test <- tryCatch({suppressWarnings(t.test(y[idx1],y[idx2]))},error=function(ex){NULL})
      if (!is.null(test)) {
        abs(as.numeric(test$stat))
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
  ## ---------------------------------------------------------------------
  ## classification
  ## y is a factor --> get "all" performance and J-class performance, a J+1 vector
  else if (family == "class") {
    ## number of class labels
    J <- length(levels(y))
    ## bail out if subsetted y has zero length
    if (n == 0) {
      rep(NA, 1 + J)
    }
    ## frequency counts
    y <- as.numeric(y)
    f1 <- tapply(y[idx1], y[idx1], length) 
    f2 <- tapply(y[idx2], y[idx2], length) 
    frq1 <- frq2 <- rep(0, J)
    names(frq1) <- names(frq2) <- 1:J
    ## local importance 
    if (local.std) {
      ## build frequencies for all cells, but keep track of 0/0 cells
      frq1[names(f1)] <- f1 
      frq2[names(f2)] <- f2
      nonzero <- frq1>0 | frq2>0
      frq1 <- frq1[nonzero]
      frq2 <- frq2[nonzero]
      J.nonzero <- sum(nonzero)
      ## over-all test
      test <- tryCatch({suppressWarnings(chisq.test(rbind(frq1, frq2)))},error=function(ex){NULL})
      if (!is.null(test)) {
        perf.all <- sqrt(test$stat/(max(1, J.nonzero-1)))
      }
      else {
        perf.all <- NA
      }
      ## class-specific test
      perf.class <- rep(NA, J)
      binomial.flag <- TRUE
      perf.class[nonzero] <- sapply(1:J.nonzero, function(j) {
        if (!binomial.flag) {##prop test
          counts <- matrix(c(frq1[j], frq2[j], sum(frq1) - frq1[j], sum(frq2) - frq2[j]), nrow = 2)
          test.j <- tryCatch({suppressWarnings(prop.test(counts))},error=function(ex){NULL})
          if (!is.null(test.j)) {
            sqrt(test.j$stat)
          }
          else {
            NA
          }
        }
        else {## binomial test, with separate variance
          p1 <- frq1[j] / sum(frq1)
          p2 <- frq2[j] / sum(frq2)
          p <- (frq1[j] + frq2[j]) / (sum(frq1) + sum(frq2))
          se <- sqrt(p1 * (1 - p1) / max(1, sum(frq1)) + p2 * (1 - p2) / max(1, sum(frq2)))
          #se <- sqrt(p * (1 - p) / max(1, sum(frq1)) + p * (1 - p) / max(1, sum(frq2)))
          if (se > 0) {
            abs(p1 - p2) / se
          }
          else {
            NA
          }
        }
      })
    }
    ## canonical importance
    else {
      ## conditional probability calculations
      prb1 <- prb2 <- rep(0, J)
      names(prb1) <- names(prb2) <- 1:J
      prb1[names(f1)] <- f1 / length(y[idx1])
      prb2[names(f2)] <- f2 / length(y[idx2])
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
