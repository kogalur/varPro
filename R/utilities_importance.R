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
get.orgvimp <- function(o, papply = mclapply, pretty = TRUE) {
  ## input value must be a varpro, cv.varpro or unsupv object
  if (!(inherits(o, "varpro", TRUE) ||
        inherits(o, "cv.varpro", TRUE) ||
        inherits(o, "unsupv", TRUE) ))  {
    stop("object must be a varpro, cv.varpro or unsupv object")
  }
  ## first deal with cv.varpro since it's already encoded for original variables
  ## (to get hot-encoded importance we use get.vimp()
  if (inherits(o, "cv.varpro", TRUE)) {
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
  vmp <- importance(o, papply = papply)
  if (o$family == "regr+") {
      vmp <- do.call(rbind, vmp)
  }
  if (o$family == "class") {
    vmp <- vmp$unconditional
  }
  ## we are finished if the data was not hotencoded
  if (!attr(o$x, "hotencode")) {
    vars <- rownames(vmp)
    vars.z <- vmp$z
  }
  ## data was hotencoded, so we need to map names appropriately
  else {
    ## pull xvar names (original and hot-encoded)
    xvar.org.names <- o$xvar.org.names
    xvar.names <- o$xvarnames
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
get.topvars <- function(o, papply = mclapply) {
  ## input value must be a varpro or unsupv object
  if (!(inherits(o, "varpro", TRUE) || inherits(o, "unsupv", TRUE))) {
    stop("object must be a varpro or unsupv object")
  }
  ## extract the vimp and names
  vmp <- importance(o, papply = papply)
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
get.vimp <- function(o, papply = mclapply, pretty = TRUE) {
  ## input value must be a varpro, cv.varpro or unsupv object
  if (!(inherits(o, "varpro", TRUE) ||
        inherits(o, "cv.varpro", TRUE) ||
        inherits(o, "unsupv", TRUE) ))  {
    stop("object must be a varpro, cv.varpro or unsupv object")
  }
  ## varpro, unsupv object
  if (inherits(o, "varpro", TRUE) || inherits(o, "unsupv", TRUE)) {
    ## extract the vimp and names
    vmp <- importance(o, papply = papply)
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
