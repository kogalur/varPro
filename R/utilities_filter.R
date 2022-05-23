## filter data using varpro
filter.data  <- function(f, data, cutoff = 2, ...) {
  # list of varpro parameters
  vnames <- get.varpro.names(hidden = TRUE)
  ## restrict to allowed values
  vnames <- vnames[vnames != "f" & vnames != "data"]
  ## get the permissible hidden options
  dots <- list(...)
  dots <- dots[names(dots) %in% vnames]
  ## call varpro
  o <- do.call("varpro", c(list(f = f, data = data), dots))
  ## call importance
  imp <- importance(o, cutoff = cutoff)
  ## identify the significant variables
  imp[is.na(imp)] <- 0
  if (sum(imp$selected == 1, na.rm = TRUE) > 0) {
    imp <- imp[imp$selected == 1,, drop = FALSE]
  }
  else {
    if (sum(imp$mean > 0, na.rm = TRUE) > 0) {
      imp <- imp[imp$mean > 0,, drop = FALSE]
    }
  }
  xvar.names <- o$xvar.names[o$xvar.names %in% rownames(imp)]
  ## return the filtered data
  data.frame(o$y.org, data[, xvar.names, drop = FALSE])
}
## select variables using varpro
select  <- function(f, data, cutoff = 2, z = TRUE, ...) {
  # list of varpro parameters
  vnames <- get.varpro.names(hidden = TRUE)
  ## restrict to allowed values
  vnames <- vnames[vnames != "f" & vnames != "data"]
  ## get the permissible hidden options
  dots <- list(...)
  dots <- dots[names(dots) %in% vnames]
  ## call varpro
  o <- do.call("varpro", c(list(f = f, data = data), dots))
  ## call importance
  imp <- importance(o, cutoff = cutoff)
  imp[is.na(imp)] <- 0
  ## return z, or return only the selected variables?
  if (z) {
    all.zero <- apply(imp, 1, function(x) {all(x==0)})
    zv <- imp[!all.zero, "z", drop = FALSE]
    setNames(zv[[1]], rownames(zv))
  }
  ## identify the significant variables
  else {
    if (sum(imp$selected == 1, na.rm = TRUE) > 0) {
      imp <- imp[imp$selected == 1,, drop = FALSE]
    }
    else {
      if (sum(imp$mean > 0, na.rm = TRUE) > 0) {
        imp <- imp[imp$mean > 0,, drop = FALSE]
      }
    }
    xvar.names <- o$xvar.names[o$xvar.names %in% rownames(imp)]
    ## return the top vars
    xvar.names
  }
}
## augment data with hyperplane variables
make.hp <- function(f, dta, subset = NULL, nterms = 2) {
  ynm <- all.vars(f)[1]
  y <- dta[, ynm]
  dta <- data.matrix(dta[, colnames(dta) != ynm])
  if (is.null(subset)) {
    subset <- 1:ncol(dta)
  }
  nm <- c(ynm, colnames(dta))
  if (length(subset) >= 2) {
    two <- do.call(cbind, lapply(combn(subset, 2, simplify = FALSE), function(j) {
      nm <<- c(nm, paste("two.", paste(j, collapse = "."), sep = ""))
      dta[, j[1]] + dta[, j[2]] 
    }))
    dtamod <- data.frame(y = y, dta, two)
    colnames(dtamod) <- nm
  }
  else {
    dtamod <- data.frame(y = y, dta)
  }
  if (length(subset) >= 3 && nterms > 2) {
    nm <- NULL
    three <- do.call(cbind, lapply(combn(subset, 3, simplify = FALSE), function(j) {
      nm <<- c(nm, paste("three.", paste(j, collapse = "."), sep = ""))
      dta[, j[1]] + dta[, j[2]] + dta[, j[3]] 
    }))
    colnames(three) <- nm
    dtamod <- data.frame(dtamod, three)
  }
  dtamod
}
## augment data with vitrual twins (vt) interactions
make.vt <- function(f, dta, subset = NULL, interact = 2) {
  ynm <- all.vars(f)[1]
  y <- dta[, ynm]
  dta <- data.matrix(dta[, colnames(dta) != ynm])
  if (is.null(subset)) {
    subset <- 1:ncol(dta)
  }
  nm <- c(ynm, colnames(dta))
  if (length(subset) >= 2) {
    two <- do.call(cbind, lapply(combn(subset, 2, simplify = FALSE), function(j) {
      nm <<- c(nm, paste("two.", paste(j, collapse = "."), sep = ""))
      dta[, j[1]] * dta[, j[2]] 
    }))
    dtamod <- data.frame(y = y, dta, two)
    colnames(dtamod) <- nm
  }
  else {
    dtamod <- data.frame(y = y, dta)
  }
  if (length(subset) >= 3 && interact > 2) {
    nm <- NULL
    three <- do.call(cbind, lapply(combn(subset, 3, simplify = FALSE), function(j) {
      nm <<- c(nm, paste("three.", paste(j, collapse = "."), sep = ""))
      dta[, j[1]] * dta[, j[2]] * dta[, j[3]] 
    }))
    colnames(three) <- nm
    dtamod <- data.frame(dtamod, three)
  }
  dtamod
}
