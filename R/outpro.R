outpro <- function(object,
                   newdata,
                   neighbor = NULL,
                   distancef = "prod",
                   reduce = TRUE,
                   cutoff = NULL,
                   max.rules.tree = 150,
                   max.tree = 150) {
  ## track whether user supplied newdata
  newdata.flag <- TRUE
  ## allow varpro objects or rfsrc grow objects
  if (!inherits(object, "varpro")) {
    if (!all(c("rfsrc", "grow") %in% class(object))) {
      stop("This function requires a 'varpro' object or an 'rfsrc' object with class c('rfsrc','grow').")
    } else {
      o <- object
      xvar.names <- o$xvar.names
      xorg <- object$xvar
      if (missing(newdata)) {
        newdata <- xorg
        newdata.flag <- FALSE
      } else {
        if (!all(xvar.names %in% colnames(newdata))) {
          stop("newdata does not match original training columns.")
        }
        newdata <- newdata[, xvar.names, drop = FALSE]
      }
    }
  } else {
    o <- object$rf
    xvar.names <- o$xvar.names
    xorg <- object$x
    if (missing(newdata)) {
      newdata <- xorg
      newdata.flag <- FALSE
    } else {
      newdata <- get.hotencode.test(object$x, newdata)
    }
  }
  ## set oob bit
  oob.bits <- if (newdata.flag) 1 else 0
  ## ensure cutoff
  if (is.null(cutoff)) cutoff <- out.get.cutoff(NCOL(xorg))
  ## dimension reduction: determine indices and weights on x scale
  if (is.character(reduce)) {
    whichx <- match(reduce, xvar.names)
    if (anyNA(whichx)) {
      miss <- reduce[is.na(whichx)]
      stop(paste0("reduce contains unknown variables: ", paste(miss, collapse = ", ")))
    }
    whichx.wt <- rep(1, length(whichx))
  } else if (is.numeric(reduce) && !is.null(names(reduce))) {
    whichx <- match(names(reduce), xvar.names)
    if (anyNA(whichx)) {
      miss <- names(reduce)[is.na(whichx)]
      stop(paste0("named weights contain unknown variables: ", paste(miss, collapse = ", ")))
    }
    whichx.wt <- as.numeric(reduce)
  } else if (isTRUE(reduce)) {
    if (inherits(object, "varpro")) {
      v <- get.orgvimp(object)
      reduce.names <- v$variable[v$z >= cutoff]
      reduce.wt <- v$z[v$z >= cutoff]
      if (length(reduce.names) <= 1) {
        reduce.names <- v$variable
        reduce.wt <- v$z
      }
      whichx <- match(reduce.names, xvar.names)
      whichx.wt <- reduce.wt
    } else {
      whichx <- seq_along(xvar.names)
      whichx.wt <- rep(1, length(whichx))
    }
  } else {
    whichx <- seq_along(xvar.names)
    whichx.wt <- rep(1, length(whichx))
  }
  ## neighbor handling
  if (is.null(neighbor)) neighbor <- out.get.neighbor(nrow(xorg))
  neighbor <- max(1, min(round(neighbor), nrow(xorg)))
  ## metric default
  if (is.null(distancef)) distancef <- "prod"
  ## call varpro.strength with test data option
  score <- varpro.strength(object = o,
                           newdata = newdata,
                           neighbor = neighbor,
                           reduce = whichx,
                           max.rules.tree = max.rules.tree,
                           max.tree = max.tree,
                           oob.bits = oob.bits)$score
  ## package for distance utilities
  out.object <- list(
    score = score,
    neighbor = neighbor,
    xorg = xorg[, xvar.names, drop = FALSE],
    xnew = if (newdata.flag) newdata[, xvar.names, drop = FALSE] else xorg[, xvar.names, drop = FALSE],
    xvar.names = xvar.names,
    xvar.selected = whichx,
    xvar.selected.wt = whichx.wt
  )
  ## build distance ingredients (standardize, drop zero sd)
  distance.object <- out.make.distance(out.object)
  ## compute distance and capture args used
  distance.res <- out.distance(list(distance.object = distance.object),
                               distancef = distancef)
  ## assemble return with more provenance
  res <- list(
    distance = distance.res$distance,
    distance.object = distance.object,
    distance.args = distance.res$args,
    score = score,
    neighbor = neighbor,
    cutoff = cutoff,
    oob.bits = oob.bits,
    selected.variables = distance.object$xvar.names,
    selected.weights = distance.object$xvar.wt,
    dropped.zero.sd.variables = distance.object$dropped.zero.sd.variables,
    means = distance.object$means,
    sds = distance.object$sds,
    call = match.call()
  )
  res
}
###################################################################
### Null calibration helper
###################################################################
outpro.null <- function(object,
                        nulldata = NULL,
                        neighbor = NULL,
                        distancef = "prod",
                        reduce = TRUE,
                        cutoff = .79,
                        max.rules.tree = 150,
                        max.tree = 150) {
  dots <- list()
  dots$neighbor <- neighbor
  dots$distancef <- distancef
  dots$reduce <- reduce
  dots$cutoff <- cutoff
  dots$max.rules.tree <- max.rules.tree
  dots$max.tree <- max.tree
  if (!is.null(nulldata)) {
    dots$newdata <- nulldata
  }
  op <- do.call("outpro", c(list(object), dots))
  op$cdf <- ecdf(op$distance)
  op$quantile <- op$cdf(op$distance)
  op
}
###################################################################
### Distance utilities
###################################################################
out.make.distance <- function(out) {
  ## raw neighbor frames, expect an 'id' column per case
  raw <- lapply(out$score, as.data.frame)
  ## selected variables
  xvar.names <- out$xvar.names[out$xvar.selected]
  xorg.raw <- out$xorg[, xvar.names, drop = FALSE]
  xnew.raw <- out$xnew[, xvar.names, drop = FALSE]
  ## compute means and sds on training
  means <- colMeans(xorg.raw, na.rm = TRUE)
  sds <- apply(xorg.raw, 2, sd, na.rm = TRUE)
  ## drop zero sd variables
  keep <- is.finite(sds) & (sds > 0)
  dropped <- xvar.names[!keep]
  xvar.names <- xvar.names[keep]
  if (length(xvar.names) == 0) {
    stop("All selected variables have zero standard deviation; cannot compute distances.")
  }
  means <- means[keep]
  sds <- sds[keep]
  xorg <- scale(xorg.raw[, keep, drop = FALSE], center = means, scale = sds)
  xnew <- scale(xnew.raw[, keep, drop = FALSE], center = means, scale = sds)
  ## absolute coordinate differences to neighbors in standardized space
  dist.xvar <- lapply(seq_along(xvar.names), function(j) {
    do.call(cbind, lapply(seq_len(length(raw)), function(t) {
      id <- raw[[t]][["id"]]
      xcf <- xnew[t, j]
      abs(xorg[id, j] - xcf)
    }))
  })
  ## coordinate weights from selection weights; normalize and square
  sel.wt <- out$xvar.selected.wt[keep]
  sel.wt[!is.finite(sel.wt)] <- 0
  if (max(sel.wt, na.rm = TRUE) == 0) {
    xvar.wt <- rep(1, length(sel.wt))
  } else {
    xvar.wt <- (sel.wt / max(sel.wt, na.rm = TRUE))^2
  }
  list(
    score = out$score,
    neighbor = out$neighbor,
    xvar.names = xvar.names,
    xvar.wt = xvar.wt,
    dist.xvar = dist.xvar,
    xorg.scale = xorg,
    xnew.scale = xnew,
    means = means,
    sds = sds,
    dropped.zero.sd.variables = dropped
  )
}
