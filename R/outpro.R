outpro <- function(object,
                   newdata,
                   neighbor = NULL,
                   distancef = "knn",
                   reduce = TRUE,
                   cutoff = NULL,
                   max.rules.tree = 150,
                   max.tree = 150,
                   knn.chunk.size = 100L,
                   newdata.xscale = FALSE) {
  ## track whether user supplied newdata
  newdata.flag <- TRUE
  ## newdata.xscale is an internal/package-developer option for
  ## varpro objects.  When TRUE, newdata is assumed to already be on
  ## the fitted varPro x-scale, i.e. already hot-encoded and aligned
  ## to object$x / object$rf$xvar.names.
  newdata.xscale <- isTRUE(newdata.xscale)
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
    if (!all(xvar.names %in% colnames(xorg))) {
      stop("fitted forest variables are not found in object$x.")
    }
    if (missing(newdata)) {
      newdata <- xorg
      newdata.flag <- FALSE
    } else {
      if (newdata.xscale) {
        if (!all(xvar.names %in% colnames(newdata))) {
          stop("newdata.xscale = TRUE requires newdata to contain the fitted x-scale columns.")
        }
        newdata <- newdata[, xvar.names, drop = FALSE]
      } else {
        newdata <- get.hotencode.test(object$x, newdata)
      }
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
  distance.choices <- c("prod",
                        "euclidean",
                        "mahalanobis",
                        "manhattan",
                        "minkowski",
                        "kernel",
                        "knn")
  if (is.null(distancef)) distancef <- "knn"
  distancef <- match.arg(distancef, distance.choices)
  ## call varpro.strength with test data option
  ## distancef = "knn" uses an ordinary nearest-neighbor reference set
  ## in the selected subspace and therefore does not require the
  ## forest-derived neighbor frames.
  if (identical(distancef, "knn")) {
    score <- NULL
  } else {
    score <- varpro.strength(object = o,
                             newdata = newdata,
                             neighbor = neighbor,
                             reduce = whichx,
                             max.rules.tree = max.rules.tree,
                             max.tree = max.tree,
                             oob.bits = oob.bits)$score
  }
  ## package for distance utilities
  out.object <- list(
    score = score,
    neighbor = neighbor,
    xorg = xorg[, xvar.names, drop = FALSE],
    xnew = if (newdata.flag) newdata[, xvar.names, drop = FALSE] else xorg[, xvar.names, drop = FALSE],
    xvar.names = xvar.names,
    xvar.selected = whichx,
    xvar.selected.wt = whichx.wt,
    oob.bits = oob.bits
  )
  ## build distance ingredients (standardize, drop zero sd)
  distance.object <- out.make.distance(out.object)
  ## compute distance and capture args used
  distance.call <- list(out = list(distance.object = distance.object),
                        distancef = distancef)
  if (identical(distancef, "knn")) {
    distance.call$knn.chunk.size <- knn.chunk.size
  }
  distance.res <- do.call(out.distance, distance.call)
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
    newdata.xscale = newdata.xscale,
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
                        distancef = "knn",
                        reduce = TRUE,
                        cutoff = NULL,
                        max.rules.tree = 150,
                        max.tree = 150,
                        knn.chunk.size = 100L,
                        nulldata.xscale = FALSE) {
  dots <- list()
  dots$neighbor <- neighbor
  dots$distancef <- distancef
  dots$reduce <- reduce
  dots$cutoff <- cutoff
  dots$max.rules.tree <- max.rules.tree
  dots$max.tree <- max.tree
  dots$knn.chunk.size <- knn.chunk.size
  dots$newdata.xscale <- nulldata.xscale
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
  ## raw neighbor frames, expect an 'id' column per case.  These are
  ## absent for distancef = "knn", where the reference set is ordinary
  ## nearest neighbors in the selected subspace.
  has.score <- !is.null(out$score)
  raw <- if (has.score) lapply(out$score, as.data.frame) else NULL
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
  ## absolute coordinate differences to forest-selected neighbors in
  ## standardized space.  These are not needed for distancef = "knn".
  dist.xvar <- if (has.score) {
    lapply(seq_along(xvar.names), function(j) {
      do.call(cbind, lapply(seq_len(length(raw)), function(t) {
        id <- raw[[t]][["id"]]
        xcf <- xnew[t, j]
        abs(xorg[id, j] - xcf)
      }))
    })
  } else {
    NULL
  }
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
    oob.bits = if (!is.null(out$oob.bits)) out$oob.bits else NA_integer_,
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
