###################################################################
### distance metrics for outpro
###################################################################
out.distance <- function(out,
                         distancef = c("prod",
                                       "euclidean",
                                       "mahalanobis",
                                       "manhattan",
                                       "minkowski",
                                       "kernel",
                                       "knn"),
                         weights = TRUE,
                         normalize.weights = TRUE,
                         p = 4,
                         epsilon = NULL,
                         knn.chunk.size = 100L) {
  distancef <- match.arg(distancef)
  dO <- out$distance.object
  if (identical(distancef, "knn")) {
    dist.xvar <- NULL
    dims <- NULL
    k <- ncol(dO$xorg.scale)
  } else {
    dist.xvar <- dO$dist.xvar
    if (is.null(dist.xvar) || length(dist.xvar) == 0) {
      stop("forest-neighborhood distance ingredients are missing")
    }
    dims <- dim(dist.xvar[[1]])
    k <- length(dist.xvar)
  }
  ## weights handling
  if (is.logical(weights)) {
    if (weights) {
      weights <- dO$xvar.wt
    } else {
      weights <- rep(1, k)
    }
  } else if (is.null(weights)) {
    weights <- rep(1, k)
  }
  if (length(weights) != k) stop("length of weights does not match number of variables")
  if (normalize.weights) {
    s <- sum(weights)
    weights <- if (s > 0) weights / s else rep(1 / k, k)
  }
  if (distancef == "knn") {
    dist.vec <- out.knn.distance(dO, weights, chunk.size = knn.chunk.size)
    return(list(
      distance = dist.vec,
      args = list(
        distancef = distancef,
        weights.used = weights,
        normalize.weights = normalize.weights,
        p = p,
        epsilon.used = NA_real_,
        knn.neighbor.used = attr(dist.vec, "knn.neighbor.used"),
        knn.self.excluded = attr(dist.vec, "knn.self.excluded"),
        knn.chunk.size = attr(dist.vec, "knn.chunk.size")
      )
    ))
  }
  ## automatic epsilon for prod, based on standardized absolute deltas
  if (is.null(epsilon) && identical(distancef, "prod")) {
    all.delta <- unlist(dist.xvar, use.names = FALSE)
    med.delta <- median(all.delta, na.rm = TRUE)
    epsilon <- max(.Machine$double.eps, med.delta * 1e-6)
  }
  ## Distance calculation
  if (distancef == "prod") {
    dist <- (abs(dist.xvar[[1]]) + epsilon)^weights[1]
    if (k > 1) {
      for (j in 2:k) {
        dist <- dist * ((abs(dist.xvar[[j]]) + epsilon)^weights[j])
      }
    }
  } else if (distancef == "euclidean") {
    dist <- (weights[1] * dist.xvar[[1]]^2)
    if (k > 1) {
      for (j in 2:k) {
        dist <- dist + weights[j] * dist.xvar[[j]]^2
      }
    }
    dist <- sqrt(dist)
  } else if (distancef == "mahalanobis") {
    ## absolute deltas by design
    A <- do.call(cbind, lapply(dist.xvar, as.vector))  ## (m*n) by p, entries >= 0
    ## incorporate weights via column scaling
    if (!is.null(weights)) {
      A <- sweep(A, 2, sqrt(weights), `*`)
    }
    ## covariance of standardized training features
    covmat <- cov(dO$xorg.scale)
    ## light ridge for stability
    if (ncol(covmat) > 0) {
      covmat <- covmat + diag(.Machine$double.eps, ncol(covmat))
    }
    inv.cov <- chol2inv(chol(covmat))
    dvec <- sqrt(rowSums((A %*% inv.cov) * A))
    dist <- matrix(dvec, nrow = dims[1], ncol = dims[2])
  } else if (distancef == "manhattan") {
    dist <- weights[1] * abs(dist.xvar[[1]])
    if (k > 1) {
      for (j in 2:k) {
        dist <- dist + weights[j] * abs(dist.xvar[[j]])
      }
    }
  } else if (distancef == "minkowski") {
    dist <- weights[1] * abs(dist.xvar[[1]])^p
    if (k > 1) {
      for (j in 2:k) {
        dist <- dist + weights[j] * abs(dist.xvar[[j]])^p
      }
    }
    dist <- dist^(1 / p)
  } else if (distancef == "kernel") {
    dist2 <- weights[1] * dist.xvar[[1]]^2
    if (k > 1) {
      for (j in 2:k) {
        dist2 <- dist2 + weights[j] * dist.xvar[[j]]^2
      }
    }
    sigma2 <- median(dist2, na.rm = TRUE)
    sigma2 <- max(sigma2, .Machine$double.eps)
    dist <- 1 - colMeans(exp(-dist2 / (2 * sigma2)), na.rm = TRUE)
  } else {
    stop("unsupported distance type")
  }
  ## aggregate across neighbors to return one number per case
  if (is.matrix(dist)) {
    dist.vec <- drop(colMeans(dist, na.rm = TRUE))
  } else {
    dist.vec <- dist
  }
  list(
    distance = dist.vec,
    args = list(
      distancef = distancef,
      weights.used = weights,
      normalize.weights = normalize.weights,
      p = p,
      epsilon.used = if (!is.null(epsilon)) epsilon else NA_real_
    )
  )
}
out.knn.distance <- function(dO, weights, chunk.size = 100L) {
  xorg <- as.matrix(dO$xorg.scale)
  xnew <- as.matrix(dO$xnew.scale)
  if (ncol(xorg) != length(weights)) {
    stop("length of weights does not match number of variables")
  }
  n.org <- nrow(xorg)
  n.new <- nrow(xnew)
  exclude.self <- isTRUE(dO$oob.bits == 0) && n.new == n.org
  neighbor <- dO$neighbor
  if (is.null(neighbor) || !is.finite(neighbor)) {
    neighbor <- out.get.neighbor(n.org)
  }
  max.neighbor <- n.org - as.integer(exclude.self)
  if (max.neighbor < 1L) {
    stop("cannot compute KNN score with fewer than two training cases")
  }
  k <- as.integer(round(neighbor))
  k <- max(1L, min(k, max.neighbor))
  chunk.size <- as.integer(chunk.size)
  if (length(chunk.size) != 1L || is.na(chunk.size) || chunk.size < 1L) {
    stop("chunk.size must be a positive integer")
  }
  score <- rep(NA_real_, n.new)
  for (st in seq.int(1L, n.new, by = chunk.size)) {
    en <- min(n.new, st + chunk.size - 1L)
    idx <- st:en
    dmat <- matrix(0, nrow = length(idx), ncol = n.org)
    for (j in seq_len(ncol(xorg))) {
      dmat <- dmat + weights[j] *
        abs(outer(xnew[idx, j], xorg[, j], "-"))
    }
    if (exclude.self) {
      dmat[cbind(seq_along(idx), idx)] <- Inf
    }
    score[idx] <- apply(dmat, 1L, function(z) {
      mean(sort(z, partial = k)[seq_len(k)], na.rm = TRUE)
    })
  }
  attr(score, "knn.neighbor.used") <- k
  attr(score, "knn.self.excluded") <- exclude.self
  attr(score, "knn.chunk.size") <- chunk.size
  score
}
###################################################################
### Helper functions
###################################################################
out.get.neighbor <- function(n, nmax = 5000) {
  min(n / 10, nmax)
}
out.get.cutoff <- function(p, pmax = 250) {
  if (p > pmax) {
    cutoff <- 0
  } else {
    cutoff <- .79
  }
  cutoff
}
