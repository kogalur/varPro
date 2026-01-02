###################################################################
### distance metrics for outpro
###################################################################
out.distance <- function(out,
                         distancef = c("prod",
                                       "euclidean",
                                       "mahalanobis",
                                       "manhattan",
                                       "minkowski",
                                       "kernel"),
                         weights = TRUE,
                         normalize.weights = TRUE,
                         p = 4,
                         epsilon = NULL) {
  distancef <- match.arg(distancef)
  dO <- out$distance.object
  dist.xvar <- dO$dist.xvar
  dims <- dim(dist.xvar[[1]])
  k <- length(dist.xvar)
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
