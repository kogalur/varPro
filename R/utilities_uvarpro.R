####################################################################
##
##
## default entropy functions
##
##
####################################################################
entropy.ssq <- function(xC, xO) {
  wss <- mean(apply(rbind(xO, xC), 2, sd, na.rm = TRUE))
  bss <- mean(apply(xC, 2, sd, na.rm = TRUE)) + mean(apply(xO, 2, sd, na.rm = TRUE))
  0.5 * bss / wss
}
entropy.default <- function(xC, xO, alpha = .025, beta = FALSE, ...) {
  imp <- entropy.ssq(xC, xO)
  dots <- list(...)
  list(imp = imp, membership = list(comp = dots$compMembership, oob = dots$oobMembership))
}
get.entropy.default <- function(entropy.values, xvar.names, ...) {
  entropy.values
}
##################################################################
##
##
## helper functions used for custom lasso entropy (below)
##
##
##################################################################
fit.logistic.cv.beta <- function(X.cls, class, nfolds, parallel, maxit, thresh) {
  ## !!!manual scaling!!!
  X.cls <- scale(X.cls, center = FALSE)
  o.cv <- tryCatch(
    suppressWarnings(glmnet::cv.glmnet(
      as.matrix(X.cls), class,
      family  = "binomial",
      nfolds  = nfolds,
      parallel= parallel,  
      maxit   = maxit,
      thresh  = thresh
    )),
    error = function(e) NULL
  )
  if (is.null(o.cv)) return(NULL)
  ## be explicit about s for robustness (same as cv.glmnet default)
  co <- tryCatch(as.matrix(stats::coef(o.cv, s = "lambda.1se")),
                 error = function(e) NULL)
  if (is.null(co) || nrow(co) <= 1L) return(NULL)
  bhat <- abs(co[-1, 1])
  names(bhat) <- rownames(co)[-1]
  bhat
}
last.lambda.coef <- function(fit) {
  lam.last <- utils::tail(fit$lambda, 1L)
  tryCatch(as.matrix(stats::coef(fit, s = lam.last)),
           error = function(e) NULL)
}
fit.linear.lasso.at.last <- function(X, Y, add.zero = FALSE, zero.name = "ZeroColumn") {
  ## !!!!manual scaling!!!! 
  X <- scale(X, center = FALSE)
  if (add.zero) {
    X <- cbind(rep(0, nrow(X)), X)
    colnames(X)[1] <- zero.name
  }
  fit <- tryCatch(suppressWarnings(glmnet::glmnet(X, Y, alpha = 1)),
                  error = function(e) NULL)
  if (is.null(fit)) return(NULL)
  co <- last.lambda.coef(fit)
  if (is.null(co) || nrow(co) <= 1L) return(NULL)
  drop.idx <- if (add.zero) c(1L, 2L) else 1L
  v <- abs(co[-drop.idx, 1])
  names(v) <- rownames(co)[-drop.idx]
  v
}
## Generalized workhorse (can target any predictor universe)
## - If ret.data = TRUE, returns list(beta = <vector>, dt = <matrix>, rel.col.fit = <int>)
## - Otherwise returns just the beta vector (padded to 'predictor.universe')
beta.workhorse <- function(releaseX,
                           rule,
                           xorg,
                           predictor.universe = colnames(xorg),
                           nfolds = 5,
                           parallel = FALSE,
                           maxit = 2500,
                           thresh = 1e-3,
                           ret.data = FALSE) {
  if (length(predictor.universe) == 0L) return(NULL)
  rel.col.fit <- match(releaseX, predictor.universe)
  if (is.na(rel.col.fit)) return(NULL)  # release not in this universe
  idx.C <- rule[[1]]
  idx.O <- rule[[2]]
  if (length(idx.C) == 0L || length(idx.O) == 0L) return(NULL)
  idx <- c(idx.C, idx.O)
  dt  <- xorg[idx, predictor.universe, drop = FALSE]
  nC  <- length(idx.C); nO <- length(idx.O)
  ## logistic step
  X.cls <- dt[, -rel.col.fit, drop = FALSE]
  class <- factor(c(rep(0, nC), rep(1, nO)))
  bhat  <- fit.logistic.cv.beta(X.cls, class, nfolds, parallel, maxit, thresh)
  if (is.null(bhat)) return(NULL)
  ## embed into a vector over the predictor universe (release variable stays zero)
  beta <- setNames(numeric(length(predictor.universe)), predictor.universe)
  hit  <- intersect(names(bhat), predictor.universe)
  if (length(hit)) beta[hit] <- bhat[hit]
  if (ret.data) {
    return(list(beta = beta, dt = dt, rel.col.fit = rel.col.fit))
  } else {
    return(beta)
  }
}
## Backward-compatible wrapper that preserves the original API
## (delegates to the generalized helper over the full column set)
get.beta.workhorse <- function(releaseX,
                               rule,
                               xorg,
                               parallel = FALSE,
                               nfolds = 10,
                               maxit = 2500,
                               thresh = 1e-3) {
  beta.workhorse(releaseX = releaseX,
                 rule = rule,
                 xorg = xorg,
                 predictor.universe = colnames(xorg),
                 nfolds = nfolds,
                 parallel = parallel,
                 maxit = maxit,
                 thresh = thresh,
                 ret.data = FALSE)
}
####################################################################
##
##
## custom entropy
## classification analysis for rule region versus complementary region
##
##
####################################################################
custom.entropy <- function(o,
                           papply = mclapply,
                           pre.filter = FALSE,
                           second.stage = FALSE,
                           vimp.min = 0, 
                           nfolds = 5,
                           maxit = 2500,
                           thresh = 1e-3,
                           parallel = FALSE) {
  if (is.null(o$x) || is.null(o$entropy)) {
    stop("Object 'o' must contain x and entropy.")
  }
  x.nms.all <- colnames(o$x)
  rel.candidates <- names(o$entropy)
  ## optional VIMP pre-filter (shrinks rows and cols)
  if (pre.filter) {
    vmp <- tryCatch(get.vimp(o, pretty = FALSE), error = function(e) NULL)
    if (is.null(vmp)) {
      x.nms.fit <- x.nms.all
      rel.vars  <- rel.candidates
    } else {
      xvars <- names(vmp[vmp > vimp.min])
      xvars <- intersect(xvars, x.nms.all)
      rel.vars <- intersect(xvars, rel.candidates)
      if (length(xvars) == 0L || length(rel.vars) == 0L) {
        return(list(info.rule = list(),
                    beta.mean.mat = NULL,
                    lasso.mean.mat = NULL))
      }
      x.nms.fit <- xvars
    }
  } else {
    x.nms.fit <- x.nms.all
    rel.vars  <- rel.candidates
  }
  ## ------------- outer loop over release variables -------------
  mat.list <- papply(rel.vars, function(x.release) {
    if (length(setdiff(x.nms.fit, x.release)) == 0L) return(NULL)
    oo <- o$entropy[[x.release]]
    if (length(oo) == 0L) return(NULL)
    ## ------------- inner loop over rules -------------## 
    rO <- lapply(oo, function(rule) {
      ## delegate logistic stage to the helper
      res <- beta.workhorse(releaseX = x.release,
                            rule = rule,
                            xorg = o$x,
                            predictor.universe = x.nms.fit,
                            nfolds = nfolds,
                            parallel = parallel,
                            maxit = maxit,
                            thresh = thresh,
                            ret.data = second.stage)  # only return dt if we need stage 2
      if (is.null(res)) return(NULL)
      if (second.stage) {
        beta <- res$beta
        dt   <- res$dt
        rel.col.fit <- res$rel.col.fit
        coef.lasso <- setNames(numeric(length(x.nms.fit)), x.nms.fit)
        nz.names <- names(beta)[beta != 0]
        if (length(nz.names)) {
          Y <- dt[, rel.col.fit]  # same as o$x[idx, x.release]; dt already aligned
          if (length(nz.names) > 1L) {
            v <- fit.linear.lasso.at.last(dt[, nz.names, drop = FALSE], Y, add.zero = FALSE)
            if (!is.null(v)) {
              hit2 <- intersect(names(v), x.nms.fit)
              if (length(hit2)) coef.lasso[hit2] <- v[hit2]
            }
          } else {
            v <- fit.linear.lasso.at.last(dt[, nz.names, drop = FALSE], Y, add.zero = TRUE)
            if (!is.null(v) && nz.names %in% names(v)) {
              coef.lasso[nz.names] <- v[nz.names]
            }
          }
        }
      } else {
        beta <- res
        coef.lasso <- setNames(numeric(length(x.nms.fit)), x.nms.fit)
      }
      list(beta = beta, lasso = coef.lasso)
    })
    rO <- Filter(Negate(is.null), rO)
    if (!length(rO)) return(NULL)
    beta.mat  <- do.call(rbind, lapply(rO, function(z) z$beta))
    lasso.mat <- do.call(rbind, lapply(rO, function(z) z$lasso))
    list(beta = beta.mat, lasso = lasso.mat)
  })
  names(mat.list) <- rel.vars
  mat.list <- mat.list[!vapply(mat.list, is.null, logical(1))]
  if (!length(mat.list)) {
    return(list(info.rule = list(),
                beta.mean.mat = NULL,
                lasso.mean.mat = NULL))
  }
  ## assemble per-release matrices
  beta.list  <- lapply(mat.list, `[[`, "beta")
  lasso.list <- lapply(mat.list, `[[`, "lasso")
  ## average the beta and lasso values (full precision; no rounding)
  beta.mean.mat  <- do.call(rbind, lapply(beta.list,  function(x) colMeans(x, na.rm = TRUE)))
  lasso.mean.mat <- do.call(rbind, lapply(lasso.list, function(x) colMeans(x, na.rm = TRUE)))
  ## standardized lasso: divide each row by sd(Y) so entries are (SDs of Y) per 1 SD of X
  if (!is.null(lasso.mean.mat) && nrow(lasso.mean.mat) > 0L) {
    rel.vars <- rownames(lasso.mean.mat)
    sd.y <- vapply(rel.vars,
                   function(v) stats::sd(o$x[, v], na.rm = TRUE),
                   FUN.VALUE = numeric(1))
    ## avoid divide-by-zero or non-finite sds
    sd.y[!is.finite(sd.y) | sd.y == 0] <- NA_real_
    lasso.mean.std.mat <- sweep(lasso.mean.mat, 1, sd.y, "/")
  } else {
    lasso.mean.std.mat <- lasso.mean.mat  # NULL or 0x0 passes through
  }
  ## return values
  list(
    info.rule             = mat.list,
    beta.mean.mat         = beta.mean.mat,
    lasso.mean.mat        = lasso.mean.mat,
    lasso.mean.std.mat    = lasso.mean.std.mat
  )
}
## convenience function
get.beta.entropy <- function(o,
                             second.stage = FALSE,
                             pre.filter = TRUE,
                             papply = mclapply,
                             vimp.min = 0,
                             nfolds = 10,
                             maxit = 2500,
                             thresh = 1e-3,
                             parallel = FALSE) {
  out <- custom.entropy(o,
                        papply = papply,
                        pre.filter = pre.filter,
                        second.stage = second.stage,
                        vimp.min = vimp.min,
                        nfolds = nfolds,
                        maxit = maxit,
                        thresh = thresh,
                        parallel = parallel)
  if (second.stage) {
    out$lasso.mean.std.mat
  }
  else {
    out$beta.mean.mat
  }
}
####################################################################
##
##
## generalized inverse regression
## legacy 
##
##
####################################################################
ginvResidual <- function (y, x, tol = sqrt(.Machine$double.eps)) {
  x <- as.matrix(cbind(1, x))
  xsvd <- svd(x)
  Positive <- xsvd$d > max(tol * xsvd$d[1L], 0)
  if (all(Positive)) {
    ginv <- xsvd$v %*% (1/xsvd$d * t(xsvd$u))
  }
  else if (!any(Positive)) {
    ginv <- array(0, dim(x)[2L:1L])
  }
  else {
    ginv <- xsvd$v[, Positive, drop = FALSE] %*% ((1/xsvd$d[Positive]) * 
       t(xsvd$u[, Positive, drop = FALSE]))
  }
  c(y - x %*% (ginv %*% y))
}
ginvMLS <- function (y, x, tol = sqrt(.Machine$double.eps)) {
  x <- as.matrix(cbind(1, x))
  xsvd <- svd(x)
  Positive <- xsvd$d > max(tol * xsvd$d[1L], 0)
  if (all(Positive)) {
    ginv <- xsvd$v %*% (1/xsvd$d * t(xsvd$u))
  }
  else if (!any(Positive)) {
    ginv <- array(0, dim(x)[2L:1L])
  }
  else {
    ginv <- xsvd$v[, Positive, drop = FALSE] %*% ((1/xsvd$d[Positive]) * 
       t(xsvd$u[, Positive, drop = FALSE]))
  }
  ## return the MLS coefficients (remove intercept)
  c(ginv %*% y)[-1] 
}
####################################################################
##
##
## custom nodesize for unsupervised setting
## typically much larger than varpro
##
##
####################################################################
set.legacy.unsupervised.nodesize <- function(n, p, nodesize = NULL) {
  if (is.null(nodesize)) {
    if (n <= 300 & p > n) {
      nodesize <- 2
    }
    else if (n <= 300 & p <= n) {
      nodesize <- min(n / 4, 20)
    }
    else if (n > 300 & n <= 2000) {
      nodesize <- 40
    }
    else {
      nodesize <- n / 50
    }
  }
  nodesize
}
set.unsupervised.nodesize <- function(n, p, nodesize = NULL) {
  if (is.null(nodesize)) {
    if (n < 100) {
      nodesize <- n / 10
    }
    else {
      nodesize <- max(n / 10, 20)
    }
  }
  nodesize
}
####################################################################
##
##
## custom ytry for unsupervised setting
##
##
####################################################################
set.unsupervised.ytry <- function(n, p, ytry = NULL) {
  if (is.null(ytry)) {
    ytry <- min(ceiling(sqrt(p)), p - 1)
  }
  ytry
}
####################################################################
##
## custom scale matrix: avoids NA's due to columns being singular
##
####################################################################
scaleM <- function(x, center = TRUE, scale = TRUE) {
  x <- data.matrix(x)
  d <- do.call(cbind, lapply(1:ncol(x), function(j) {
    sj <- sd(x[, j], na.rm = TRUE)
    if (scale) {
      if (sj < .Machine$double.eps) {
        sj <- 1
      }
      if (center) {
        (x[, j] - mean(x[, j], na.rm = TRUE)) / sj
      }
      else {
        x[, j] / sj
      }
    }
    else {
      if (center) {
        (x[, j] - mean(x[, j], na.rm = TRUE))
      }
      else {
        x[, j]
      }
    }
  }))
  colnames(d) <- colnames(x)
  data.matrix(d)
}
####################################################################
##
## graphical plot displaying s-dependency
##
## I: importance matrix (p x p or q x p) obtained from get.beta.entropy(o)
## threshold: minimum importance value to retain edges in the graph
## q.signal: quantile minimum threshold to retain signal designation 
## directed: directed graph?
## min.degree: minimum number of strong connections to consider a variable as signal
## plot: whether to plot the graph using igraph
##
####################################################################
sdependent <- function(I,
                       threshold = .25,
                       layout = "grid",
                       q.signal = .75,
                       directed = TRUE,
                       min.degree = NULL,
                       title = "s-Dependent Variable Detection",
                       plot = TRUE) {
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("Package 'igraph' is required but not installed.")
  }
  p <- nrow(I)
  q <- ncol(I)
  ## Pad rows with zero if needed
  if (q > p) {
    rownames(I) <- if (is.null(rownames(I))) paste0("x", 1:p) else rownames(I)
    extra.rows <- matrix(0, nrow = q - p, ncol = q)
    rownames(extra.rows) <- setdiff(colnames(I), rownames(I))
    I <- rbind(I, extra.rows)
    p <- nrow(I)  
  }
  ## Ensure square and clean diagonal
  diag(I) <- 0
  colnames(I) <- rownames(I) <- colnames(I)
  ## Compute column sums as global importance scores
  imp.score <- colSums(I, na.rm=TRUE)
  ## Minimum degree
  if (is.null(min.degree)) min.degree <- if (directed) 1 else 2
  ## Thresholding the matrix to construct the adjacency matrix
  A <- (I >= threshold) * 1
  if (directed) {
    g <- igraph::graph_from_adjacency_matrix(A, mode = "directed", diag = FALSE)
  }
  else {
    g <- igraph::graph_from_adjacency_matrix(A, mode = "undirected", diag = FALSE)
  }
  ## Identify and remove isolated nodes (degree zero)
  isolated <- igraph::degree(g, mode = "all") == 0
  g <- igraph::delete_vertices(g, which(isolated))
  vertex.names <- igraph::V(g)$name
  ## update values due to removed nodes
  imp.score <- imp.score[vertex.names]
  I <- I[vertex.names, vertex.names, drop = FALSE]
  ## check that graph is not empty
  if (length(imp.score) == 0) {
    return("graph is null after removing isolated nodes (degree zero) - increase threshold")
  }
  ## Compute node degrees (number of strong influences)
  ##
  ## for directed graphs
  ## out degree = row = # variables selected when variable is released
  ## in degree = column = # number of times Xs is selected when others are released
  if (directed) {
    node.degrees <- igraph::degree(g, mode = "out")
  }
  ##
  ## for undirected graphs, in and out distinction is irrelevant
  ##
  else {
    node.degrees <- igraph::degree(g)
  }
  ## Identify signal variables based on degree and importance
  signal.vars <- names(which(node.degrees >= min.degree
          & imp.score >= quantile(imp.score, q.signal, na.rm=TRUE)))
  ## Optional plotting
  if (plot) {
    ## layout
    layout.matrix <- get.layout(g, layout)
    ## options
    edge <- igraph::as_edgelist(g, names = TRUE)
    edge.width <- mapply(function(i, j) I[i, j], edge[,1], edge[,2])
    #edge.width <- sqrt(edge.width)
    edge.width <- 1 + 3 * edge.width / max(edge.width)
    vertex.size <- 3 + log1p(imp.score)
    igraph::V(g)$degree <- node.degrees
    ## plot
    igraph::plot.igraph(
      g,
      layout = layout.matrix,
      edge.width = edge.width,
      edge.arrow.size = 0.5,
      edge.curved = 0.2,
      edge.color = ifelse(edge[,1] %in% signal.vars, "dodgerblue", "gray60"),
      vertex.size = 2  * vertex.size,
      vertex.label.cex = 0.8,
      vertex.label.color = "black",
      vertex.color = ifelse(igraph::V(g)$name %in% signal.vars, "dodgerblue", "gray80"),
      main = title
    )
  }
  ## Return key values as a list, invisibly
  invisible(list(
    signal.vars = signal.vars,
    imp.score = sort(imp.score),
    degree = node.degrees))
}
get.layout <- function(g, layout) {
  ## All available layout functions in igraph
  layout_map <- c(
    fr       = "layout_with_fr",
    dh       = "layout_with_dh",
    gem      = "layout_with_gem",
    kk       = "layout_with_kk",
    lgl      = "layout_with_lgl",
    mds      = "layout_with_mds",
    sugiyama = "layout_with_sugiyama",
    graphopt = "layout_with_graphopt",
    nicely   = "layout_nicely",
    random   = "layout_randomly",
    sphere   = "layout_on_sphere",
    grid     = "layout_on_grid",
    circle   = "layout_in_circle",
    star     = "layout_as_star",
    tree     = "layout_as_tree",
    bipartite= "layout_as_bipartite"
  )
  if (is.character(layout)) {
    layout <- tolower(layout)
    ## Match either abbreviation or full name
    matched <- grep(paste0("^", layout), names(layout_map), value = TRUE)
    if (length(matched) == 0) {
      stop("Unknown layout name: '", layout, "'.")
    } else if (length(matched) > 1) {
      stop("Ambiguous layout abbreviation: '", layout, "'. Matches: ", paste(matched, collapse = ", "))
    }
    layout_fun_name <- layout_map[matched]
    layout_fun <- get(layout_fun_name, envir = asNamespace("igraph"))
    ## sugiyama returns a list with $layout
    result <- layout_fun(g)
    if (matched == "sugiyama") result <- result$layout
    return(result)
  } else if (is.matrix(layout)) {
    return(layout)
  } else {
    stop("Invalid layout input: must be character or matrix.")
  }
}
