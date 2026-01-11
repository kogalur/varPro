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
## Extract absolute, non-zero coefficients from a glmnet coef() object
## (typically a 1-column dgCMatrix). Returns a named numeric vector.
.abs_nz_coef <- function(co, drop_names = "(Intercept)") {
  if (is.null(co)) return(NULL)
  rn <- rownames(co)
  if (is.null(rn)) rn <- as.character(seq_len(nrow(co)))
  if (inherits(co, "dgCMatrix")) {
    ## For a single-column dgCMatrix, non-zeros are in @x and rows in @i (0-based)
    idx <- co@i + 1L
    val <- co@x
    nm  <- rn[idx]
    keep <- is.finite(val) & (val != 0)
    if (!is.null(drop_names) && length(drop_names)) {
      keep <- keep & !(nm %in% drop_names)
    }
    if (!any(keep)) return(setNames(numeric(0), character(0)))
    v <- abs(val[keep])
    names(v) <- nm[keep]
    v
  } else {
    ## Fallback: dense
    m <- tryCatch(as.matrix(co), error = function(e) NULL)
    if (is.null(m) || nrow(m) == 0L) return(NULL)
    v <- as.numeric(m[, 1])
    names(v) <- rn
    if (!is.null(drop_names) && length(drop_names)) {
      v <- v[!(names(v) %in% drop_names)]
    }
    v <- v[is.finite(v) & v != 0]
    abs(v)
  }
}
fit.logistic.cv.beta <- function(X.cls, class,
                                 nfolds, parallel, maxit, thresh,
                                 use.cv = TRUE,
                                 lambda.sel = c("lambda.1se", "lambda.min"),
                                 nlambda = NULL,
                                 lambda.min.ratio = NULL) {
  lambda.sel <- match.arg(lambda.sel)
  ## !!!manual scaling!!!
  X.cls <- scale(X.cls, center = FALSE)
  if (isTRUE(use.cv)) {
    args <- list(
      x        = X.cls,
      y        = class,
      family   = "binomial",
      nfolds   = nfolds,
      parallel = parallel,
      maxit    = maxit,
      thresh   = thresh
    )
    if (!is.null(nlambda))          args$nlambda <- nlambda
    if (!is.null(lambda.min.ratio)) args$lambda.min.ratio <- lambda.min.ratio
    o.cv <- tryCatch(
      suppressWarnings(do.call(glmnet::cv.glmnet, args)),
      error = function(e) NULL
    )
    if (is.null(o.cv)) return(NULL)
    co <- tryCatch(stats::coef(o.cv, s = lambda.sel),
                   error = function(e) NULL)
  } else {
    ## No-CV option (faster): fit once and take the last lambda on the path
    args <- list(
      x      = X.cls,
      y      = class,
      family = "binomial",
      maxit  = maxit,
      thresh = thresh
    )
    if (!is.null(nlambda))          args$nlambda <- nlambda
    if (!is.null(lambda.min.ratio)) args$lambda.min.ratio <- lambda.min.ratio
    fit <- tryCatch(
      suppressWarnings(do.call(glmnet::glmnet, args)),
      error = function(e) NULL
    )
    if (is.null(fit)) return(NULL)
    co <- last.lambda.coef(fit)
  }
  ## Pull only non-zero coefficients (drop intercept)
  bhat <- .abs_nz_coef(co, drop_names = "(Intercept)")
  if (is.null(bhat) || !length(bhat)) return(NULL)
  bhat
}
last.lambda.coef <- function(fit) {
  lam.last <- utils::tail(fit$lambda, 1L)
  tryCatch(stats::coef(fit, s = lam.last),
           error = function(e) NULL)
}
fit.linear.lasso.at.last <- function(X, Y,
                                     add.zero = FALSE,
                                     zero.name = "ZeroColumn",
                                     nlambda = NULL,
                                     lambda.min.ratio = NULL,
                                     maxit = 2500,
                                     thresh = 1e-3) {
  ## !!!!manual scaling!!!!
  X <- scale(X, center = FALSE)
  if (add.zero) {
    X <- cbind(rep(0, nrow(X)), X)
    colnames(X)[1] <- zero.name
  }
  args <- list(
    x     = X,
    y     = Y,
    alpha = 1,
    maxit = maxit,
    thresh = thresh
  )
  if (!is.null(nlambda))          args$nlambda <- nlambda
  if (!is.null(lambda.min.ratio)) args$lambda.min.ratio <- lambda.min.ratio
  fit <- tryCatch(
    suppressWarnings(do.call(glmnet::glmnet, args)),
    error = function(e) NULL
  )
  if (is.null(fit)) return(NULL)
  co <- last.lambda.coef(fit)
  if (is.null(co)) return(NULL)
  drop <- "(Intercept)"
  if (isTRUE(add.zero)) drop <- c(drop, zero.name)
  v <- .abs_nz_coef(co, drop_names = drop)
  if (is.null(v) || !length(v)) return(NULL)
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
                           ret.data = FALSE,
                           sparse = FALSE,
                           use.cv = TRUE,
                           lambda.sel = c("lambda.1se", "lambda.min"),
                           nlambda = NULL,
                           lambda.min.ratio = NULL) {
  if (length(predictor.universe) == 0L) return(NULL)
  lambda.sel <- match.arg(lambda.sel)
  rel.col.fit <- match(releaseX, predictor.universe)
  if (is.na(rel.col.fit)) return(NULL)  # release not in this universe
  idx.C <- rule[[1]]
  idx.O <- rule[[2]]
  if (length(idx.C) == 0L || length(idx.O) == 0L) return(NULL)
  idx <- c(idx.C, idx.O)
  nC  <- length(idx.C); nO <- length(idx.O)
  ## Build the design once; avoid repeated char-based column matching when possible
  if (isTRUE(ret.data)) {
    if (!is.null(colnames(xorg)) && identical(predictor.universe, colnames(xorg))) {
      dt <- xorg[idx, , drop = FALSE]
    } else {
      dt <- xorg[idx, predictor.universe, drop = FALSE]
    }
    X.cls <- dt[, -rel.col.fit, drop = FALSE]
  } else {
    dt <- NULL
    if (!is.null(colnames(xorg)) && identical(predictor.universe, colnames(xorg))) {
      X.cls <- xorg[idx, -rel.col.fit, drop = FALSE]
    } else {
      X.cls <- xorg[idx, predictor.universe[-rel.col.fit], drop = FALSE]
    }
  }
  ## logistic step
  class <- factor(c(rep(0, nC), rep(1, nO)))
  bhat  <- fit.logistic.cv.beta(X.cls, class,
                                nfolds = nfolds,
                                parallel = parallel,
                                maxit = maxit,
                                thresh = thresh,
                                use.cv = use.cv,
                                lambda.sel = lambda.sel,
                                nlambda = nlambda,
                                lambda.min.ratio = lambda.min.ratio)
  if (is.null(bhat)) return(NULL)
  ## sparse mode: return only selected predictors (non-zero coef magnitudes)
  if (isTRUE(sparse)) {
    if (isTRUE(ret.data)) {
      return(list(beta = bhat, dt = dt, rel.col.fit = rel.col.fit))
    } else {
      return(bhat)
    }
  }
  ## dense mode: embed into a full vector over the predictor universe
  beta <- setNames(numeric(length(predictor.universe)), predictor.universe)
  pos  <- match(names(bhat), predictor.universe)
  ok   <- is.finite(pos)
  if (any(ok)) beta[pos[ok]] <- bhat[ok]
  if (isTRUE(ret.data)) {
    list(beta = beta, dt = dt, rel.col.fit = rel.col.fit)
  } else {
    beta
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
                           parallel = FALSE,
                           ## (2) speed: don't keep per-rule matrices unless you need them
                           store.rules = TRUE,
                           ## (6) speed knobs for the logistic stage
                           use.cv = TRUE,
                           lambda.sel = c("lambda.1se", "lambda.min"),
                           nlambda = NULL,
                           lambda.min.ratio = NULL,
                           ## (6) knobs for the stage-2 lasso (only used when second.stage=TRUE)
                           nlambda.lasso = nlambda,
                           lambda.min.ratio.lasso = lambda.min.ratio) {
  if (is.null(o$x) || is.null(o$entropy)) {
    stop("Object 'o' must contain x and entropy.")
  }
  lambda.sel <- match.arg(lambda.sel)
  ## resolve papply
  papply <- .resolve_papply(
    papply,
    cores = getOption("mc.cores", 1L),
    envir = environment()
  )
  x.nms.all     <- colnames(o$x)
  rel.candidates <- names(o$entropy)
  ## optional VIMP pre-filter (shrinks rows and cols)
  if (isTRUE(pre.filter)) {
    vmp <- tryCatch(get.vimp(o, pretty = FALSE), error = function(e) NULL)
    if (is.null(vmp)) {
      x.nms.fit <- x.nms.all
      rel.vars  <- rel.candidates
    } else {
      xvars    <- names(vmp[vmp > vimp.min])
      xvars    <- intersect(xvars, x.nms.all)
      rel.vars <- intersect(xvars, rel.candidates)
      if (length(xvars) == 0L || length(rel.vars) == 0L) {
        return(list(info.rule = list(),
                    beta.mean.mat = NULL,
                    lasso.mean.mat = NULL,
                    lasso.mean.std.mat = NULL))
      }
      x.nms.fit <- xvars
    }
  } else {
    x.nms.fit <- x.nms.all
    rel.vars  <- rel.candidates
  }
  if (!length(x.nms.fit) || !length(rel.vars)) {
    return(list(info.rule = list(),
                beta.mean.mat = NULL,
                lasso.mean.mat = NULL,
                lasso.mean.std.mat = NULL))
  }
  ## (5c) Convert once to a numeric matrix (faster slicing than data.frame)
  x_fit <- data.matrix(o$x[, x.nms.fit, drop = FALSE])
  colnames(x_fit) <- x.nms.fit
  ## Don't ship the full 'o' through the cluster closure if we can avoid it
  entropy_list <- o$entropy
  p_fit <- length(x.nms.fit)
  ## ------------- outer loop over release variables -------------
  mat.list <- papply(rel.vars, function(x.release) {
    ## Safety / edge cases
    if (length(setdiff(x.nms.fit, x.release)) == 0L) return(NULL)
    oo <- entropy_list[[x.release]]
    if (length(oo) == 0L) return(NULL)
    nr <- length(oo)
    ## storage
    if (isTRUE(store.rules)) {
      beta.mat <- matrix(0, nrow = nr, ncol = p_fit,
                         dimnames = list(NULL, x.nms.fit))
      ## (1) don't build per-rule all-zero lasso objects when second.stage=FALSE
      lasso.mat <- if (isTRUE(second.stage)) {
        matrix(0, nrow = nr, ncol = p_fit,
               dimnames = list(NULL, x.nms.fit))
      } else {
        NULL
      }
      keep <- rep(FALSE, nr)
    } else {
      beta.sum <- numeric(p_fit)
      lasso.sum <- if (isTRUE(second.stage)) numeric(p_fit) else NULL
      n.ok <- 0L
    }
    ## --------- inner loop over rules ---------
    for (rr in seq_len(nr)) {
      rule <- oo[[rr]]
      ## logistic stage (sparse, fast path)
      res <- beta.workhorse(
        releaseX = x.release,
        rule     = rule,
        xorg     = x_fit,
        predictor.universe = x.nms.fit,
        nfolds   = nfolds,
        parallel = parallel,
        maxit    = maxit,
        thresh   = thresh,
        ret.data = second.stage,
        sparse   = TRUE,
        use.cv   = use.cv,
        lambda.sel = lambda.sel,
        nlambda  = nlambda,
        lambda.min.ratio = lambda.min.ratio
      )
      if (is.null(res)) next
      if (isTRUE(second.stage)) {
        bhat <- res$beta       # sparse named vector
        dt   <- res$dt         # matrix with all columns in x.nms.fit
        rel.col.fit <- res$rel.col.fit
      } else {
        bhat <- res            # sparse named vector
      }
      if (isTRUE(store.rules)) {
        keep[rr] <- TRUE
        pos <- match(names(bhat), x.nms.fit)
        ok  <- is.finite(pos)
        if (any(ok)) beta.mat[rr, pos[ok]] <- bhat[ok]
      } else {
        n.ok <- n.ok + 1L
        pos <- match(names(bhat), x.nms.fit)
        ok  <- is.finite(pos)
        if (any(ok)) beta.sum[pos[ok]] <- beta.sum[pos[ok]] + bhat[ok]
      }
      ## optional second stage lasso: regress released var on selected predictors
      if (isTRUE(second.stage)) {
        nz.names <- names(bhat)  # already non-zero only
        if (length(nz.names)) {
          Y <- dt[, rel.col.fit]
          if (length(nz.names) > 1L) {
            v <- fit.linear.lasso.at.last(
              dt[, nz.names, drop = FALSE], Y,
              add.zero = FALSE,
              nlambda = nlambda.lasso,
              lambda.min.ratio = lambda.min.ratio.lasso,
              maxit = maxit,
              thresh = thresh
            )
          } else {
            v <- fit.linear.lasso.at.last(
              dt[, nz.names, drop = FALSE], Y,
              add.zero = TRUE,
              nlambda = nlambda.lasso,
              lambda.min.ratio = lambda.min.ratio.lasso,
              maxit = maxit,
              thresh = thresh
            )
          }
          if (!is.null(v) && length(v)) {
            pos2 <- match(names(v), x.nms.fit)
            ok2  <- is.finite(pos2)
            if (any(ok2)) {
              if (isTRUE(store.rules)) {
                lasso.mat[rr, pos2[ok2]] <- v[ok2]
              } else {
                lasso.sum[pos2[ok2]] <- lasso.sum[pos2[ok2]] + v[ok2]
              }
            }
          }
        }
      }
    }
    ## prune failures and return
    if (isTRUE(store.rules)) {
      if (!any(keep)) return(NULL)
      beta.mat <- beta.mat[keep, , drop = FALSE]
      if (!isTRUE(second.stage)) {
        ## keep output contract: lasso matrix exists but is all zeros
        lasso.mat <- matrix(0, nrow = nrow(beta.mat), ncol = p_fit,
                            dimnames = list(NULL, x.nms.fit))
      } else {
        lasso.mat <- lasso.mat[keep, , drop = FALSE]
      }
      list(beta = beta.mat, lasso = lasso.mat)
    } else {
      if (n.ok < 1L) return(NULL)
      beta.mean <- beta.sum / n.ok
      names(beta.mean) <- x.nms.fit
      if (isTRUE(second.stage)) {
        lasso.mean <- lasso.sum / n.ok
        names(lasso.mean) <- x.nms.fit
      } else {
        lasso.mean <- NULL
      }
      list(beta.mean = beta.mean,
           lasso.mean = lasso.mean,
           n.ok = n.ok)
    }
  })
  names(mat.list) <- rel.vars
  mat.list <- mat.list[!vapply(mat.list, is.null, logical(1))]
  if (!length(mat.list)) {
    return(list(info.rule = list(),
                beta.mean.mat = NULL,
                lasso.mean.mat = NULL,
                lasso.mean.std.mat = NULL))
  }
  ## -------- assemble outputs --------
  if (isTRUE(store.rules)) {
    beta.list  <- lapply(mat.list, `[[`, "beta")
    lasso.list <- lapply(mat.list, `[[`, "lasso")
    beta.mean.mat  <- do.call(rbind, lapply(beta.list,  function(x) colMeans(x,  na.rm = TRUE)))
    lasso.mean.mat <- do.call(rbind, lapply(lasso.list, function(x) colMeans(x, na.rm = TRUE)))
    info.rule <- mat.list
  } else {
    beta.mean.mat <- do.call(rbind, lapply(mat.list, `[[`, "beta.mean"))
    if (isTRUE(second.stage)) {
      lasso.mean.mat <- do.call(rbind, lapply(mat.list, `[[`, "lasso.mean"))
    } else {
      lasso.mean.mat <- matrix(0, nrow = nrow(beta.mean.mat), ncol = ncol(beta.mean.mat),
                               dimnames = dimnames(beta.mean.mat))
    }
    info.rule <- list()  ## intentionally empty to reduce object size
  }
  ## standardized lasso: divide each row by sd(Y) so entries are (SDs of Y) per 1 SD of X
  if (isTRUE(second.stage) && !is.null(lasso.mean.mat) && nrow(lasso.mean.mat) > 0L) {
    rel.rows <- rownames(lasso.mean.mat)
    sd.y <- vapply(rel.rows,
                   function(v) stats::sd(x_fit[, v], na.rm = TRUE),
                   FUN.VALUE = numeric(1))
    sd.y[!is.finite(sd.y) | sd.y == 0] <- NA_real_
    lasso.mean.std.mat <- sweep(lasso.mean.mat, 1, sd.y, "/")
  } else {
    ## (when second.stage=FALSE, lasso.mean.mat is already all zeros)
    lasso.mean.std.mat <- lasso.mean.mat
  }
  list(
    info.rule          = info.rule,
    beta.mean.mat      = beta.mean.mat,
    lasso.mean.mat     = lasso.mean.mat,
    lasso.mean.std.mat = lasso.mean.std.mat
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
                             parallel = FALSE,
                             ## (6) speed knobs
                             use.cv = TRUE,
                             lambda.sel = c("lambda.1se", "lambda.min"),
                             nlambda = NULL,
                             lambda.min.ratio = NULL,
                             nlambda.lasso = nlambda,
                             lambda.min.ratio.lasso = lambda.min.ratio) {
  out <- custom.entropy(
    o,
    papply = papply,
    pre.filter = pre.filter,
    second.stage = second.stage,
    vimp.min = vimp.min,
    nfolds = nfolds,
    maxit = maxit,
    thresh = thresh,
    parallel = parallel,
    ## (2) We only return means here, so don't store rule-level matrices.
    store.rules = FALSE,
    ## (6)
    use.cv = use.cv,
    lambda.sel = lambda.sel,
    nlambda = nlambda,
    lambda.min.ratio = lambda.min.ratio,
    nlambda.lasso = nlambda.lasso,
    lambda.min.ratio.lasso = lambda.min.ratio.lasso
  )
  if (isTRUE(second.stage)) {
    out$lasso.mean.std.mat
  } else {
    out$beta.mean.mat
  }
}
####################################################################
##
##
## custom nodesize for unsupervised setting
## typically much larger than varpro
##
##
####################################################################
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
