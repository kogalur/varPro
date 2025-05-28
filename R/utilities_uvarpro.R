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
####################################################################
##
##
## classification analysis for rule region versus complementary region
##
##
####################################################################
get.beta.entropy <- function(o,
                             papply=mclapply,
                             lasso=TRUE,
                             nfolds=10,
                             maxit=2500,
                             thresh=1e-3,
                             glm.thresh=10) {
  ## input value must be an unusupervised varpro object
  if (!inherits(o, "uvarpro")) {
    stop("this wrapper only applies to unsupervised varpro")
  }
  ## get topvars, filter x
  vmp <- get.vimp(o, pretty=FALSE)
  vmp <- vmp[vmp>0]
  if (length(vmp)==0) {
    return(NULL)
  }
  xvars <- names(vmp)
  x <- o$x[, xvars, drop=FALSE]
  ## parse the membership values to obtain beta for each variable
  beta <- lapply(xvars, function(releaseX) {
    if (sum(xvars != releaseX) > 0) {
      bO <- do.call(rbind, papply(o$entropy[[releaseX]], function(rule) {
        get.beta.workhorse(releaseX, rule, x,
          lasso=lasso, nfolds=nfolds, maxit=maxit, thresh=thresh, glm.thresh=glm.thresh)
      }))
      if (!is.null(bO)) {
        colMeans(bO, na.rm = TRUE)
      }
      else {
        bO
      }
    }
    else {
      NULL
    }
  })
  names(beta) <- xvars
  do.call(rbind, beta)
}      
get.beta.workhorse <- function(releaseX,
                               rule,
                               xorg,
                               parallel=FALSE,
                               lasso=TRUE,
                               nfolds=10,
                               maxit=2500,
                               thresh=1e-3,
                               glm.thresh=10) {
  ## build the x data
  xC <- xorg[rule[[1]],]
  xO <- xorg[rule[[2]],]
  x <- rbind(xC, xO)
  nC <- nrow(xC)
  nO <- nrow(xO)
  x <- x[, colnames(x)!=releaseX]
  x <- scale(x, center=FALSE)
  ## define the classifier outcome
  class <- factor(c(rep(0, nC), rep(1, nO)))
  ##
  xnms <- colnames(xorg)
  p <- length(xnms)
  ## failure returns NULL
  beta <- NULL
  ## glmnet - maybe slower but reliable
  if (lasso) {
    o.glmnet <- tryCatch(
                  {suppressWarnings(cv.glmnet(as.matrix(x), class, family="binomial",
                         nfolds=nfolds, parallel=parallel, maxit=maxit, thresh=thresh))},
                         error=function(ex){NULL})
    if (!is.null(o.glmnet)) {
      bhat <- abs(coef(o.glmnet)[-1,1])
      beta <- rep(0, p)
      names(beta) <- xnms
      beta[names(bhat)] <- bhat
    }
  }
  ## glm - could be faster but very unreliable
  else {
    o.glm <- tryCatch(
              {suppressWarnings(glm(class~., data.frame(class=class, x), family="binomial"))},
      error=function(ex){NULL})
    if (!is.null(o.glm)) {
      if (max(abs(summary(o.glm)$coef[-1,1]), na.rm=TRUE) < glm.thresh) {
        bhat <- abs(summary(o.glm)$coef[-1,3])
        beta <- rep(NA, p)
        names(beta) <- xnms
        beta[names(bhat)] <- bhat
      }
    }
  }
  beta
}
####################################################################
##
##
## residuals from generalized inverse regression
## used to obtain beta from partial coefficients
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
set.unsupervised.nodesize <- function(n, p, nodesize = NULL) {
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
