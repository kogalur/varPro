partialpro <- function(object,
                       xvar.names,
                       nvar,
                       target,
                       learner,
                       newdata,
                       method = c("unsupv", "rnd", "auto"),
                       verbose = FALSE,
                       vt.filter = c("isopro", "outpro", "none"),
                       ...)
{
  ## ------------------------------------------------------------------------
  ##
  ## incoming object must be a varpro object: extract relevant parameters
  ##
  ## ------------------------------------------------------------------------
  if (!inherits(object, "varpro")) {
    stop("object must be a varpro object")
  }
  ## validate additional arguments before computing profiles
  dots <- list(...)
  if (length(dots) > 0L &&
      (is.null(names(dots)) || anyNA(names(dots)) || any(!nzchar(names(dots))))) {
    stop("partialpro(): arguments in ... must be named", call. = FALSE)
  }
  duplicates <- unique(names(dots)[duplicated(names(dots))])
  if (length(duplicates) > 0L) {
    stop("partialpro(): duplicate argument(s): ",
         paste(duplicates, collapse = ", "), call. = FALSE)
  }
  hidden <- get.partialpro.hidden(dots)
  extra <- setdiff(names(dots), names(hidden))
  if (length(extra) > 0L) {
    stop("partialpro(): unrecognized argument(s): ",
         paste(extra, collapse = ", "), call. = FALSE)
  }
  ## set xvar.names here
  topvars <- get.topvars(object)
  if (missing(xvar.names)) {
    xvar.names <- topvars
  }
  ## Limit the requested variables without introducing an index for an empty set.
  if (!missing(nvar)) {
    if (!is.numeric(nvar) || length(nvar) != 1L || is.na(nvar) ||
        nvar < 1 || (is.finite(nvar) && nvar != floor(nvar))) {
      stop("nvar must be a positive integer or Inf", call. = FALSE)
    }
    xvar.names <- xvar.names[seq_len(min(length(xvar.names), nvar))]
  }
  ## extract x and set the dimension
  xvar <- object$x
  n <- nrow(xvar)
  ## pull the family
  family <- object$family
  ## set UVT method and filter
  method <- match.arg(method, c("unsupv", "rnd", "auto"))
  vt.filter <- match.arg(vt.filter, c("isopro", "outpro", "none"))
  ## the default learner used for prediction is the varpro random forest object
  if (missing(learner)) {
    learner <- function(newx) {
      if (missing(newx)) {
        predict.rfsrc(object$rf, perf.type = "none")$predicted.oob
      }
      else {
        predict.rfsrc(object$rf, newx, perf.type = "none")$predicted
      }
    }
  }
  if (!is.function(learner)) {
    stop("learner must be a prediction function", call. = FALSE)
  }
  ## check to see if new data is available
  predict.flag <- !missing(newdata)
  ## ------------------------------------------------------------------------
  ##
  ## family specific details
  ##
  ## ------------------------------------------------------------------------
  ## define yvar with special treatment for factors (check directly using y original)
  if (is.factor(object$y.org)) {
    yvar <- object$y.org
    family <- "class"
  }
  else {
    yvar <- object$y
  }
  ## -------------------
  ## process yvar
  ## -------------------
  ## A matrix response denotes a multivariate analysis, not a numeric vector.
  target.label <- NULL
  if (is.numeric(yvar) && is.null(dim(yvar))) {
    target <- 1L
  }
  ## classification
  else if (is.factor(yvar)) {
    ## set the target value
    yvar.levels <- levels(yvar)
    if (missing(target)) {
      target <- yvar.levels[length(yvar.levels)]
    }
    if (is.character(target)) {
      target.label <- match.arg(target, yvar.levels)
      target <- match(target.label, yvar.levels)
    }
    else {
      if (!is.numeric(target) || length(target) != 1L ||
          !is.finite(target) || target != floor(target) ||
          target < 1L || target > length(yvar.levels)) {
        stop("target must be a class label or a valid integer column index",
             call. = FALSE)
      }
    }
  }
  ## not handled (yet)
  else {
    stop("multivariate regression families not currently supported")
  }
  ## ------------------------------------------------------------------------
  ##
  ## hidden options
  ##
  ## ------------------------------------------------------------------------
  ## unpack hidden options
  cut <- hidden$cut
  nsmp <- hidden$nsmp
  nvirtual0 <- hidden$nvirtual
  nmin <- hidden$nmin
  alpha <- hidden$alpha
  df <- round(max(1, hidden$df))
  sampsize <- hidden$sampsize
  ntree <- hidden$ntree
  nodesize <- hidden$nodesize
  mse.tolerance <- hidden$mse.tolerance
  out.distancef <- hidden$out.distancef
  out.neighbor <- hidden$out.neighbor
  out.reduce <- hidden$out.reduce
  out.cutoff <- hidden$out.cutoff
  out.max.rules.tree <- hidden$out.max.rules.tree
  out.max.tree <- hidden$out.max.tree
  out.knn.chunk.size <- hidden$out.knn.chunk.size
  out.null <- hidden$out.null
  ## is UVT at play?
  cut.flag <- (cut != 0) && !identical(vt.filter, "none")
  ## ------------------------------------------------------------------------
  ##
  ## process the requested variables
  ##
  ## ------------------------------------------------------------------------
  unavailable <- setdiff(xvar.names, object$xvar.names)
  if (length(unavailable) > 0L) {
    warning("partialpro(): skipping xvar.names not found in object$xvar.names: ",
            paste(unavailable, collapse = ", "), call. = FALSE)
  }
  variables <- object$xvar.names[as.numeric(na.omit(match(xvar.names, object$xvar.names)))]
  if (length(variables) == 0) {
    return(NULL)
  }
  ## ------------------------------------------------------------------------
  ##
  ## validate and align newdata once (if supplied)
  ##
  ## ------------------------------------------------------------------------
  if (predict.flag) {
    if (sum(!(colnames(xvar) %in% colnames(newdata))) > 0) {
      stop("x-variables in newdata does not match original data")
    }
    newdata <- newdata[, colnames(xvar), drop = FALSE]
  }
  ## ------------------------------------------------------------------------
  ##
  ## UVT filter setup
  ##
  ## ------------------------------------------------------------------------
  if (cut.flag && identical(vt.filter, "isopro")) {
    ## unsupervised method cannot be used if only one variable is present
    if (length(topvars) == 1 && method == "unsupv") {
      method <- "rnd"
    }
    ## isopro call
    o.iso <- isopro(data = xvar[, topvars, drop = FALSE], method = method,
                    sampsize = sampsize, ntree = ntree, nodesize = nodesize)
  }
  if (cut.flag && identical(vt.filter, "outpro")) {
    if (!exists("outpro", mode = "function")) {
      stop("vt.filter = 'outpro' requires the outpro function")
    }
    if (!exists("outpro.null", mode = "function")) {
      stop("vt.filter = 'outpro' requires the outpro.null function")
    }
  }
  ## cache outpro null calibrations by selected subspace and options
  out.null.cache <- new.env(parent = emptyenv())
  .out_reduce_for <- function(xnm) {
    if (is.null(out.reduce)) {
      ## Default: evaluate support in the top VarPro subspace and
      ## always include the feature being profiled.
      reduce <- unique(c(topvars, xnm))
    }
    else if (is.character(out.reduce)) {
      reduce <- unique(c(out.reduce, xnm))
    }
    else if (is.numeric(out.reduce) && !is.null(names(out.reduce))) {
      reduce <- out.reduce
      if (!(xnm %in% names(reduce))) {
        reduce <- c(reduce, stats::setNames(1, xnm))
      }
    }
    else {
      ## TRUE, FALSE, and other outpro-native values are passed through.
      reduce <- out.reduce
    }
    reduce
  }
  .out_reduce_key <- function(reduce) {
    if (is.null(reduce)) {
      return("NULL")
    }
    if (is.logical(reduce)) {
      return(paste0("logical:", paste(as.character(reduce), collapse = ",")))
    }
    if (is.numeric(reduce) && !is.null(names(reduce))) {
      return(paste0("weighted:",
                    paste(paste(names(reduce), signif(reduce, 14), sep = "="),
                          collapse = ",")))
    }
    paste0("vars:", paste(as.character(reduce), collapse = ","))
  }
  .out_get_null <- function(xnm, reduce) {
    ## User-supplied calibration: either a single outpro.null object
    ## or a named list of such objects, keyed by variable name.
    if (!is.null(out.null)) {
      out.null.user <- out.null
      if (is.list(out.null) && !is.null(out.null[[xnm]]) &&
          is.list(out.null[[xnm]]) && !is.null(out.null[[xnm]]$distance)) {
        out.null.user <- out.null[[xnm]]
      }
      if (is.null(out.null.user$distance)) {
        stop("out.null must be an outpro.null object or a named list of outpro.null objects")
      }
      if (is.null(out.null.user$cdf)) {
        out.null.user$cdf <- ecdf(out.null.user$distance)
      }
      return(out.null.user)
    }
    key <- paste(out.distancef,
                 if (is.null(out.neighbor)) "NULL" else out.neighbor,
                 if (is.null(out.cutoff)) "NULL" else out.cutoff,
                 out.max.rules.tree,
                 out.max.tree,
                 out.knn.chunk.size,
                 .out_reduce_key(reduce),
                 sep = "\r")
    if (exists(key, envir = out.null.cache, inherits = FALSE)) {
      return(get(key, envir = out.null.cache, inherits = FALSE))
    }
    null.obj <- outpro.null(object,
                            neighbor = out.neighbor,
                            distancef = out.distancef,
                            reduce = reduce,
                            cutoff = out.cutoff,
                            max.rules.tree = out.max.rules.tree,
                            max.tree = out.max.tree,
                            knn.chunk.size = out.knn.chunk.size)
    assign(key, null.obj, envir = out.null.cache)
    null.obj
  }
  .out_vt_score <- function(xfake, xnm) {
    reduce <- .out_reduce_for(xnm)
    null.obj <- .out_get_null(xnm, reduce)
    op <- outpro(object,
                 newdata = xfake,
                 neighbor = out.neighbor,
                 distancef = out.distancef,
                 reduce = reduce,
                 cutoff = out.cutoff,
                 max.rules.tree = out.max.rules.tree,
                 max.tree = out.max.tree,
                 knn.chunk.size = out.knn.chunk.size,
                 newdata.xscale = TRUE)
    ## outpro is an outlyingness distance. Convert it to a support
    ## score so the existing partialpro convention is preserved:
    ## larger scores are more acceptable, and cut = 0 disables UVT.
    1 - null.obj$cdf(op$distance)
  }
  ## ------------------------------------------------------------------------
  ##
  ## helpers (internal)
  ##
  ## ------------------------------------------------------------------------
  ## robust/fast polynomial fit using precomputed design matrices
  .safe_lm_fit <- function(X, y) {
    ## lm.fit does not tolerate NA/NaN/Inf
    ok <- is.finite(y) & (rowSums(is.finite(X)) == ncol(X))
    if (!any(ok)) {
      return(NULL)
    }
    X <- X[ok, , drop = FALSE]
    y <- y[ok]
    fit <- tryCatch(stats::lm.fit(x = X, y = y), error = function(e) NULL)
    ## An unidentified polynomial must not become a partial or flat curve.
    if (is.null(fit) || fit$rank < ncol(X) ||
        any(!is.finite(fit$coefficients))) {
      return(NULL)
    }
    fit
  }
  .safe_pred <- function(fit, Xnew) {
    if (is.null(fit)) {
      return(rep(NA_real_, nrow(Xnew)))
    }
    pred <- drop(Xnew %*% fit$coefficients)
    pred[!is.finite(pred)] <- NA_real_
    pred
  }
  ## ------------------------------------------------------------------------
  ##
  ## loop over requested variables obtaining partial plots
  ##
  ## ------------------------------------------------------------------------
  rO <- lapply(variables, function(xnm) {
    ## verbose output
    if (verbose) {
      cat("fitting variable", xnm, "\n")
    }
    ## create desired x-feature sequence of virtual values
    xorg <- xvar[, xnm]
    nxorg <- length(unique(xorg))
    binary.variable <- nxorg == 2
    xvirtual <- myunique(xorg, nvirtual0, alpha)
    nvirtual <- length(xvirtual)
    ## --------------------------------------------------------
    ## make fake partial data (vectorized; avoids per-case rbind)
    ## --------------------------------------------------------
    if (!predict.flag) {
      smp <- sample.int(n, size = min(n, nsmp), replace = FALSE)
      baseX <- xvar[smp, , drop = FALSE]
      case_ids <- smp
    } else {
      baseX <- newdata
      case_ids <- seq_len(nrow(baseX))
    }
    ncase <- nrow(baseX)
    if (ncase == 0L || nvirtual == 0L) {
      return(NULL)
    }
    ## replicate cases in blocks (case1 repeated nvirtual times, etc)
    idx_rep <- rep(seq_len(ncase), each = nvirtual)
    xfake <- baseX[idx_rep, , drop = FALSE]
    xfake[[xnm]] <- rep(xvirtual, times = ncase)
    ## training split per case (stored as ncase x nvirtual matrix)
    train_mat <- matrix(0L, nrow = ncase, ncol = nvirtual)
    for (ii in seq_len(ncase)) {
      train_mat[ii, ] <- mytrainsample(nvirtual)
    }
    train_mat <- (train_mat == 1L)
    ## unlimited virtual twins step: identify admissible virtual twins
    goodvt_mat <- matrix(TRUE, nrow = ncase, ncol = nvirtual)
    if (cut.flag) {
      vt.score <- if (identical(vt.filter, "isopro")) {
        tryCatch({
          ## prefer the columns used to train the isolation forest
          predict.isopro(o.iso, xfake[, topvars, drop = FALSE])
        }, error = function(e) {
          predict.isopro(o.iso, xfake)
        })
      } else if (identical(vt.filter, "outpro")) {
        .out_vt_score(xfake, xnm)
      } else {
        rep(1, nrow(xfake))
      }
      if (!is.numeric(vt.score) || length(vt.score) != nrow(xfake)) {
        stop("virtual-twin filtering must return one numeric score per row",
             call. = FALSE)
      }
      goodvt <- is.finite(vt.score) & (vt.score >= cut)
      if (sum(goodvt) == 0) {
        return(NULL)
      }
      goodvt_mat <- matrix(goodvt, nrow = ncase, ncol = nvirtual, byrow = TRUE)
    }
    ## obtain predicted value for fake partial data
    ## (IMPORTANT: pass only feature columns to learner; case/train/goodvt are internal)
    pred <- learner(xfake)
    if (is.data.frame(pred)) {
      if (any(!vapply(pred, is.numeric, logical(1)))) {
        stop("learner predictions must be numeric", call. = FALSE)
      }
      pred <- as.matrix(pred)
    } else if (is.numeric(pred) && length(dim(pred)) <= 1L) {
      ## Treat a one-dimensional array as a single prediction column,
      ## just like an ordinary numeric vector. Preserve matrix columns.
      pred <- matrix(pred, ncol = 1L)
    }
    if (!is.matrix(pred) || !is.numeric(pred) ||
        nrow(pred) != nrow(xfake) || ncol(pred) < 1L) {
      stop("learner must return numeric predictions with one row per input row",
           " (expected ", nrow(xfake), " rows; class: ",
           paste(class(pred), collapse = "/"), "; dimensions: ",
           if (is.null(dim(pred))) "none" else paste(dim(pred), collapse = " x "),
           "; length: ", length(pred), ")", call. = FALSE)
    }
    target.column <- target
    if (family == "class") {
      if (ncol(pred) != length(yvar.levels)) {
        stop("classification learners must return one probability column per class",
             call. = FALSE)
      }
      ## Names resolve class labels; a numeric target still selects by position.
      if (!is.null(target.label) && !is.null(colnames(pred))) {
        if (anyNA(colnames(pred)) || anyDuplicated(colnames(pred)) ||
            !(target.label %in% colnames(pred))) {
          stop("learner probability columns do not identify the requested class",
               call. = FALSE)
        }
        target.column <- match(target.label, colnames(pred))
      }
    }
    yhat <- as.numeric(pred[, target.column])
    if (family == "class") {
      if (any(!is.na(yhat) & (!is.finite(yhat) | yhat < 0 | yhat > 1))) {
        stop("learner class probabilities must be between zero and one",
             call. = FALSE)
      }
      yhat <- mylogodds(yhat)
    }
    ## reshape predictions into case-by-virtual matrix
    yhat_mat <- matrix(yhat, nrow = ncase, ncol = nvirtual, byrow = TRUE)
    ## --------------------------------------------------------------------------
    ##
    ## loop over cases: local polynomial fit (fast path via lm.fit)
    ##
    ## --------------------------------------------------------------------------
    ## preallocate outputs
    keep_case <- logical(ncase)
    goodvt_out <- matrix(NA_real_, nrow = ncase, ncol = nvirtual)
    yhat_nonpar_out <- matrix(NA_real_, nrow = ncase, ncol = nvirtual)
    yhat_causal_out <- matrix(NA_real_, nrow = ncase, ncol = nvirtual)
    bhat_out <- matrix(NA_real_, nrow = ncase, ncol = df + 1)
    ## design matrix for polynomial regression: [1, x, x^2, ... x^df]
    ## only needed for continuous variables
    Xfull <- NULL
    if (!binary.variable) {
      Xfull <- outer(xvirtual, 0:df, `^`)
    }
    ## threshold for sufficient good twins
    min_good <- min(nmin, nxorg / 2)
    for (ii in seq_len(ncase)) {
      goodvt <- goodvt_mat[ii, ]
      train <- train_mat[ii, ]
      if (sum(goodvt) >= min_good || binary.variable) {
        keep_case[ii] <- TRUE
        ## store goodvt as 1/NA (same convention as original)
        goodvt_out[ii, ] <- ifelse(goodvt, 1, NA_real_)
        ## y predictions for this case across virtual values
        yalli <- yhat_mat[ii, ]
        ## container
        yhat.nonpar <- rep(NA_real_, nvirtual)
        bhat <- rep(NA_real_, df + 1)
        ## ------------------------------------------------------------
        ## continuous variable fit
        ## ------------------------------------------------------------
        if (!binary.variable) {
          fit_sel <- NULL
          ## out-of-sample comparison of cut vs nocut
          if (cut.flag && sum(train & goodvt) > (nmin / 2)) {
            fit_cut <- .safe_lm_fit(Xfull[train & goodvt, , drop = FALSE], yalli[train & goodvt])
            fit_nocut <- .safe_lm_fit(Xfull[train, , drop = FALSE], yalli[train])
            if (!is.null(fit_cut) && !is.null(fit_nocut)) {
              ## predictions on held-out virtual values
              ytest <- yalli[!train]
              ytest.cut <- .safe_pred(fit_cut, Xfull[!train, , drop = FALSE])
              ytest.nocut <- .safe_pred(fit_nocut, Xfull[!train, , drop = FALSE])
              ## Use the unrestricted fit only when both errors are defined.
              err.cut <- mymse(ytest, ytest.cut)
              err.nocut <- mymse(ytest, ytest.nocut)
              if (is.finite(err.cut) && is.finite(err.nocut) &&
                  err.nocut < (err.cut - mse.tolerance)) {
                fit_sel <- .safe_lm_fit(Xfull, yalli)
              }
            }
          }
          ## Use all supported values when no valid comparison favors no cut.
          if (is.null(fit_sel)) {
            fit_sel <- .safe_lm_fit(Xfull[goodvt, , drop = FALSE], yalli[goodvt])
          }
          if (!is.null(fit_sel)) {
            bhat <- fit_sel$coefficients
            yhat.nonpar <- .safe_pred(fit_sel, Xfull)
            ## center by intercept (matches original)
            yhat.nonpar <- yhat.nonpar - bhat[1]
          }
        }
        ## ------------------------------------------------------------
        ## binary variable fit
        ## ------------------------------------------------------------
        else {
          ## both virtual twins must be available since extrapolation not possible
          ## if one is missing, set entire case to NA
          if (nvirtual >= 2L && any(goodvt)) {
            x_chr <- as.character(xvirtual)
            xi_chr <- x_chr[goodvt]
            yi <- yalli[goodvt]
            ## match original behavior: only populate if BOTH virtual values are present
            if (any(xi_chr == x_chr[1]) && any(xi_chr == x_chr[2])) {
              yhat.nonpar[1] <- mean(yi[xi_chr == x_chr[1]], na.rm = TRUE)
              yhat.nonpar[2] <- mean(yi[xi_chr == x_chr[2]], na.rm = TRUE)
            }
          }
        }
        ## causal estimate
        yhat.causal <- yhat.nonpar - yhat.nonpar[1]
        ## store
        yhat_nonpar_out[ii, ] <- yhat.nonpar
        yhat_causal_out[ii, ] <- yhat.causal
        bhat_out[ii, ] <- bhat
      }
    }
    ## --------------------------------------------------------------------------
    ##
    ## final processing (drop NULL cases)
    ##
    ## --------------------------------------------------------------------------
    if (!any(keep_case)) {
      return(NULL)
    }
    case_out <- case_ids[keep_case]
    goodvt_out <- goodvt_out[keep_case, , drop = FALSE]
    yhat_nonpar_out <- yhat_nonpar_out[keep_case, , drop = FALSE]
    yhat_causal_out <- yhat_causal_out[keep_case, , drop = FALSE]
    bhat_out <- bhat_out[keep_case, , drop = FALSE]
    ## --------------------------------------------------------------------------
    ##
    ## final processing of estimators:
    ## polynomial parametric estimator (only applies to continuous variables)
    ## nonparametric estimator
    ##
    ## --------------------------------------------------------------------------
    if (!binary.variable) {
      ## global mean intercept
      bhat_mean <- colMeans(bhat_out, na.rm = TRUE)
      bhat_mean[!is.finite(bhat_mean)] <- 0
      global.mean <- bhat_mean[1]
      ## fast parametric curve per case: global.mean + sum_k beta_k * x^k
      Xpow <- Xfull[, -1, drop = FALSE]                # nvirtual x df
      B <- bhat_out[, -1, drop = FALSE]                # ncase x df
      fitted <- rowSums(is.finite(bhat_out)) == ncol(bhat_out)
      B[!is.finite(B)] <- 0                            # temporary values for multiplication
      yhat.par <- global.mean + tcrossprod(B, Xpow)    # ncase x nvirtual
      yhat.par[!fitted, ] <- NA_real_                  # failed fits stay unavailable
      yhat.par[!is.finite(yhat.par)] <- NA_real_
      ## add back the global mean to the centered nonparametric curve
      yhat.nonpar <- yhat_nonpar_out + global.mean
    }
    else {
      yhat.par <- yhat.nonpar <- yhat_nonpar_out
    }
    ## --------------------------------------------------------------------------
    ##
    ## return the blob (for further processing downstream)
    ##
    ## --------------------------------------------------------------------------
    list(case = case_out,
         xorg = xorg,
         xvirtual = xvirtual,
         goodvt = goodvt_out,
         yhat.par = yhat.par,
         yhat.nonpar = yhat.nonpar,
         yhat.causal = yhat_causal_out)
  }) ## ends loop over variables
  ## ------------------------------------------------------------------------
  ##
  ## finalize: return
  ##
  ## ------------------------------------------------------------------------
  names(rO) <- variables
  class(rO) <- "partialpro"
  invisible(rO)
}
