partialpro <- function(object,
                       xvar.names,
                       nvar,
                       target,
                       learner,
                       newdata,
                       method = c("unsupv", "rnd", "auto"),
                       verbose = FALSE, ...)
{
  ## ------------------------------------------------------------------------
  ##
  ## incoming object must be a varpro object: extract relevant parameters
  ##
  ## ------------------------------------------------------------------------
  if (!inherits(object, "varpro")) {
    stop("object must be a varpro object")
  }
  ## set xvar.names here
  topvars <- get.topvars(object)
  if (missing(xvar.names)) {
    xvar.names <- topvars
  }
  ## filter xvar.names
  if (!missing(nvar)) {
    xvar.names <- xvar.names[1:min(length(xvar.names), nvar)]
  }
  ## extract x and set the dimension
  xvar <- object$x
  n <- nrow(xvar)
  ## pull the family
  family <- object$family
  ## set UVT method
  method <- match.arg(method, c("unsupv", "rnd", "auto"))
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
  ## regression
  if (is.numeric(yvar)) {
    target <- 1
  }
  ## classification
  else if (is.factor(yvar)) {
    ## set the target value
    yvar.levels <- levels(yvar)
    if (missing(target)) {
      target <- yvar.levels[length(yvar.levels)]
    }
    if (is.character(target)) {
      target <- match(match.arg(target, yvar.levels), yvar.levels)
    }
    else {
      if ((target > length(yvar.levels)) | (target < 1)) {
        stop("target is specified incorrectly:", target)
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
  ## obtain hidden options
  hidden <- get.partialpro.hidden(list(...))
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
  ## is UVT at play?
  cut.flag <- cut != 0
  ## ------------------------------------------------------------------------
  ##
  ## process the requested variables
  ##
  ## ------------------------------------------------------------------------
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
  ## isopro for isolation forests
  ##
  ## ------------------------------------------------------------------------
  if (cut.flag) {
    ## unsupervised method cannot be used if only one variable is present
    if (length(topvars) == 1 && method == "unsupv") {
      method <- "rnd"
    }
    ## isopro call
    o.iso <- isopro(data = xvar[, topvars, drop = FALSE], method = method,
                    sampsize = sampsize, ntree = ntree, nodesize = nodesize)
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
    tryCatch(stats::lm.fit(x = X, y = y), error = function(e) NULL)
  }
  .safe_pred <- function(fit, Xnew) {
    if (is.null(fit)) {
      return(rep(NA_real_, nrow(Xnew)))
    }
    drop(Xnew %*% fit$coefficients)
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
    ## unlimited virtual twins step: identify bad virtual twins
    goodvt_mat <- matrix(TRUE, nrow = ncase, ncol = nvirtual)
    if (cut.flag) {
      howbad <- tryCatch({
        ## prefer the columns used to train the isolation forest
        predict.isopro(o.iso, xfake[, topvars, drop = FALSE])
      }, error = function(e) {
        predict.isopro(o.iso, xfake)
      })
      if (sum(howbad >= cut) == 0) {
        return(NULL)
      }
      goodvt_mat <- matrix(howbad >= cut, nrow = ncase, ncol = nvirtual, byrow = TRUE)
    }
    ## obtain predicted value for fake partial data
    ## (IMPORTANT: pass only feature columns to learner; case/train/goodvt are internal)
    pred <- learner(xfake)
    yhat <- as.numeric(cbind(pred)[, target])
    if (family == "class") {
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
              ## switch to no cut based on out-of-sample mse performance
              if (mymse(ytest, ytest.nocut) < (mymse(ytest, ytest.cut) - mse.tolerance)) {
                fit_sel <- .safe_lm_fit(Xfull, yalli)
              } else {
                fit_sel <- .safe_lm_fit(Xfull[goodvt, , drop = FALSE], yalli[goodvt])
              }
            }
          }
          ## cut.flag is off OR not enough data for out-of-sample performance
          ## (match original behavior: only run this fallback when the OOS branch
          ## is NOT entered; if OOS is entered but fitting fails, leave NA's)
          if (is.null(fit_sel) && !(cut.flag && sum(train & goodvt) > (nmin / 2))) {
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
      B[!is.finite(B)] <- 0                            # emulate na.rm=TRUE in original rowSums
      yhat.par <- global.mean + tcrossprod(B, Xpow)    # ncase x nvirtual
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
