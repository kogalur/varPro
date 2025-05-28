partialpro <- function(object,
                       xvar.names,
                       nvar,
                       target,
                       learner,
                       newdata,
                       method = c("unsupv", "rnd", "auto"),
                       verbose = FALSE,
                       papply = mclapply, ...)
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
  nvirtual <- hidden$nvirtual
  nmin <- hidden$nmin
  alpha <- hidden$alpha
  df <- round(max(1, hidden$df))
  sampsize <- hidden$sampsize
  ntree <- hidden$ntree
  nodesize <- hidden$nodesize
  mse.tolerance <- hidden$mse.tolerance
  ## set formula (do not use "y" for the yvar name)
  yfkname <- "y123XYZ9999abc"
  f <- paste0(yfkname, "~1+x")
  if (df > 1) {
    f <- paste0(f, paste(sapply(2:df, function(k) {paste0("+I(x^", k, ")")}), collapse = ""))
  }
  f <- as.formula(f)
  ## is UVT at play?
  cut.flag <- cut !=0
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
    xvirtual <- myunique(xorg, nvirtual, alpha)
    nvirtual <- length(xvirtual)
    ## --------------------------------------------------------
    ## make fake partial data
    ## --------------------------------------------------------
    ## default setting (using training data)
    ## draw random cases
    if (!predict.flag) {
      smp <- sample(1:n, size = min(n, nsmp), replace = FALSE)
      xfake <- do.call(rbind, papply(smp, function(i) {
        dfake <- xvar[i,, drop = FALSE]
        dfake <- dfake[rep(1, nvirtual),, drop = FALSE]
        dfake[, xnm] <- xvirtual
        data.frame(case = i, train = mytrainsample(nvirtual), goodvt = 1, dfake)
      }))
    }
    ## newdata is present - use this for creating the fake data
    else {
      if (sum(!(colnames(xvar)  %in% colnames(newdata))) > 0) {
        stop("x-variables in newdata does not match original data")
      }
      newdata <- newdata[, colnames(xvar), drop=FALSE]
      xfake <- do.call(rbind, papply(1:nrow(newdata), function(i) {
        dfake <- newdata[i,, drop = FALSE]
        dfake <- dfake[rep(1, nvirtual),, drop = FALSE]
        dfake[, xnm] <- xvirtual
        data.frame(case = i, train = mytrainsample(nvirtual), goodvt = 1, dfake)
      }))
    }
    ## unlimited virtual twins step: identify bad virtual twins
    if (cut.flag) {
      howbad <- predict.isopro(o.iso, xfake)
      if (sum(howbad >= cut) == 0) {
        return(NULL)
      }
      xfake$goodvt[howbad < cut] <- 0
    }
    ## obtain predicted value for fake partial data
    yhat <- as.numeric(cbind(learner(xfake))[, target])
    if (family == "class") {
      yhat <- mylogodds(yhat)
    }
    ## --------------------------------------------------------------------------
    ##
    ## loop over cases, obtaining nonparametric supersmooth fit
    ##
    ## --------------------------------------------------------------------------
    rOcase <- papply(unique(xfake$case), function(i) {
      ## pointers for case i
      pt <- xfake$case == i
      train <- xfake$train[pt] == 1
      goodvt <- xfake$goodvt[pt] == 1
      ## need a reasonable number of good twins
      ## -  over-ride for binary case
      ## -  make exception for discrete value x's
      if (sum(goodvt) >= min(nmin, nxorg / 2) || binary.variable) {
        ## extract the x, y data
        xi <- xvirtual[goodvt]
        yi <- yhat[pt][goodvt]
        yalli <- yhat[pt]
        yhat.nonpar <- rep(NA, nvirtual)
        bhat <- rep(NA, df + 1)
        ## --------------------------------------------------------------------------
        ##
        ## continuous variable fit
        ##
        ## --------------------------------------------------------------------------
        if (!binary.variable) {
          ##----------------------------------
          ##
          ## local polynomial estimation
          ##
          ##----------------------------------
          if (cut.flag && sum(train[goodvt]) > (nmin / 2)) {
            o.lm.cut <- tryCatch({suppressWarnings(lm(f,
               setNames(data.frame(yi[train[goodvt]], xi[train[goodvt]]), c(yfkname, "x"))))},
                      error = function(ex) {NULL})
            o.lm.nocut <- tryCatch({suppressWarnings(lm(f,
               setNames(data.frame(yalli[train], xvirtual[train]), c(yfkname, "x"))))},
                      error = function(ex) {NULL})
            ## switch to no cut based on out-of-sample mse performance
            if (!is.null(o.lm.cut) && !is.null(o.lm.nocut)) {
              ytest.cut <- predict.lm(o.lm.cut, data.frame(x = xvirtual[!train]))
              ytest.nocut <- predict.lm(o.lm.nocut, data.frame(x = xvirtual[!train]))
              ytest <- yalli[!train]
              if (mymse(ytest, ytest.nocut) < (mymse(ytest, ytest.cut) - mse.tolerance)) {
                o.lm <- lm(f, setNames(data.frame(yalli, xvirtual), c(yfkname, "x")))
                yhat.nonpar <- o.lm$fitted
              }
              else {
                o.lm <- lm(f, setNames(data.frame(yi, xi), c(yfkname, "x")))
                yhat.nonpar <- predict.lm(o.lm, data.frame(x = xvirtual))
              }
            }
            else {
              NULL
            }
          }
          ## cut.flag is off or not enough data for out-of-sample performace
          else {
            o.lm <- tryCatch({suppressWarnings(lm(f, setNames(data.frame(yi, xi), c(yfkname, "x"))))},
                  error = function(ex) {NULL})
            if (!is.null(o.lm)) {
              yhat.nonpar <- predict.lm(o.lm, data.frame(x = xvirtual)) 
            }
            else {
              NULL
            }
          }
          if (!is.null(o.lm)) {
            bhat <- o.lm$coef
            yhat.nonpar <- yhat.nonpar - bhat[1]
          }
        }
        ## --------------------------------------------------------------------------
        ##
        ## binary variable fit
        ##
        ## --------------------------------------------------------------------------
        ## both virtual twins must be available since extrapolation not possible
        ## if one is missing, set entire case to NA
        else {
          binary.yhat <- tapply(yi, xi, mean, na.rm = TRUE)
          if (length(binary.yhat) == 2) {
            if (xvirtual[1] %in% names(binary.yhat)) {
              yhat.nonpar[1] <- binary.yhat[1]
            }
            if (xvirtual[2] %in% names(binary.yhat)) {
              yhat.nonpar[2] <- binary.yhat[2]
            }
          }
        }
        ## --------------------------------------------------------------------------
        ##
        ## causal estimate
        ##
        ## --------------------------------------------------------------------------
        yhat.causal <- yhat.nonpar - yhat.nonpar[1]
        ## --------------------------------------------------------------------------
        ##
        ## track the virtual twins by using NA's for bad cases (used for processing later)
        ##
        ## --------------------------------------------------------------------------
        goodvt <- 1 * goodvt
        goodvt[goodvt != 1] <- NA
        ## --------------------------------------------------------------------------
        ##
        ## return goodies
        ##
        ## --------------------------------------------------------------------------
        list(case = i,
             goodvt = goodvt,
             bhat = bhat,
             yhat.nonpar = yhat.nonpar,
             yhat.causal = yhat.causal)
      }
      else {
        NULL
      }
    })
    ## --------------------------------------------------------------------------
    ##
    ## final processing
    ##
    ## --------------------------------------------------------------------------
    ## remove null cases
    rOcase <- rOcase[!sapply(rOcase, is.null)]
    if (length(rOcase) == 0) {
      return(NULL)
    }
    ## --------------------------------------------------------------------------
    ##
    ## final processing of estimators:
    ## polynomial parametric estimator (only applies to continuous variables)
    ## nonparametric estimator
    ##
    ## --------------------------------------------------------------------------
    if (!binary.variable) {
      bhat.all <- do.call(rbind, lapply(rOcase, function(oo) {oo$bhat}))
      bhat <- colMeans(bhat.all, na.rm = TRUE)
      bhat[is.na(bhat)] <- 0
      global.mean <- bhat[1]
      #yhat.par <- global.mean +
      #   rowSums(do.call(cbind, lapply(1:df, function(k) {bhat[1+k] * xvirtual ^ k})), na.rm = TRUE)
      yhat.par <- global.mean + t(apply(bhat.all, 1, function(bhat) {
        rowSums(do.call(cbind, lapply(1:df, function(k) {bhat[1+k] * xvirtual ^ k})), na.rm=TRUE)
      }))
      yhat.nonpar <- do.call(rbind, lapply(rOcase, function(oo) {oo$yhat.nonpar + global.mean}))       
    }
    else {
      yhat.par <- yhat.nonpar <- do.call(rbind, lapply(rOcase, function(oo) {oo$yhat.nonpar}))
    }
    ## --------------------------------------------------------------------------
    ##
    ## return the blob (for further processing downstream)
    ##
    ## --------------------------------------------------------------------------
    list(case = sapply(rOcase, function(oo) {oo$case}),
         xorg = xorg,
         xvirtual = xvirtual,
         goodvt = do.call(rbind, lapply(rOcase, function(oo) {oo$goodvt})),
         yhat.par = yhat.par,
         yhat.nonpar = yhat.nonpar,
         yhat.causal = do.call(rbind, lapply(rOcase, function(oo) {oo$yhat.causal}))
         )
  })### ends loop over variables
  ## ------------------------------------------------------------------------
  ##
  ## finalize: return
  ##
  ## ------------------------------------------------------------------------
  names(rO) <- variables
  class(rO) <- "partialpro"
  invisible(rO)
}
