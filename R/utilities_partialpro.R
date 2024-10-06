## hidden options
get.partialpro.hidden <- function(dots) {
  list(cut = if (is.null(dots$cut)) .1 else dots$cut,
       nsmp = if (is.null(dots$nsmp)) 250 else dots$nsmp,
       nvirtual = if (is.null(dots$nvirtual)) 100 else dots$nvirtual,
       nmin = if (is.null(dots$nmin)) 15 else dots$nmin,
       alpha = if (is.null(dots$alpha)) .025 else dots$alpha,
       bass = if (is.null(dots$bass)) 10 else dots$bass,
       df = if (is.null(dots$df)) 2 else dots$df,
       sampsize = if (is.null(dots$sampsize)) function(x){min(2^8, .632 * x)} else dots$sampsize,
       ntree =  if (is.null(dots$ntree)) 500 else dots$ntree,
       nodesize = if (is.null(dots$ntree)) 1 else dots$nodesize,
       mse.tolerance = if (is.null(dots$mse.tolerance)) 0 else dots$mse.tolerance
       )
}
## robust log-odds
mylogodds <- function(p, eps = .001) {
  p[p <= eps] <- eps
  p[p >= (1 - eps)] <- 1 - eps
  log(p / (1 - p))
}
## robust standardize mse
mymse <- function(y, yhat) {
  yvar <- var(y, na.rm = TRUE)
  if (is.na(yvar) || yvar == 0) {
    yvar <- 1
  }
  mean((y - yhat)^2, na.rm = TRUE) / yvar
}   
## used for balanced train/testing
mytrainsample <- function(n, frc = .632) {
  n1 <- round(n * max(0.5, frc))
  n0 <- n - n1
  ## degenerate case
  if (n == 1) {
    1
  }
  ## usual case
  else {
    if (n0 > 0) {
      sample(c(rep(1, n1), rep(0, n0)))
    }
    else {
      sample(c(rep(1, n - 1), 0))
    }
  }
}
## uniquify the feature
myunique <- function(x, npts, alpha = .05, nfactor = 10) {
  ## factors
  if (is.factor(x) || length(unique(x)) <= nfactor) {
    if (is.factor(x)) {
      levels(x)
    }
    else {
      sort(unique(x))
    }
  }
  ## non-factors
  else {
    xorg <- sort(unique(x))
    q.out <- quantile(x, probs = c(alpha, 1 - alpha))
    x <- sort(unique(x[x >= q.out[1] & x <= q.out[2]]))
    unique(quantile(x, prob = (1:npts) / npts))
  }
}
###################################################################
##
## custom learners
##
###################################################################
##---------------------------------------------------------------
##
## bart learner
##
##---------------------------------------------------------------
predict.wbart <- utils::getFromNamespace("predict.wbart", "BART")
bart.learner <- function(o, ...) {
  ## input value must be a varpro object
  if (!inherits(o, "varpro", TRUE)) {
    stop("object must be a varpro object")
  }
  if (!(o$family == "regr")) {
    stop("only applies for regression/survival")
  }
  ## bart call
  invisible(capture.output(mybartlearner <- wbart(x.train = o$x[, o$xvar.names, drop = FALSE],
                                  y.train = o$y)))
  ## user allowed options
  dots <- list(...)
  mc.cores <- if (is.null(dots$mc.cores)) getOption("mc.cores") else dots$mc.cores
  function(x) {
    if (missing(x)) {
      mybartlearner$yhat.train.mean
    }
    else {
      invisible(capture.output(yhat <- predict.wbart(mybartlearner,
      x[, o$xvar.names, drop = FALSE], mc.cores = mc.cores)))
      colMeans(yhat, na.rm = TRUE)
    }
  }
}
##---------------------------------------------------------------
##
## boosted learner
##
##---------------------------------------------------------------
gbm.learner <- function(o, ...) {
  ## input value must be a varpro object
  if (!inherits(o, "varpro", TRUE)) {
    stop("object must be a varpro object")
  }
  if (!(o$family == "regr" || o$family == "class")) {
    stop("only applies for regression/survival and classification")
  }
  ## special handling for class
  if (o$family == "class") {
    ylevels <- levels(o$y)
    if (length(ylevels) > 2) {
      stop("classification only applies to two-class problems")
    }
    o$y <- as.numeric(factor(as.character(o$y), levels = c(0, 1))) - 1
  }
  ## user allowed options
  dots <- list(...)
  n.trees <- if (is.null(dots$n.trees)) 500 else dots$n.trees
  shrinkage <- if (is.null(dots$shrinkage)) 0.1 else dots$shrinkage
  interaction.depth <- if (is.null(dots$interaction.depth)) 3 else dots$interaction.depth
  cv.folds <- if (is.null(dots$cv.folds)) 5 else dots$cv.folds
  n.cores  <- if (is.null(dots$n.cores)) getOption("mc.cores") else dots$n.cores
  ## gbm call
  gbm.dta <- data.frame(y = o$y, o$x[, o$xvar.names, drop = FALSE])
  suppressWarnings(mygbmlearner <- gbm(y~., data = gbm.dta,
                                distribution = if (o$family=="class") "bernoulli" else "gaussian",
                                n.trees = n.trees,
                                shrinkage = shrinkage,
                                cv.folds = cv.folds,
                                interaction.depth = interaction.depth,
                                bag.fraction = .632,
                                train.fraction = 0.5,  
                                n.minobsinnode = min(10, nrow(gbm.dta)/2),
                                keep.data = TRUE,
                                verbose = FALSE,
                                n.cores = n.cores))
  best.iter <- gbm.perf(mygbmlearner, plot.it = FALSE, method = "cv")
  ## construct the learner function
  if (o$family == "regr") {
    function(x) {
      if (missing(x)) {
        predict.gbm(mygbmlearner, n.trees = best.iter)
      }
      else {
        predict.gbm(mygbmlearner, x, n.trees = best.iter)
      }
    }
  }
  else {
    function(x) {
      if (missing(x)) {
        yhat <- predict.gbm(mygbmlearner, n.trees = best.iter, type = "response")
      }
      else {
        yhat <- predict.gbm(mygbmlearner, x, n.trees = best.iter, type = "response")
      }
      cbind(1 - yhat, yhat)
    }
  }
}
##---------------------------------------------------------------
##
## rf learner
##
##---------------------------------------------------------------
rf.learner <- function(o, ...) {
  ## input value must be a varpro object
  if (!inherits(o, "varpro", TRUE)) {
    stop("object must be a varpro object")
  }
  ## custom defined xvar.wt - much gentler than the sparse varpro value
  xvar.wt <- rep(0, length(o$xvar.names))
  names(xvar.wt) <- o$xvar.names
  xvar.wt[get.topvars(o)] <- 1
  ## user allowed options
  dots <- list(...)
  rfnames <- randomForestSRC:::get.rfnames(hidden = TRUE)
  rfnames <- rfnames[rfnames != "formula" & rfnames != "data"]
  dots <- dots[names(dots) %in% rfnames]
  dots$formula <- as.formula("y~.")
  dots$xvar.wt <- xvar.wt
  dots$perf.type <- "none"
  ## correct processing of class labels in classification also
  ## provides a back door for 0/1 real valued var pro analysis to be
  ## treated as classification
  y <- if (is.factor(o$y.org)) factor(o$y.org) else o$y  
  ## rfsrc call  
  myrflearner <- do.call("rfsrc",
   c(list(data=data.frame(y = y, o$x[, o$xvar.names, drop = FALSE])), dots))
  ## construct the learner function
  function(x) {
    if (missing(x)) {
      predict.rfsrc(myrflearner, perf.type = "none")$predicted.oob
    }
    else {
      predict.rfsrc(myrflearner, x, perf.type = "none")$predicted
    }
  }
}
