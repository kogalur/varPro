## random forest censoring distribution
get.cens.dist <- function(data, ntree, nodesize, ssize) {
  colnames(data)[1:2] <- c("time", "cens")
  data$cens <- 1 * (data$cens == 0)
  ssize <- min(ssize, eval(formals(randomForestSRC::rfsrc.fast)$sampsize)(nrow(data)), na.rm = TRUE)
  cens.o <- randomForestSRC::rfsrc(Surv(time, cens) ~ ., data, splitrule = "random",
                       ntree = ntree, nodesize = nodesize, sampsize = ssize, perf.type = "none")
  list(surv = cens.o$survival.oob, time.interest = cens.o$time.interest)
}
get.crps <- function (o, papply = mclapply, cens.dist = NULL)  {
  if (!is.null(o$survival.oob)) {
    surv.ensb <- t(o$survival.oob)
  }
  else {
    surv.ensb <- t(o$survival)
    o$yvar <- o$forest$yvar
  }
  event.info <- randomForestSRC:::get.event.info(o)
  ## KM censoring distribution estimator
  if (is.null(cens.dist)) {
    cens.model <- "km"
    censTime <- sort(unique(event.info$time[event.info$cens == 0]))
    censTime.pt <- c(sIndex(censTime, event.info$time.interest))
    if (length(censTime) > 0) {
      censModel.obj <- do.call(rbind, papply(1:length(censTime), function(j) {
        c(sum(event.info$time >= censTime[j], na.rm = TRUE), 
          sum(event.info$time[event.info$cens == 0] == 
              censTime[j], na.rm = TRUE))
      }))
      Y <- censModel.obj[, 1]
      d <- censModel.obj[, 2]
      r <- d/(Y + 1 * (Y == 0))
      cens.dist <- c(1, exp(-cumsum(r)))[1 + censTime.pt]
    }
    else {
      cens.dist <- rep(1, length(censTime.pt))
    }
  }
  else {## random forest censoring distribution
    cens.model <- "rfsrc"
    censTime.pt <- c(sIndex(cens.dist$time.interest, event.info$time.interest))
    cens.dist <- t(cbind(1, cens.dist$surv)[, 1 + censTime.pt])
  }
  ## brier calculation
  brier.matx <- do.call(rbind, papply(1:ncol(surv.ensb), function(i) {
    tau <- event.info$time
    event <- event.info$cens
    t.unq <- event.info$time.interest
    cens.pt <- sIndex(t.unq, tau[i])
    if (cens.model == "km") {
      c1 <- 1 * (tau[i] <= t.unq & event[i] != 0)/c(1, cens.dist)[1 + cens.pt]
      c2 <- 1 * (tau[i] > t.unq)/cens.dist
    }
    else {
      c1 <- 1 * (tau[i] <= t.unq & event[i] != 0)/c(1, cens.dist[, i])[1 + cens.pt]
      c2 <- 1 * (tau[i] > t.unq) / cens.dist[, i]
    }
    (1 * (tau[i] > t.unq) - surv.ensb[, i])^2 * (c1 + c2)
  }))
  brier.score <- data.frame(time = event.info$time.interest, 
                            brier.score = colMeans(brier.matx, na.rm = TRUE))
  crps <- trapz(brier.score$time, brier.score$brier.score)
  crps / max(brier.score$time)
}
## This is the analog of get.rf.cores(). It sets the core usage for use by the "parallel" package.
get.mc.cores <- function() {
    ## PART I:  Two ways for the user to specify cores:
    ## (1) R-option "mc.cores"
    ## (2) Shell-environment-option "MC_CORES"
    if (is.null(getOption("mc.cores", NULL))) {
        if (!is.na(as.numeric(Sys.getenv("MC_CORES")))) {
            options(mc.cores = as.integer(Sys.getenv("MC_CORES")))
        }
    }
    ## If the user has set the cores using either of the two methods, we respect it.
    if (!is.null(getOption("mc.cores", NULL))) {
        return (getOption("mc.cores"))
    }
    ## PART II:  Respect R CMD check limit
    chk <- tolower(Sys.getenv("_R_CHECK_LIMIT_CORES_", ""))
    if (nzchar(chk) && chk != "false") {
        ## under R CMD check --as-cran (CRAN sets this)
        return(2L)
    }
    ## PART III:  Use everything.
    n <- parallel::detectCores(logical = TRUE)
    if (is.na(n) || n < 1L) n <- 1L
    return(n)
}
## standard error workhorse
get.sderr.workhorse <- function(obj, standardize = TRUE, outcome.target = NULL,
                       crps = FALSE, papply = mclapply, cens.dist = NULL) {
  ## set the target response outcome
  ynms <- obj$yvar.names
  if (is.null(outcome.target)) {
    ynms <- ynms[1]
  }
  else {
    ynms <- intersect(ynms, outcome.target)
    if (length(ynms) == 0) {
      stop("outcome.target incorrectly specified")
    }
  }
  ## CRPS calculation for survival families
  if (obj$family == "surv" && crps) {
    get.crps(obj, papply = papply, cens.dist = cens.dist)
  }
  ## default is to get the error rate
  else {
    err <- lapply(ynms, function(nn) {
    o.coerce <- randomForestSRC:::coerce.multivariate(obj, nn)
    er <- o.coerce$err.rate
    if (o.coerce$family == "class") {
      utils::tail(na.omit(er[, 1]), 1)
    }
    else if (o.coerce$family == "surv") {
      utils::tail(na.omit(er), 1)
    }
    else if (o.coerce$family == "regr+") {
      utils::tail(na.omit(er), 1) / var(o.coerce$yvar[, outcome.target], na.rm = TRUE)
    }
    else {
      utils::tail(na.omit(er), 1) / var(o.coerce$yvar, na.rm = TRUE)
    }
    })
    unlist(err)
  }
}
## standard error
get.sderr <- function(obj, nblocks,
                      outcome.target = NULL,
                      crps = FALSE,
                      papply = mclapply,
                      newdata = NULL,
                      imbalanced.obj = NULL,
                      cens.dist = NULL) {
  ## error metrics are normalized so that > 1.0 is bad.
  ## use normalized brier score for classification
  ## brier score is over-ridden with gmean for imbalanced two class setting
  nblocks <- min(nblocks, obj$ntree)
  if (obj$family == "class") {
    if (is.null(imbalanced.obj)) {
      perf.type <- "brier"
    }
    else {
      perf.type <- imbalanced.obj$perf.type
    }
  }
  else {
    if (obj$family == "surv" && crps) {
      perf.type <- "none"
    }
    else {
      perf.type <- "default"
    }
  }
  ## trivial case
  if (nblocks == 1) {
    return(c(get.sderr.workhorse(randomForestSRC::predict.rfsrc(obj,
          perf.type = perf.type), outcome.target = outcome.target,
          crps = crps, papply = papply, cens.dist = cens.dist), 0))
  }
  ## extract error rates for blocks of trees
  tree.seq <- unique(c(1, round(seq(1, obj$ntree, length = nblocks)), obj$ntree))
  err <- sapply(1:(length(tree.seq) - 1), function(j) {
    if (is.null(newdata)) {
      get.sderr.workhorse(randomForestSRC::predict.rfsrc(obj,
        get.tree = tree.seq[j]:tree.seq[j+1], perf.type = perf.type),
        outcome.target = outcome.target, crps = crps, papply = papply, cens.dist = cens.dist)
    }
    else {
     get.sderr.workhorse(randomForestSRC::predict.rfsrc(obj,
        newdata = newdata, get.tree = tree.seq[j]:tree.seq[j+1], perf.type = perf.type),
        outcome.target = outcome.target, crps = crps, papply = papply, cens.dist = cens.dist)
    } 
  })
  ## return the mean and standard deviation of the blocked error rates
  c(mean(err, na.rm = TRUE), sd(err, na.rm = TRUE))
}
## unregeister foreach backend
## https:
## https:
#get.unregister <- function() {
#  env <- utils::getFromNamespace(".foreachGlobals", "foreach")
#  rm(list=ls(name=env), pos=env)
#}
trapz <- function (x, y) {
  idx = 2:length(x)
  return(as.double((x[idx] - x[idx - 1]) %*% (y[idx] + y[idx - 1]))/2)
}
sIndex <- function (x, y) {
  sapply(1:length(y), function(j) {
    sum(x <= y[j])
  })
}
