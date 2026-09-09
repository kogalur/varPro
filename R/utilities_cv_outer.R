## Internal helpers for optional outer assessment of cv.varpro().
## The inner tree-block selector and get.sderr.workhorse() are shared.
.cv.varpro.rules <- function() c("imp", "imp.conserve", "imp.liberal")
## Evaluate without changing the caller's RNG state, including on failure.
.cv.varpro.with.seed <- function(seed, expr) {
  present <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  old <- if (present) get(".Random.seed", envir = .GlobalEnv) else NULL
  kind <- RNGkind()
  on.exit({
    if (!identical(RNGkind(), kind)) do.call(RNGkind, as.list(kind))
    if (present) {
      assign(".Random.seed", old, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  if (!is.null(seed)) set.seed(seed)
  force(expr)
}
## Locate the rows retained by the existing input preparation. Compare
## values as well as row names so reset row names cannot misalign foldid.
.cv.varpro.analysis.rows <- function(input, clean) {
  nms <- names(clean)
  if (!all(nms %in% names(input))) {
    stop("outer CV could not match the prepared columns to the input data")
  }
  same <- function(rows) {
    if (length(rows) != nrow(clean) || anyNA(rows) || anyDuplicated(rows)) {
      return(FALSE)
    }
    all(vapply(nms, function(nm) {
      a <- input[[nm]][rows]
      b <- clean[[nm]]
      if (is.factor(a) || is.factor(b) || is.character(a) || is.character(b)) {
        a <- as.character(a)
        b <- as.character(b)
      }
      isTRUE(all.equal(unname(a), unname(b), check.attributes = FALSE,
                       tolerance = 0))
    }, logical(1)))
  }
  rows <- match(rownames(clean), rownames(input))
  if (same(rows)) return(rows)
  rows <- which(complete.cases(input[, nms, drop = FALSE]))
  if (same(rows)) return(rows)
  stop(paste("outer CV could not align prepared observations with input rows;",
             "supply a complete-case data frame and matching foldid"))
}
## Describe the metric actually requested of the fold's grow object.
.cv.varpro.metric <- function(info, crps) {
  if (info$family == "surv") {
    if (crps) "crps" else "cindex"
  } else if (info$family %in% c("regr", "regr+")) {
    "normalized.mse"
  } else {
    info$perf.type
  }
}
.cv.varpro.folds <- function(data, family, yvar.names, cv.folds, foldid = NULL) {
  n <- nrow(data)
  if (n < 4L) stop("outer CV requires at least four complete observations")
  response <- data[[yvar.names[1L]]]
  strata <- rep.int(1L, n)
  limit <- n %/% 2L
  if (family == "class") {
    strata <- factor(response)
    counts <- table(strata)
    if (length(counts) < 2L || min(counts) < 2L) {
      stop("outer CV requires at least two observations in every observed response class")
    }
    limit <- min(limit, min(counts))
  } else if (family == "surv") {
    event <- data[[yvar.names[2L]]] != 0
    if (sum(event) < 2L) stop("outer CV requires at least two observed events")
    strata <- factor(event)
    limit <- min(limit, sum(event))
  }
  if (is.null(foldid)) {
    k <- min(cv.folds, limit)
    if (k < 2L) stop("the data cannot support two outer folds")
    if (k != cv.folds) {
      warning("cv.folds reduced to ", k,
              " to retain adequate validation sizes and outcome support",
              call. = FALSE)
    }
    foldid <- integer(n)
    ## Keep fold sizes balanced within strata and over the complete data.
    ## Randomly permute the fold order; rotate after each stratum.
    labels <- sample.int(k)
    offset <- 0L
    for (idx in split(seq_len(n), strata, drop = TRUE)) {
      idx <- idx[sample.int(length(idx))]
      foldid[idx] <- labels[((seq_along(idx) - 1L + offset) %% k) + 1L]
      offset <- (offset + length(idx)) %% k
    }
  } else {
    if (!is.numeric(foldid) || length(foldid) != n || anyNA(foldid) ||
        any(!is.finite(foldid)) || any(foldid < 1) ||
        any(foldid != floor(foldid))) {
      stop("foldid must contain positive integer labels for every retained observation")
    }
    labels <- sort(unique(foldid))
    k <- length(labels)
    if (k < 2L || !identical(as.numeric(labels), as.numeric(seq_len(k)))) {
      stop("the retained foldid labels must be consecutive integers starting at one")
    }
    if (cv.folds > 0 && cv.folds != k) {
      stop("cv.folds does not agree with the number of supplied folds")
    }
    foldid <- as.integer(foldid)
  }
  for (j in seq_len(k)) {
    test <- foldid == j
    if (sum(test) < 2L || sum(!test) < 2L) {
      stop("outer fold ", j, " must have at least two training and two validation observations")
    }
    if (family == "class") {
      observed <- levels(factor(response))
      if (!all(observed %in% as.character(response[test])) ||
          !all(observed %in% as.character(response[!test]))) {
        stop("outer fold ", j, " must contain every observed response class in training and validation")
      }
    } else if (family == "surv") {
      event <- data[[yvar.names[2L]]] != 0
      if (!any(event[test]) || !any(event[!test])) {
        stop("outer fold ", j, " must contain events in training and validation")
      }
    } else if (!is.finite(var(response[!test])) || var(response[!test]) <= 0) {
      stop("the first response is constant in the training portion of outer fold ", j)
    }
  }
  list(foldid = foldid, folds = k)
}
## The top-variable fallback is used only when outer assessment is enabled.
## Keep the original cutoff and record the forced selection explicitly.
.cv.varpro.complete <- function(object, importance) {
  rules <- .cv.varpro.rules()
  fallback <- setNames(rep(FALSE, length(rules)), rules)
  for (nm in rules) {
    if (is.null(object[[nm]]) || NROW(object[[nm]]) == 0L) {
      usable <- which(!is.na(importance$variable) & !is.na(importance$z))
      if (!length(usable)) {
        stop("a top-variable fallback requires a usable original-variable importance ranking")
      }
      object[[nm]] <- importance[usable[1L], , drop = FALSE]
      fallback[nm] <- TRUE
    }
  }
  list(object = object, fallback = fallback)
}
## Grow one whole forest per distinct selected set. Sampling and forest
## seeds are shared across these fits, as in the existing cutoff search.
.cv.varpro.assess <- function(selection, info, train, test, formula, crps, seed) {
  rules <- .cv.varpro.rules()
  cut.names <- c("zcut", "zcut.conserve", "zcut.liberal")
  ssize <- info$sampsize
  if (!is.numeric(ssize) || length(ssize) != 1L || !is.finite(ssize) ||
      ssize <= 0 || ssize > nrow(train)) {
    stop("sampsize must be positive and no larger than the outer training sample")
  }
  samp <- randomForestSRC:::make.sample(info$ntree, nrow(train), ssize)
  forest.seed <- get.seed(seed)
  cens.dist <- NULL
  if (info$family == "surv" && crps) {
    nms <- unique(c(info$yvar.names, info$importance$variable))
    cens.dist <- get.cens.dist(train[, nms, drop = FALSE],
                               ntree = info$ntree, nodesize = info$nodesize,
                               ssize = ssize, newdata = test[, nms, drop = FALSE])
  }
  fitted.sets <- list()
  errors <- numeric()
  metric <- .cv.varpro.metric(info, crps)
  records <- lapply(seq_along(rules), function(j) {
    variables <- as.character(selection$object[[rules[j]]]$variable)
    if (!length(variables) || anyNA(variables) || anyDuplicated(variables) ||
        !all(variables %in% setdiff(names(train), info$yvar.names))) {
      stop("invalid selected predictors for ", rules[j])
    }
    ## Use the shared ranking order, as in the inner candidate forests.
    variables <- as.character(info$importance$variable[
      info$importance$variable %in% variables])
    matched <- which(vapply(fitted.sets, identical, logical(1), variables))
    if (length(matched)) {
      err <- errors[matched[1L]]
    } else {
      nms <- c(info$yvar.names, variables)
      grow.args <- list(formula = formula, data = train[, nms, drop = FALSE],
                        ntree = info$ntree, rfq = info$rfq,
                        splitrule = info$splitrule, perf.type = info$perf.type,
                        bootstrap = "by.user", samp = samp, seed = forest.seed,
                        forest = TRUE)
      if (isTRUE(info$fast)) {
        forest <- do.call(randomForestSRC::rfsrc.fast, grow.args)
      } else {
        grow.args$nodesize <- info$nodesize
        forest <- do.call(rfsrc, grow.args)
      }
      ## Inherit the grow settings. Outcomes in test supply err.rate.
      predicted <- predict.rfsrc(forest, newdata = test[, nms, drop = FALSE])
      err <- get.sderr.workhorse(predicted, crps = crps, cens.dist = cens.dist)
      if (length(err) > 1L) stop("outer assessment expected one workhorse error per fold")
      if (!length(err) || !is.finite(err)) err <- NA_real_
      err <- unname(as.numeric(err))
      fitted.sets[[length(fitted.sets) + 1L]] <- variables
      errors <- c(errors, err)
    }
    data.frame(n.train = nrow(train), n.test = nrow(test),
               nvar = length(variables),
               zcut = selection$object[[cut.names[j]]], err = err,
               perf.type = metric, fallback = unname(selection$fallback[rules[j]]),
               status = if (is.finite(err)) "ok" else "undefined error",
               stringsAsFactors = FALSE)
  })
  names(records) <- rules
  records
}
.cv.varpro.outer <- function(full, info, args, cv.folds, foldid, n.input, seed) {
  rules <- .cv.varpro.rules()
  data <- info$data
  variables <- setdiff(names(data), info$yvar.names)
  completed <- .cv.varpro.complete(full, info$importance)
  full <- completed$object
  if (!is.null(seed)) {
    if (!is.numeric(seed) || length(seed) != 1L || !is.finite(seed)) {
      stop("seed must be a finite numeric scalar")
    }
    set.seed(as.integer(1 + abs(as.double(seed)) %% (.Machine$integer.max - 1)))
  }
  fold.seed <- sample.int(.Machine$integer.max, 1L)
  supplied <- if (is.null(foldid)) NULL else foldid[info$rows]
  partition <- .cv.varpro.with.seed(fold.seed, {
    .cv.varpro.folds(data, info$family, info$yvar.names, cv.folds, supplied)
  })
  k <- partition$folds
  selection.seed <- sample.int(.Machine$integer.max, k)
  prediction.seed <- sample.int(.Machine$integer.max, k)
  z <- matrix(NA_real_, nrow = length(variables), ncol = k,
              dimnames = list(variables, NULL))
  counts <- matrix(0L, nrow = length(variables), ncol = length(rules),
                   dimnames = list(variables, rules))
  records <- setNames(lapply(rules, function(nm) vector("list", k)), rules)
  for (j in seq_len(k)) {
    if (isTRUE(args$verbose)) cat("outer CV fold", j, "of", k, "\n")
    test.rows <- which(partition$foldid == j)
    train.rows <- which(partition$foldid != j)
    train <- data[train.rows, , drop = FALSE]
    test <- data[test.rows, , drop = FALSE]
    tryCatch({
      fold.args <- args
      fold.args$data <- train
      fold.args$seed <- selection.seed[j]
      fold <- .cv.varpro.with.seed(selection.seed[j], {
        do.call(.cv.varpro.select, fold.args)
      })
      fold.info <- attr(fold, ".cv.info")
      attr(fold, ".cv.info") <- NULL
      if (!identical(as.integer(fold.info$rows), seq_len(nrow(train)))) {
        stop("inner input preparation changed the outer training rows")
      }
      selected <- .cv.varpro.complete(fold, fold.info$importance)
      idx <- match(fold.info$importance$variable, variables)
      if (anyNA(idx) || anyDuplicated(idx)) {
        stop("fold importance names do not match the original predictors")
      }
      values <- fold.info$importance$z
      values[!is.finite(values)] <- NA_real_
      z[idx, j] <- values
      for (nm in rules) {
        idx <- match(selected$object[[nm]]$variable, variables)
        if (anyNA(idx)) stop("selected variables do not match the original predictors")
        counts[unique(idx), nm] <- counts[unique(idx), nm] + 1L
      }
      scored <- .cv.varpro.with.seed(prediction.seed[j], {
        .cv.varpro.assess(selected, fold.info, train, test, args$formula,
                         args$crps, prediction.seed[j])
      })
      for (nm in rules) records[[nm]][[j]] <- cbind(fold = j, scored[[nm]])
    }, error = function(e) {
      stop("outer CV fold ", j, ": ", conditionMessage(e), call. = FALSE)
    })
  }
  nz <- rowSums(is.finite(z))
  zm <- rowMeans(z, na.rm = TRUE)
  zm[nz == 0L] <- NA_real_
  zs <- apply(z, 1L, sd, na.rm = TRUE)
  zs[nz < 2L] <- NA_real_
  frequency <- 100 * counts / k
  incomplete <- mixed <- character()
  for (nm in rules) {
    rec <- do.call(rbind, records[[nm]])
    rownames(rec) <- NULL
    metrics <- unique(rec$perf.type)
    same.metric <- length(metrics) == 1L
    complete <- all(is.finite(rec$err))
    if (!complete) incomplete <- c(incomplete, nm)
    if (!same.metric) mixed <- c(mixed, nm)
    idx <- match(full[[nm]]$variable, variables)
    if (anyNA(idx)) stop("full-data selection names do not match the original predictors")
    full[[nm]]$cv.select <- unname(frequency[idx, nm])
    full[[nm]]$cv.z.mean <- unname(zm[idx])
    full[[nm]]$cv.z.sd <- unname(zs[idx])
    full[[nm]]$cv.z.n <- unname(nz[idx])
    attr(full[[nm]], "cv") <- list(
      err = if (complete && same.metric) mean(rec$err) else NA_real_,
      sd = if (complete && same.metric) sd(rec$err) else NA_real_,
      perf.type = if (same.metric) metrics else "mixed",
      folds = rec, selection.frequency = frequency[, nm],
      fallback = unname(completed$fallback[nm]))
  }
  if (length(incomplete)) {
    warning("undefined outer-fold error for ", paste(incomplete, collapse = ", "),
            "; aggregate error and deviation are NA (see table 'cv' attributes)",
            call. = FALSE)
  }
  if (length(mixed)) {
    warning(paste("outer folds requested different performance measures;",
                  "aggregate error and deviation are NA.",
                  "Inspect fold records and use consistent RFQ settings to compare folds"),
            call. = FALSE)
  }
  full.foldid <- rep(NA_integer_, n.input)
  full.foldid[info$rows] <- partition$foldid
  attr(full, "xvar.map") <- info$xvar.map
  attr(full, "cv") <- list(folds = k, foldid = full.foldid,
                            analysis.rows = info$rows, fold.seed = fold.seed,
                            selection.seed = selection.seed,
                            prediction.seed = prediction.seed)
  full
}
