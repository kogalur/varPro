## legacy - however function is well written so keep it for now
beta.varpro <- function(o,
                        use.cv = FALSE,
                        use.1se = TRUE,
                        nfolds = 10,
                        maxit = 2500,
                        thresh = 1e-3,
                        max.rules.tree,
                        max.tree) {
  ## ------------------------------------------------------------
  ## prelim
  ## ------------------------------------------------------------
  if (missing(max.rules.tree)) max.rules.tree <- o$max.rules.tree
  if (missing(max.tree))       max.tree       <- o$max.tree
  ## ------------------------------------------------------------
  ## call varpro strength to obtain membership lists
  ## ------------------------------------------------------------
  oo <- get.varpro.strength(o, membership = TRUE,
                           max.rules.tree = max.rules.tree,
                           max.tree       = max.tree)
  keep.rules <- which(oo$strengthArray$oobCT > 0 & oo$strengthArray$compCT > 0)
  if (!length(keep.rules)) return(NULL)
  oobMembership  <- oo$oobMembership
  compMembership <- oo$compMembership
  ## ------------------------------------------------------------
  ## released variable ids (restrict to keep.rules to shrink X)
  ## ------------------------------------------------------------
  xreleaseId     <- as.integer(oo$strengthArray$xReleaseID)
  xreleaseIdUnq  <- sort(unique(xreleaseId[keep.rules]))
  ## X: only the unique released variables (same as your intent)
  xvar.names <- o$xvar.names[xreleaseIdUnq]
  x_raw      <- o$x[, xvar.names, drop = FALSE]
  x_mat      <- data.matrix(x_raw)          # convert once
  storage.mode(x_mat) <- "double"
  ## map: rule row -> column position in x_mat
  rel_col <- match(xreleaseId, xreleaseIdUnq)  # length = nrow(strengthArray)
  ## Y
  if (o$family == "regr+") {
    y_all <- data.matrix(o$y)
    storage.mode(y_all) <- "double"
  } else {
    y_all <- o$y
    if (o$family == "regr") y_all <- as.numeric(y_all)
  }
  ## ------------------------------------------------------------
  ## set up results container
  ## ------------------------------------------------------------
  pt1 <- 1:3
  pt2 <- which(grepl("oobCT", colnames(oo$strengthArray)))
  if (o$family != "regr+") {
    pt3 <- which(grepl("importance", colnames(oo$strengthArray)))
  } else {
    pt3 <- which(grepl("imp", colnames(oo$strengthArray)))
  }
  results <- oo$strengthArray[, c(pt1, pt2, pt3), drop = FALSE]
  cnames  <- gsub("oobCT", "n.oob", colnames(oo$strengthArray)[pt2])
  if (o$family != "regr+") {
    inames <- gsub("importance", "imp", colnames(oo$strengthArray)[pt3])
  } else {
    inames <- paste0("imp.", seq_len(ncol(y_all)))
  }
  colnames(results) <- c("tree", "branch", "variable", cnames, inames)
  results[, inames] <- NA_real_
  ## ------------------------------------------------------------
  ## output allocation
  ## ------------------------------------------------------------
  nr <- length(keep.rules)
  if (o$family == "class") {
    nclass <- length(levels(y_all))
    beta_out <- matrix(NA_real_, nrow = nr, ncol = nclass + 1L)
  } else if (o$family == "regr+") {
    beta_out <- matrix(NA_real_, nrow = nr, ncol = ncol(y_all))
  } else {
    beta_out <- rep(NA_real_, nr)
  }
  ## ------------------------------------------------------------
  ## helper: extract region coefficient magnitude
  ## (region is always first predictor column in X, so:
  ##  - glmnet$beta row 1
  ##  - coef(cv.glmnet) row 2 (row 1 is intercept)
  ## ------------------------------------------------------------
  get_region_beta <- function(fit, cv = FALSE, family) {
    if (isTRUE(cv)) {
      lam <- get.lambda(fit, use.1se)
      if (family %in% c("regr", "binomial")) {
        cc <- tryCatch(stats::coef(fit, s = lam), error = function(e) NULL)
        if (is.null(cc)) return(NA_real_)
        return(abs(as.numeric(cc[2, 1])))
      } else if (family %in% c("multinomial", "mgaussian")) {
        cc <- tryCatch(stats::coef(fit, s = lam), error = function(e) NULL)
        if (is.null(cc) || !is.list(cc)) return(NULL)
        b_class <- sapply(cc, function(mat) {
          ## mat includes intercept row then predictors; region is row 2
          abs(as.numeric(mat[2, 1]))
        })
        return(b_class)
      }
    } else {
      ## glmnet (no CV): beta excludes intercept; region is row 1
      if (family %in% c("regr", "binomial")) {
        bb <- fit$beta
        if (is.null(bb)) return(NA_real_)
        return(abs(as.numeric(bb[1, ncol(bb)])))
      } else if (family %in% c("multinomial", "mgaussian")) {
        bb <- fit$beta
        if (is.null(bb) || !is.list(bb)) return(NULL)
        b_class <- sapply(bb, function(mat) {
          abs(as.numeric(mat[1, ncol(mat)]))
        })
        return(b_class)
      }
    }
    NA_real_
  }
  ## ------------------------------------------------------------
  ## MAIN LOOP (fast sequential)
  ## ------------------------------------------------------------
  for (k in seq_len(nr)) {
    i <- keep.rules[k]
    idxC <- compMembership[[i]]
    idxO <- oobMembership[[i]]
    if (!length(idxC) || !length(idxO)) next
    idx  <- c(idxC, idxO)
    nC   <- length(idxC)
    nO   <- length(idxO)
    ## build X: region indicator + all released variables EXCEPT the released one
    j <- rel_col[i]
    if (!is.finite(j)) next
    ## subset rows once, then drop released column by integer index
    Xsub <- x_mat[idx, , drop = FALSE]
    if (ncol(Xsub) > 0L) {
      Xsub <- Xsub[, -j, drop = FALSE]
    }
    region <- c(rep.int(0, nC), rep.int(1, nO))
    Xrule  <- cbind(region = region, Xsub)
    storage.mode(Xrule) <- "double"
    ## subset y once
    if (o$family == "regr+") {
      yrule <- y_all[idx, , drop = FALSE]
    } else {
      yrule <- y_all[idx]
    }
    ## ----------------------------------------------------------
    ## fit per family
    ## ----------------------------------------------------------
    if (o$family == "regr") {
      if (use.cv) {
        fit <- tryCatch(
          suppressWarnings(glmnet::cv.glmnet(Xrule, yrule,
                                             nfolds = nfolds,
                                             maxit  = maxit,
                                             thresh = thresh,
                                             parallel = FALSE)),
          error = function(e) NULL
        )
        beta_out[k] <- if (is.null(fit)) NA_real_ else get_region_beta(fit, cv = TRUE, family = "regr")
      } else {
        fit <- tryCatch(
          suppressWarnings(glmnet::glmnet(Xrule, yrule,
                                          maxit  = maxit,
                                          thresh = thresh)),
          error = function(e) NULL
        )
        beta_out[k] <- if (is.null(fit)) NA_real_ else get_region_beta(fit, cv = FALSE, family = "regr")
      }
    } else if (o$family == "class") {
      nclass <- length(levels(y_all))
      if (nclass == 2L) {
        if (use.cv) {
          fit <- tryCatch(
            suppressWarnings(glmnet::cv.glmnet(Xrule, yrule, family = "binomial",
                                               nfolds = nfolds,
                                               maxit  = maxit,
                                               thresh = thresh,
                                               parallel = FALSE)),
            error = function(e) NULL
          )
          b <- if (is.null(fit)) NA_real_ else get_region_beta(fit, cv = TRUE, family = "binomial")
          beta_out[k, ] <- rep(b, nclass + 1L)
        } else {
          fit <- tryCatch(
            suppressWarnings(glmnet::glmnet(Xrule, yrule, family = "binomial",
                                            maxit  = maxit,
                                            thresh = thresh)),
            error = function(e) NULL
          )
          b <- if (is.null(fit)) NA_real_ else get_region_beta(fit, cv = FALSE, family = "binomial")
          beta_out[k, ] <- rep(b, nclass + 1L)
        }
      } else {
        if (use.cv) {
          fit <- tryCatch(
            suppressWarnings(glmnet::cv.glmnet(Xrule, yrule, family = "multinomial",
                                               nfolds = nfolds,
                                               maxit  = maxit,
                                               thresh = thresh,
                                               parallel = FALSE)),
            error = function(e) NULL
          )
          b_class <- if (is.null(fit)) rep(NA_real_, nclass) else get_region_beta(fit, cv = TRUE, family = "multinomial")
          if (is.null(b_class)) b_class <- rep(NA_real_, nclass)
          beta_out[k, ] <- c(sum(b_class, na.rm = TRUE), b_class)
        } else {
          fit <- tryCatch(
            suppressWarnings(glmnet::glmnet(Xrule, yrule, family = "multinomial",
                                            maxit  = maxit,
                                            thresh = thresh)),
            error = function(e) NULL
          )
          b_class <- if (is.null(fit)) rep(NA_real_, nclass) else get_region_beta(fit, cv = FALSE, family = "multinomial")
          if (is.null(b_class)) b_class <- rep(NA_real_, nclass)
          beta_out[k, ] <- c(sum(b_class, na.rm = TRUE), b_class)
        }
      }
    } else if (o$family == "regr+") {
      mresp <- ncol(y_all)
      if (use.cv) {
        fit <- tryCatch(
          suppressWarnings(glmnet::cv.glmnet(Xrule, yrule, family = "mgaussian",
                                             nfolds = nfolds,
                                             maxit  = maxit,
                                             thresh = thresh,
                                             parallel = FALSE)),
          error = function(e) NULL
        )
        b_resp <- if (is.null(fit)) rep(NA_real_, mresp) else get_region_beta(fit, cv = TRUE, family = "mgaussian")
        if (is.null(b_resp)) b_resp <- rep(NA_real_, mresp)
        beta_out[k, ] <- b_resp
      } else {
        fit <- tryCatch(
          suppressWarnings(glmnet::glmnet(Xrule, yrule, family = "mgaussian",
                                          maxit  = maxit,
                                          thresh = thresh)),
          error = function(e) NULL
        )
        b_resp <- if (is.null(fit)) rep(NA_real_, mresp) else get_region_beta(fit, cv = FALSE, family = "mgaussian")
        if (is.null(b_resp)) b_resp <- rep(NA_real_, mresp)
        beta_out[k, ] <- b_resp
      }
    } else {
      stop("family specified not currently supported: ", o$family)
    }
  }
  ## ------------------------------------------------------------
  ## overlay the beta values
  ## ------------------------------------------------------------
  results[keep.rules, inames] <- beta_out
  ## package up as varpro object
  rO <- o
  rO$results <- results
  rO$max.rules.tree <- max.rules.tree
  rO$max.tree <- max.tree
  rO
}
get.lambda <- function(o, use.1se = TRUE) {
  if (use.1se) o$lambda.1se else o$lambda.min
}
