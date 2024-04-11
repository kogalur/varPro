beta.varpro <- function(o, use.cv=FALSE, use.1se=TRUE, nfolds=10, maxit=2500, thresh=1e-3,
                        max.rules.tree, max.tree, papply=mclapply) {
  ##------------------------------------------------------------------
  ##
  ## prelim
  ##
  ##------------------------------------------------------------------
  if (missing(max.rules.tree)) {
    max.rules.tree <- o$max.rules.tree
  }
  if (missing(max.tree)) {
    max.tree <- o$max.tree
  }
  ##------------------------------------------------------------------
  ##
  ## call varpro strength to obtain membership lists
  ##
  ##------------------------------------------------------------------
  oo <- get.varpro.strength(o, membership = TRUE, max.rules.tree = max.rules.tree, max.tree = max.tree)
  ## identify non-empty rules
  keep.rules <- which(oo$strengthArray$oobCT > 0 & oo$strengthArray$compCT > 0)
  if (length(keep.rules) == 0) {
    return(NULL)
  }
  ##------------------------------------------------------------------
  ##
  ## process the strength array and other prelim calculations
  ##
  ##------------------------------------------------------------------
  ## membership lists 
  oobMembership <- oo$oobMembership
  compMembership <- oo$compMembership
  ## keep track of which variable is released for a rule
  xreleaseId <- oo$strengthArray$xReleaseID
  xreleaseIdUnq <- sort(unique(xreleaseId))
  ## pull x and y
  xvar.names <- o$xvar.names[xreleaseIdUnq]
  x <- o$x[, xvar.names, drop=FALSE]
  y <- o$y
  ## storage for the new importance values - family dependent
  pt1 <- 1:3
  pt2 <- which(grepl("oobCT", colnames(oo$strengthArray)))
  if (o$family != "regr+") {
    pt3 <- which(grepl("importance", colnames(oo$strengthArray)))
  }
  else {
    pt3 <- which(grepl("imp", colnames(oo$strengthArray)))
  }
  results <- oo$strengthArray[, c(pt1, pt2, pt3), drop = FALSE]
  cnames <- gsub("oobCT", "n.oob", colnames(oo$strengthArray)[pt2])
  if (o$family != "regr+") {
    inames <- gsub("importance", "imp", colnames(oo$strengthArray)[pt3])
  }
  else {
    inames <- paste0("imp.", 1:ncol(y))
  }
  colnames(results) <- c("tree", "branch", "variable", cnames, inames)
  results[, inames] <- NA
  ##------------------------------------------------------------------
  ##
  ## MAIN LOOP - parse the membership values to obtain lasso beta for each variable
  ##
  ##------------------------------------------------------------------
  beta <- papply(keep.rules, function(i) {
    ## build the x data
    xC <- x[compMembership[[i]],, drop=FALSE]
    xO <- x[oobMembership[[i]],, drop=FALSE]
    x <- rbind(xC, xO)
    nC <- nrow(xC)
    nO <- nrow(xO)
    x <- cbind(c(rep(0, nC), rep(1, nO)), x[, colnames(x)!=xreleaseId[i], drop=FALSE])
    if (o$family != "regr+") {
      y <- y[c(compMembership[[i]], oobMembership[[i]])]
    }
    else {
      y <- y[c(compMembership[[i]], oobMembership[[i]]),, drop=FALSE]
    }
    ##------------------------------------------------------------------
    ##
    ## regression/survival
    ##
    ##------------------------------------------------------------------
    if (o$family == "regr") {
      if (use.cv) {## standard cv-lasso
        o.glmnet <- tryCatch(
                  {suppressWarnings(cv.glmnet(as.matrix(x), y,
                         nfolds=nfolds, maxit=maxit, thresh=thresh, parallel=FALSE))},
        error=function(ex){NULL})
        ## pull the beta "region" parameter (if lasso did not fail)
        if (!is.null(o.glmnet)) {
          b <- abs(coef(o.glmnet, s=get.lambda(o.glmnet, use.1se))[2])
        }
        else {
          b <- NA
        }
      }
      else {## modified lasso without cv
        o.glmnet <- tryCatch(
                  {suppressWarnings(glmnet(as.matrix(x), y, 
                   nfolds=nfolds, maxit=maxit, thresh=thresh, parallel=FALSE))}, error=function(ex){NULL})
        if (!is.null(o.glmnet)) {
          b <- as.matrix(o.glmnet$beta)
          b <- as.numeric(abs(b[, ncol(b)])[1])
        }
        else {
          b <- NA
        }
      }
    }
    ##------------------------------------------------------------------
    ##
    ## classification/multiclassification
    ##
    ##------------------------------------------------------------------
    else if (o$family == "class") {
      ## number of class labels
      nclass <- length(levels(y))
      ## two class
      if (nclass == 2) {
        if (use.cv) {## standard cv-lasso
          o.glmnet <- tryCatch(
               {suppressWarnings(cv.glmnet(scale(as.matrix(x)), y, family = "binomial",
                    nfolds=nfolds, maxit=maxit, thresh=thresh, parallel=FALSE))}, error=function(ex){NULL})
          if (!is.null(o.glmnet)) {
            b <- abs(coef(o.glmnet, s=get.lambda(o.glmnet, use.1se))[2,])
            b <- rep(b, nclass+1)
          }
          else {
            b <- rep(NA, nclass+1)
          }
        }
        else {## modified lasso without cv
          o.glmnet <- tryCatch(
             {suppressWarnings(glmnet(as.matrix(x), y, family = "binomial",
                   nfolds=nfolds, maxit=maxit, thresh=thresh, parallel=FALSE))}, error=function(ex){NULL})
          if (!is.null(o.glmnet)) {
            b <- as.matrix(o.glmnet$beta)
            b <- as.numeric(abs(b[, ncol(b)])[1])
            b <- rep(b, nclass+1)
          }
          else {
            b <- rep(NA, nclass+1)
          }
        }
      }
      ## multiclass 
      else {
        if (use.cv) {## standard cv-lasso
          o.glmnet <- tryCatch(
               {suppressWarnings(cv.glmnet(scale(as.matrix(x)), y, family = "multinomial",
                    nfolds=nfolds, maxit=maxit, thresh=thresh, parallel=FALSE))}, error=function(ex){NULL})
          if (!is.null(o.glmnet)) {
            b <- abs(do.call(cbind, lapply(coef(o.glmnet, s=get.lambda(o.glmnet, use.1se)), function(o) {o[-1,]}))[1,])
            b <- c(sum(b, na.rm=TRUE), b)
          }
          else {
            b <- rep(NA, nclass+1)
          }
        }
        else {## modified lasso without cv
          o.glmnet <- tryCatch(
             {suppressWarnings(glmnet(as.matrix(x), y, family = "multinomial",
                       nfolds=nfolds, maxit=maxit, thresh=thresh, parallel=FALSE))}, error=function(ex){NULL})
          if (!is.null(o.glmnet)) {
            b <- sapply(o.glmnet$beta, function(beta) {
              bb <- as.matrix(beta)
              as.numeric(abs(bb[, ncol(bb)])[1])
            })
            b <- c(sum(b, na.rm=TRUE), b)
          }
          else {
            b <- rep(NA, nclass+1)
          }
        }
      }
    }
    ##------------------------------------------------------------------
    ##
    ## ## mv-regression
    ##
    ##------------------------------------------------------------------
    else if (o$family == "regr+") {
      if (use.cv) {## standard cv-lasso
        o.glmnet <- tryCatch(
               {suppressWarnings(cv.glmnet(scale(data.matrix(x)), y, family = "mgaussian",
                          nfolds=nfolds, maxit=maxit, thresh=thresh, parallel=FALSE))}, error=function(ex){NULL})
        if (!is.null(o.glmnet)) {
          b <- abs(do.call(cbind, lapply(coef(o.glmnet, s=get.lambda(o.glmnet, use.1se)), function(o) {o[-1,]}))[1,])
        }
        else {
          b <- rep(NA, ncol(y))
        }
      }
      else {## modified lasso without cv
        o.glmnet <- tryCatch(
               {suppressWarnings(glmnet(scale(data.matrix(x)), y, family = "mgaussian",
                            nfolds=nfolds, maxit=maxit, thresh=thresh, parallel=FALSE))}, error=function(ex){NULL})
        if (!is.null(o.glmnet)) {
          b <- sapply(o.glmnet$beta, function(beta) {
            bb <- as.matrix(beta)
            as.numeric(abs(bb[, ncol(bb)])[1])
          })
        }
        else {
          b <- rep(NA, ncol(y))
        }
      }
    }
    ##------------------------------------------------------------------
    ##
    ## error for unsuported family
    ##
    ##------------------------------------------------------------------
    else {
      stop("family specified not currently supported: ", o$family)
    }
    ### return the beta
    b
  })
  ##------------------------------------------------------------------
  ##
  ##
  ## overlay the beta importance values depending on family
  ##
  ##
  ##------------------------------------------------------------------
  if (o$family == "class" | o$family == "regr+") {
    beta <- do.call(rbind, beta)
  }
  else {
    beta <- unlist(beta)
  }
  results[keep.rules, inames] <- beta
  ##------------------------------------------------------------------
  ##
  ##
  ## package results up as a varpro object
  ##
  ##
  ##------------------------------------------------------------------
  rO <- o
  rO$results <- results
  rO$max.rules.tree <- max.rules.tree
  rO$max.tree <- max.tree
  ##------------------------------------------------------------------
  ##
  ##
  ## return the goodies
  ##
  ##
  ##------------------------------------------------------------------
  #importance(rO)
  rO
}
get.lambda <- function(o, use.1se=TRUE) {if (use.1se) o$lambda.1se else o$lambda.min}
