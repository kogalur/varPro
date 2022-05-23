##################################################################
### <May 03 2022>Hemant Ishwaran
### --------------------------------------------------------------- 
### benchmarking varPro 
### ---------------------------------------------------------------
###  Written by Min Lu and Hemant Ishwaran
###
###  Hemant Ishwaran                     hemant.ishwaran@gmail.com
###  Division of Biostatistics           
###  Clinical Research Building
###  1120 NW 14th Street
###  University of Miami, Miami FL 33136
###
###  https://ishwaran.org
###  -------------------------------------------------------------
###  THIS PROGRAM SHOULD NOT BE COPIED, USED, MODIFIED, OR 
###  DISSEMINATED IN ANY WAY WITHOUT SPECIFIC WRITTEN PERMISSION 
###  FROM THE AUTHOR.
####################################################################

### install derandomKnock 
if("derandomKnock" %in% rownames(installed.packages()) == FALSE) {
if (!require("devtools")){
  install.packages("devtools")
}
devtools::install_github("zhimeir/derandomKnock")
}
if("derandomKnock" %in% rownames(installed.packages()) == FALSE) {
  install.packages("ranger")  ### derandomKnock.forest wants ranger
}


library(varPro)
library(randomForestSRC)

library(derandomKnock)
library(gbm)
library(glmnet)
library(knockoff)
library(doMC) ## knockoff wants this for faster computation
library(parallel)
library(RSpectra)
library(vimp) ##  Williamson et al vimp 
library("SuperLearner")  ## Williamson et al vimp
library("xgboost") ## Williamson et al vimp
library("kko")   ## kernel knockoff

## extr: for categorical predictors, we want one coefficient, instead of M-1 coefficients 
## for M categories from dummy variables; the coefficient kept is the maximum absolute value from
## these M-1 coefficients
extr <- function(x, xname){

  ## if x is a vector length 0 return NA
  if (length(x) == 0){
    temp <- rep(NA, length(xname))
    names(temp) <- xname
    return(temp)
  }
    
  temp <- matrix(NA, nrow = 1, ncol = length(xname),
                 dimnames = list("beta", xname))
  Xname <- names(x)
 # j <- 1
  for (j in 1:length(xname)){
  for (i in 1:length(x)){
    if (Xname[i] == xname[j]){
      temp[1, j] <- x[i]
    #  j <- j + 1
    } else if (grepl(xname[j], Xname[i])){ 
      temp[1, j] <- max(abs(temp[1, j]), abs(x[i]), na.rm = TRUE) } 
  }
  }
  temp
}

## customized message when verbose=TRUE
cat.method <- function(method, finished = FALSE) {
  cat(paste0("running method ", method, "\n"))
  if (finished) cat("------------done----------------")
}


##-------------------------------------------------
##
## variable selection with lasso, VIMP, VarPro, 
##
##-------------------------------------------------
comparison.methods <- function(x, y,
                              split.weight = TRUE, 
                              ntree = 500,
                              max.rules.tree = 500, 
                              max.tree = 100,
                              cutoff = 2, 
                              alpha = .1, 
                              verbose = TRUE,
                              Vvimp = 5,
                              method = c("lasso",
                                         "knockoff.glmnet", 
                                         "knockoff.forest", 
                                         "derandomknock.glmnet",
                                         "derandomknock.forest",
                                         "derandomknock.glmnet2",
                                         "derandomknock.forest2",
                                         "kernelknock",
                                         "wvimp",
                                         "gbm",
                                         "forest",
                                         "varPro"),
                              Sigma = NULL,
                              maxit = 5000)
{

  ## set up storage
  xname <- colnames(x)
  nmethod <- length(method)
  methodname <- method
  result <- matrix(NA, nmethod, ncol(x), dimnames = list(methodname, xname))

  ## make the design matrix
  design <- makeX(as.data.frame(x), na.impute = FALSE, sparse = FALSE)
  mu <- colMeans(design, na.rm = TRUE)
  if (is.null(Sigma)) Sigma <- round(var(design, na.rm = TRUE),3) ## round() helps Sigma positive defined

  for (j in 1:nmethod){

    ###### lasso
    if (method[j] == "lasso") {
      if (verbose) cat.method(method[j])
      obj <- cv.glmnet(design, y, maxit = maxit)
      beta <- coef(obj)[-1,] # discard intercept
      beta <- extr(beta, xname) 
      beta[beta == 0] <- NA
      result[j, match(colnames(beta), xname)] <- abs(beta)
    }

    ###### knockoffs --- glmnet
    if (method[j] == "knockoff.glmnet") {
      if (verbose) cat.method(method[j])
      tryCatch({
        obj <- knockoff.filter(design, y,
                               knockoffs = function(x) {create.fixed(x, y = y)},
                               statistic = stat.glmnet_coefdiff,
                               fdr = 0.1,
                               offset = 0) ## offset = 1 tend to filter less variables
        beta <- obj$statistic[obj$selected]
        names(beta) <- colnames(design)[obj$selected]
        beta <- extr(beta, xname) 
        result[j, match(colnames(beta), xname)] <- abs(beta)
        
      }, error=function(e){print(paste("knockoff.glmnet failed"))})
    }
    
    ###### knockoffs ---- random forest
    if (method[j] == "knockoffs.forest") {
      if (verbose) cat.method(method[j])
      tryCatch({
        obj <- knockoff.filter(design, y,
                               knockoffs = function(x) {create.fixed(x, y = y)},
                               statistic = stat.random_forest,
                               fdr = 0.1,
                               offset = 0) ## offset = 1 tend to filter less variables
        beta <- obj$statistic[obj$selected]
        names(beta) <- colnames(design)[obj$selected]
        beta <- extr(beta, xname) 
        result[j, match(colnames(beta), xname)] <- abs(beta)
        
      }, error=function(e){print(paste("knockoff.randomForest failed"))})
    }
    
    ###### derandomKnock ---- requires sigma
    if (method[j] == "derandomknock.glmnet") {
      if (verbose) cat.method(method[j])
      tryCatch({
        
        obj <- derandomKnock(design,y,k=1, alpha = alpha, 
                             knockoff_method = "gaussian",
                           knockoff_stat = stat.glmnet_coefdiff,
                           mu = mu, Sigma = Sigma)
        
        beta <- obj$frequency
        names(beta) <- colnames(design)[obj$S]
        beta <- extr(beta, xname) 
        result[j, match(colnames(beta), xname)] <- beta
        
      }, error=function(e){print(paste("derandomknock.glmnet failed"))})
    }
    
    ###### derandomKnock ---- requires sigma
    if (method[j] == "derandomknock.forest") {
      if (verbose) cat.method(method[j])
      tryCatch({
        obj <- derandomKnock(design,y,k=1, alpha = alpha, 
                             knockoff_method = "gaussian",
                             knockoff_stat = stat.random_forest,
                             mu = mu, Sigma = Sigma)
        
        beta <- obj$frequency
        names(beta) <- colnames(design)[obj$S]
        beta <- extr(beta, xname) 
        result[j, match(colnames(beta), xname)] <- beta
        
      }, error=function(e){print(paste("derandomknock.forest failed"))})
    }
    
    ###### derandomKnock ---- requires sigma
    if (method[j] == "derandomknock.glmnet2") {
      if (verbose) cat.method(method[j])
      tryCatch({
        obj <- base_filter(design,y,v0=1, knockoff_method = "gaussian",
                           knockoff_stat = stat.glmnet_coefdiff,
                           mu = mu, Sigma = Sigma)
        
        beta <- obj$W
        names(beta) <- colnames(design)
        beta <- extr(beta, xname) 
        result[j, match(colnames(beta), xname)] <- beta
        
      }, error=function(e){print(paste("derandomknock.glmnet2 failed"))})
    }
    
    ###### derandomKnock ---- requires sigma
    if (method[j] == "derandomknock.forest2") {
      if (verbose) cat.method(method[j])
      tryCatch({
        obj <- base_filter(design,y,v0=1, knockoff_method = "gaussian",
                           knockoff_stat = stat.random_forest,
                           mu = mu, Sigma = Sigma)
        
        beta <- obj$W
        names(beta) <- colnames(design)
        beta <- extr(beta, xname) 
        result[j, match(colnames(beta), xname)] <- beta
        
      }, error=function(e){print(paste("derandomknock.forest2 failed"))})
    }

    ###### kernelKnock 
    if (method[j] == "kernelknock") {
      tryCatch({
        
        rkernel="laplacian" # kernel choice
        rk_scale=1 # scaling paramtere of kernel
        
        cv_folds=10 # folds of cross-validation in group lasso
        n_stb=50 # number of subsampling for importance scores
        n_stb_tune=100 # number of subsampling for tuning random feature number
        frac_stb=1/2 # fraction of subsample
        nCores_para= 4 # number of cores for parallelization
        
        X_k = create.second_order(design) # generate knockoff
        
        rfn_range = 2:ncol(X_k)
        tune.o <- rk_tune(design,y,X_k,rfn_range,n_stb,cv_folds,frac_stb,nCores_para,rkernel,rk_scale)
        obj <- kko(design,y,X_k,tune.o$rfn_range,n_stb_tune,n_stb,cv_folds,frac_stb,nCores_para,rkernel,rk_scale)
        
        beta <- obj$importance_score 
        names(beta) <- colnames(design)
        
        beta[which(rank(beta)<=(ncol(X_k)-obj$rfn_tune))] <- 0
        
        beta <- extr(beta, xname) 
        result[j, match(colnames(beta), xname)] <- beta
        
        
        
      }, error=function(e){print(paste("kernelknock failed"))})
    }

    ###### gbm
    if (method[j] == "gbm") {
      if (verbose) cat.method(method[j])
      tryCatch({
        obj <- gbm(y~., data.frame(x, y = y), 
                   distribution = "gaussian", n.trees = 1500, shrinkage = 0.1,             
                   interaction.depth = 2, bag.fraction = 0.5, train.fraction = 1,  
                   n.minobsinnode = 10, cv.folds = 10, keep.data = TRUE, 
                   verbose = FALSE, n.cores=detectCores())
        best.iter <- gbm.perf(obj, method = "cv", plot.it = FALSE)
        beta <- relative.influence(obj, n.trees = best.iter)
        result[j, match(names(beta), xname)] <- abs(beta)  
      }, error=function(e){print(paste("gbm failed"))})
    }
    
    ###### breiman cutler vimp
    if (method[j] == "forest") {
      if (verbose) cat.method(method[j])
      obj <- rfsrc(y~., data.frame(x, y = y), importance = "permute")
      vmp <- obj$importance
      result[j, match(names(vmp), xname)] <- vmp
    }
    
    ###### wvimp (williams VIMP)
    if (method[j] == "wvimp") {

      if (verbose) cat.method(method[j])
      learners <- c("SL.glm", "SL.glmnet", "SL.xgboost", "SL.mean")[1]

      # -----------------------------------------
      # using Super Learner (with a small number of folds, for illustration only)
      ## very slow - need to use mclapply
      ## use SL.glm otherwise the code is too slow
      # -----------------------------------------
      result.tmp <- unlist(mclapply(1:ncol(x), function(k) {

      # ------------------------------------------
      # doing things by hand, and plugging them in
      # (with a small number of folds, for illustration only)
      # ------------------------------------------
      # set up the folds
      indx <- k
      V <- Vvimp
      Y <- matrix(y)
      #set.seed(4747)
      # Note that the CV.SuperLearner should be run with an outer layer
        # of 2*V folds (for V-fold cross-fitted importance)
      full_cv_fit <- suppressWarnings(SuperLearner::CV.SuperLearner(
        Y = Y, X = x, SL.library = learners, 
        cvControl = list(V = 2 * V),
        innerCvControl = list(list(V = V))
      ))
        # use the same cross-fitting folds for reduced
      reduced_cv_fit <- suppressWarnings(SuperLearner::CV.SuperLearner(
        Y = Y, X = x[, -indx, drop = FALSE], SL.library = learners,
        cvControl = SuperLearner::SuperLearner.CV.control(
          V = 2 * V, validRows = full_cv_fit$folds
        ),
        innerCvControl = list(list(V = V))
        ))
      # extract the predictions on split portions of the data,
      # for hypothesis testing
      cross_fitting_folds <- get_cv_sl_folds(full_cv_fit$folds)
      #set.seed(1234)
      sample_splitting_folds <- make_folds(unique(cross_fitting_folds), V = 2)
      full_cv_preds <- extract_sampled_split_predictions(
        full_cv_fit, sample_splitting_folds = sample_splitting_folds, full = TRUE
      )
      reduced_cv_preds <- extract_sampled_split_predictions(
        reduced_cv_fit, sample_splitting_folds = sample_splitting_folds, full = FALSE
      )
      #set.seed(5678)
      est <- cv_vim(Y = y, cross_fitted_f1 = full_cv_preds,
                    cross_fitted_f2 = reduced_cv_preds, indx = 2, delta = 0, V = V, type = "r_squared",
                    cross_fitting_folds = cross_fitting_folds,
                    sample_splitting_folds = sample_splitting_folds,
                    run_regression = FALSE, alpha = alpha, na.rm = TRUE)
      
      if (est$test) {
         1 - est$p_value ## larger value indicate signals
      }
       else {
        NA
      }
      }))
      result[j, ] <- result.tmp
    }
    
    ###### VarPro
    if (grepl("varPro", method[j])) {
      if (verbose) cat.method(method[j])
      obj <- varpro(y~., data.frame(x, y = y), 
                    split.weight = split.weight, 
                    ntree = ntree,
                    max.rules.tree = max.rules.tree, 
                    max.tree = max.tree,
                    verbose = verbose)

      imp <- importance.varpro(obj, cutoff = cutoff)
      imp[is.na(imp)] <- 0
      if (sum(imp$selected == 1, na.rm = TRUE) > 0) {
        imp <- imp[imp$selected == 1,, drop = FALSE]
      }
      else {
        if (sum(imp$mean > 0, na.rm = TRUE) > 0) {
          imp <- imp[imp$mean > 0,, drop = FALSE]
        }
      }
      result[j, match(rownames(imp), xname)] <- imp[, "z"]

    }

  }##completes loop over methods
  
  ###### return the goodies
  result 

}
