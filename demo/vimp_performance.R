library("PRROC")


###################################################################
###
###
###
###
### performance of estimator used with simulations
###
###
###
###
###################################################################

vimp.performance <- function(vmp, simo) {

  
  ##------------------------------------------------
  ##
  ## extract truth
  ##
  ##------------------------------------------------

  ## acquire the signal variables
  xnms <- colnames(simo$dta)[colnames(simo$dta) != "y"]
  p <- length(xnms)
  signal <- rep(0, p)
  names(signal) <- xnms
  signal[all.vars(as.formula(paste("~", simo$f)))] <- 1

  ##------------------------------------------------
  ##
  ## extract vimp
  ##
  ##------------------------------------------------

  vmp.tmp <- rep(0, p)
  names(vmp.tmp) <- xnms
  vmp.tmp[names(vmp)] <- vmp
  vmp <- vmp.tmp


  
  ##------------------------------------------------
  ##
  ## define the auc data
  ## truth=0 (vimp = 0) truth not 0 (vimp non-zero)
  ## "diagnostic test" (AUC terminology) large means truth is 1 
  ##
  ##------------------------------------------------

  auc.dta <- data.frame(truth = 1 * (signal != 0), yhat = vmp)
                        

  ##------------------------------------------------
  ##
  ## obtain the different auc values
  ##
  ##------------------------------------------------

  auc <- auc.performance(auc.dta[, 1], auc.dta[, 2])
  auc.pr <- pr.auc.performance(auc.dta[, 1], auc.dta[, 2])


  ##------------------------------------------------
  ##
  ## get the different error rates
  ##
  ##------------------------------------------------

  ## precision = ppv = 1 - fdr
  ## fdr = fraction of selected that are incorrect
  ## fdr = FP/(Predicted Condition Positive)=FP/(FP+TP)
  ## positive estimates --> model picks 
  ## corrected by Gang 07/15/2019
  fdr <- NA
  if (sum(auc.dta[, 2] > 0) > 0) {
    fdr <- sum((auc.dta[, 2] > 0) & (auc.dta[, 1] == 0)) / sum(auc.dta[, 2] > 0)
  }

  ## sensitivity = recall = tpr
  ## tpr = rate of correctly selecting true  (conditions on truth)
  ## tpr = TP/(Condition Positive) = TP/(TP+FN)
  ##
  ## specificity = tnr
  ## tnr = rate of correctly selecting false (conditions on truth)
  ## tnr = TN/(Condition Negative) = TN/(TN+FP)
  ##
  ## positive estimates --> model picks 
  ## zero or negative estimates --> model does not pick
  tpr <- tnr <- NA
  if (sum(auc.dta[, 1] == 1) > 0) {
    tpr <- sum((auc.dta[, 2] > 0) & (auc.dta[, 1] == 1)) / sum(auc.dta[, 1] == 1)
  }
  if (sum(auc.dta[, 1] == 0) > 0) {
    tnr <- sum((auc.dta[, 2] <= 0) & (auc.dta[, 1] == 0)) / sum(auc.dta[, 1] == 0)
  }
  
  ## return the goodies
  perf <- data.frame(auc = auc,
             auc.pr = auc.pr[1],
             aur.pr.rand = auc.pr[2],
             precision = 1 - fdr,
             sensitivity = tpr,
             specificity = tnr,
             gmean = sqrt(tpr * tnr))

  perf
  
}


###################################################################
##
## auc function
##
###################################################################

## auc performance function
auc.performance <- function(truth, yhat) {

  x <- yhat[truth == 1]
  y <- yhat[truth == 0]

  if (length(x) > 0 && length(y) > 0) {

    ## auc calculation via exact wilcox 
    AUC <- tryCatch({
      wilcox.test(x, y, exact = F)$stat/(length(x) * length(y))
    }, error = function(ex) {
      NA
    })

  }
  
  else {
    
    AUC <- NA
  
  }

  as.numeric(AUC)

}


## pr auc performance function
pr.auc.performance <- function(truth, yhat) {
  
  x <- yhat[truth == 1]
  y <- yhat[truth == 0]

  rO <- c(NA, NA)
  
  if (length(x) > 0 && length(y) > 0) {

    ## precision recall with random baseline
    pr.o <- tryCatch({pr.curve(x, y, rand.compute = TRUE)}, error = function(ex) {NULL})
    
    if (!is.null(pr.o)) {
      
      rO <- c(pr.o$auc.integral, pr.o$rand$auc.integral)

    }
    
  }

  rO
  
}

