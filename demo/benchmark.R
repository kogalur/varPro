##################################################################
### <May 05 2022>Hemant Ishwaran
### --------------------------------------------------------------- 
### benchmarking varPro
### ---------------------------------------------------------------
###  Written by:
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

##################################################################
### 
### 
###  source libraries/files
###
###
####################################################################

source("comparison_methods.R") 
source("vimp_performance.R")
source("generate_function_simulation.R")

#################################################################
###
### parameter settings
###
###################################################################

## filename for storing results (only applies if save.on.the.fly = FALSE)
fnmOut <- paste0(getwd(), "/vimpBenchmarkOutput/")
if (!file.exists(path = fnmOut)) {
  dir.create(path = fnmOut, showWarnings = TRUE)
}


## comparison methods
method <-  c("lasso",
             "knockoff.glmnet", 
             "wvimp",
             "gbm",
             "forest",
             "varPro")

## save data?
save.on.the.fly <- TRUE

## data settings
n <- c(200, 2000)[2]
ntruth <- c(200, 10000)[2]
d <- c(15, 40, 100, 500)[2]
corrv <- c(0, .9)
nrep <- c(1, 10, 20, 50, 100)[1]
simnames <- names(simulation.sum)

## varPro settings
cutoff <- 2
ntree <- 500
max.rules.tree <- 100
max.tree <- 150
papply <- mclapply
verbose <- !FALSE
split.weight <- FALSE

#################################################################
###
###
### acquire truth: which variables are signal?
###
###
###################################################################

truth <- do.call(rbind, lapply(simnames, function(nms) {

  ## verbose output
  cat("determining signal variables for simulation:", nms, "\n")

  ## draw the simulation
  simo <- simulation.sum[[nms]](n=n, d=d, corrv = 0)

  ## acquire the signal variables
  xnms <- colnames(simo$dta)[colnames(simo$dta) != "y"]
  if (length(xnms) != d) {
    stop("simulation dimension does not meet target dimension\n")
  }
  signal <- rep(0, d)
  names(signal) <- xnms
  signal[all.vars(as.formula(paste("~", simo$f)))] <- 1

  ## return the signal vector
  signal

}))
rownames(truth) <- simnames


#################################################################
###
###
### benchmark benchmark benchmark
### - outer loop is correlation
### - inner loops are data sets and replicates
###
###
###################################################################

## storage for performance lists
perO <- vector("list", length(corrv))
betaO <- vector("list", length(corrv))


cat("\n\n\n +-----> simulations and analysis will now begin ...\n")
rO <- lapply(1:length(corrv), function(j) {

  ## useful counter
  ctr <- 0
  
  ## set the correlation parameter
  cr <- corrv[j]
  
  ## verbose output
  cat("-------------------------------------------------------------------\n")
  cat("correlation:", cr, "\n")

  oo <- lapply(1:length(simnames), function(k) {

    ## set the file name
    nms <- simnames[k]

    ## update the counter
    ctr <<- ctr + 1

    ## verbose output
    cat("simulation #:", ctr, "experiment:", nms, "\n")

    ## temporary storage
    perTmp <- betaTmp <- NULL
    
    ## loop over replicates/average
    ooo <- lapply(1:nrep, function(l) {

      ## verbose output
      if (nrep > 1) cat("-------- iteration", l, "\n")

      ## ----------------------------------------------
      ##
      ##
      ## simulation
      ##
      ##
      ## ----------------------------------------------
      
      simo <- simulation.sum[[nms]](n=n, d=d, corrv = cr)

      
      ## ----------------------------------------------
      ##
      ##
      ## call the various methods
      ##
      ##
      ## ----------------------------------------------
      
      o <- comparison.methods(simo$dta[, -which(colnames(simo$dta) == "y")], simo$dta$y, 
                              split.weight=split.weight, 
                              ntree=ntree,
                              max.rules.tree=max.rules.tree, 
                              max.tree=max.tree,
                              cutoff=cutoff, 
                              verbose=verbose,
                              method=method)
      
      ## ----------------------------------------------
      ##
      ##
      ## obtain performance values
      ##
      ##
      ## ----------------------------------------------

      methodname <- rownames(o)
      o[is.na(o)] <- 0

      per <- lapply(1:nrow(o), function(ii){vimp.performance(o[ii,], simo)})
      pero <- lapply(1:length(per), function(ii){rbind(perTmp[[ii]], per[[ii]])})
      names(pero) <- methodname
      perTmp <<- pero

      beta <- lapply(1:nrow(o), function(ii){o[ii,]})
      betao <- lapply(1:length(beta), function(ii){rbind(betaTmp[[ii]], beta[[ii]])})
      names(betao) <- methodname
      betaTmp <<- betao
      
    })
    
    ## ----------------------------------------------
    ##
    ## store the results
    ##
    ## ----------------------------------------------

    perO[[j]][length(perO[[j]])+1] <<- list(perTmp)
    betaO[[j]][length(betaO[[j]])+1] <<- list(betaTmp)

    ## ----------------------------------------------
    ##
    ##
    ## save on the fly?
    ## saves the simulation result to disk as .rda file
    ##
    ##
    ## ----------------------------------------------
    
    if (save.on.the.fly) {
      
      truth <- truth[nms, ]
      perf <- perTmp
      beta <- betaTmp
      fnm <- fnmOut
      fnm <- paste0(fnm, nms, "_corr_", cr, ".rda")
      save(truth, perf, beta, file = fnm)

    }

    NULL

  })

  ## ----------------------------------------------
  ##
  ## pretty up and finalize the return
  ##
  ## ----------------------------------------------
  
  names(perO[[j]]) <<- simnames
  names(betaO[[j]]) <<- simnames
  
  NULL

})


#################################################################
###
###
### save the values
###
###
###################################################################

save(truth, perO, betaO, file = "benchmark.rda")

