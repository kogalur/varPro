##################################################################
### <May 05 2022>Hemant Ishwaran
### --------------------------------------------------------------- 
### Summarize results of benchmarking varPro
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

library(xtable)
source("vimp_performance.R")
source("generate_function_simulation.R")

#################################################################
###
### parameters
###
###################################################################

corrv <- c(0, .9)
n <- c(200, 2000)[2]
d <- c(15, 40, 100, 500)[2]

#################################################################
###
### storage lists
###
###################################################################

perO <- vector("list", length(corrv))
betaO <- vector("list", length(corrv))


#################################################################
###
### load files 
###
###################################################################

path <- "vimpBenchmarkOutput"

lapply(1:length(corrv), function(j) {

  cr <- corrv[j]

  pattern <- paste0("*_corr_", cr, ".rda")  
  files <- list.files(path=path, pattern=pattern, full.names=TRUE, recursive=FALSE)
  nms <- list.files(path=path, pattern=pattern, full.names=FALSE, recursive=FALSE)
  simnames <- unlist(strsplit(nms, pattern))
  
  lapply(1:length(files), function(k) {

    ## load the rda file
    load(files[k])

    ## store gradient estimation performance
    perO[[j]][length(perO[[j]])+1] <<- list(perf)
    betaO[[j]][length(betaO[[j]])+1] <<- list(beta)
    

    NULL
    
  })

  # ----------------------------------------------
  ##
  ## pretty up and finalize the return
  ##
  ## ----------------------------------------------

  names(perO[[j]]) <<- simnames
  names(betaO[[j]]) <<- simnames
  
  
})



#################################################################
###
### extract performance values from beta
###
###################################################################

perf.o <- mclapply(1:length(betaO[[1]]), function(j) {
  o <- betaO[[1]][[j]]
  simo <- simulation.sum[[names(betaO[[1]])[j]]](n=n, d=d, corrv = corrv[1])
  rO <- do.call(rbind, lapply(1:length(o), function(k) {
    o.k <- o[[k]]
    colMeans(do.call(rbind, lapply(1:nrow(o.k), function(ii){
      vimp.performance(o.k[ii,], simo)}
    )), na.rm = TRUE)
  }))
  rownames(rO) <- names(o)
  rO
})
names(perf.o) <- names(betaO[[1]])

perf.c <- mclapply(1:length(betaO[[2]]), function(j) {
  o <- betaO[[2]][[j]]
  simo <- simulation.sum[[names(betaO[[1]])[j]]](n=n, d=d, corrv = corrv[2])
  rO <- do.call(rbind, lapply(1:length(o), function(k) {
    o.k <- o[[k]]
    colMeans(do.call(rbind, lapply(1:nrow(o.k), function(ii){
      vimp.performance(o.k[ii,], simo)}
    )), na.rm = TRUE)
  }))
  rownames(rO) <- names(o)
  rO
})
names(perf.c) <- names(betaO[[2]])

#################################################################
###
### extract gmean 
###
###################################################################

gmean.o <- do.call(rbind, lapply(perf.o, function(o) {o[, "gmean"]}))
gmean.c <- do.call(rbind, lapply(perf.c, function(o) {o[, "gmean"]}))

cat("gmean - uncorrelated\n")
print(gmean.o)

cat("gmean - correlated\n")
print(gmean.c)

#################################################################
###
### pr-auc
###
###################################################################

auc.pr.o <- do.call(rbind, lapply(perf.o, function(o) {o[, "auc.pr"]}))
auc.pr.c <- do.call(rbind, lapply(perf.c, function(o) {o[, "auc.pr"]}))

cat("auc.pr - uncorrelated\n")
print(auc.pr.o)

cat("auc.pr - correlated\n")
print(auc.pr.c)

