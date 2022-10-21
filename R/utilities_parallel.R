myDoRegister <- function(cores, parallel) {
  if (parallel && requireNamespace("doMC", quietly = TRUE)) {
    doMC::registerDoMC(cores = cores)
    list(parallel = TRUE, nfolds = min(cores, 10))
  } else {
    list(parallel = FALSE, nfolds = 10)
  }
}
myUnRegister <- function(parallel) {
  if (parallel && requireNamespace("foreach", quietly = TRUE)) {
    foreach::registerDoSEQ()
  } 
  NULL
}
