myDoRegister <- function(cores, parallel) {
  if (parallel && requireNamespace("doMC", quietly = TRUE)) {
    doMC::registerDoMC(cores = cores)
    TRUE
  } else {
    FALSE
  }
}

myUnRegister <- function(parallel) {
  if (parallel && requireNamespace("foreach", quietly = TRUE)) {
    foreach::registerDoSEQ()
  } 
  NULL
}
