###################################################################
##
##
## performance metrics
##
##
####################################################################
get.iso.performance <- function(y, p) {
  ## only works for binary y
  if (length(unique(y)) != 2) {
    stop("y can only take 2 distinct values\n")
  }
  ## convert y to 0/1 factor
  y <- factor(y, labels = c(0, 1))
  ## metrics
  auc <- get.auc(y, cbind(p, 1-p))
  pr.auc <- get.pr.auc(y, 1-p)[1]
  ## return the goodies
  per <- c(auc, pr.auc)
  names(per) <- c("auc", "pr.auc")
  per
}
