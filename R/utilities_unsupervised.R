##  make SH data (modes 1 and 2)
make.sh <- function(dat, mode = 1, papply = mclapply) {
  ## extract sample size dimension
  nr <- dim(dat)[[1]]
  nc <- dim(dat)[[2]]
  if (nc == 0) {
    stop("can't make SH data ... not enough unique values\n")
  }
  ## coerce to data frame format
  if (!is.data.frame(dat)) {
    dat <- data.frame(dat)
  }
  ## mode 1
  if (mode == 1) {
    data.frame(classes = factor(c(rep(1, nr), rep(2, nr))),
      rbind(dat, data.frame(papply(dat, sample, replace = TRUE))))
  }
  ## mode 2
  else {
    data.frame(classes = factor(c(rep(1, nr), rep(2, nr))),
      rbind(dat, data.frame(papply(dat, function(x) {
        if (is.factor(x)) {
          resample(x, replace = TRUE)
        }
        else {
          runif(nr, min(x, na.rm = TRUE), max(x, na.rm = TRUE))
        }
      }))))
  }
}
