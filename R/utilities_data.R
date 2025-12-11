## stumpy tree
get.stump <- function(f, data) {
  rfsrc(f, data, mtry=1, splitrule="random", nodedepth=0,
                      perf.type="none", save.memory=TRUE, ntree=1)
}

## robust sample
resample <- function(x, ...) x[sample.int(length(x), ...)]

## rough imputation - avoids using non-exported get.na.roughfix 
roughfix <- function(data) {
  imean <- lapply(data, function(x) {
    if (all(is.na(x))) {
      NA
    } else if (is.factor(x)) {
      tab <- table(x)
      names(tab)[which.max(tab)]
    } else {
      mean(x, na.rm = TRUE)
    }
  })
  names(imean) <- colnames(data)

  out <- data.frame(lapply(colnames(data), function(nm) {
    x <- data[[nm]]
    isna <- is.na(x)
    if (any(isna)) x[isna] <- imean[[nm]]
    x
  }), stringsAsFactors = TRUE)
  colnames(out) <- colnames(data)
  out
}

