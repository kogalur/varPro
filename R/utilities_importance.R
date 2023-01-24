get.orgvimp <- function(o1, o2) {
  ## be flexible and allow various input values
  if (is.data.frame(o1) || is.matrix(o1)) {
    o1 <- colnames(o1)
  }
  ## second input value must be a varpro object
  if (!inherits(o2, "varpro", TRUE)) {
    stop("o2 must be a varpro object")
  }
  ## extract the vimp and names
  vmp <- importance(o2)
  if (o2$family == "regr+") {
      vmp <- do.call(rbind, vmp)
  }
  if (o2$family == "class") {
    vmp <- vmp$unconditional
  }
  o2 <- rownames(vmp)
  ## match original variable names to varpro names which uses hot encode data
  vars <- o1[which(unlist(lapply(o1, function(nn) {
  if (any(grepl(nn, o2))) {
    TRUE
  }
  else {
    FALSE
  }
  })))]
  ## obtain z for mapped variables
  vars.z <- lapply(o1, function(nn) {
    if (any((pt <- grepl(nn, o2)))) {
      if (!all(is.na(vmp[pt, 3]))) {
        max(vmp[pt, 3], na.rm = TRUE)
      }
      else {
        NA
      }
    }
    else {
      NULL
    }
  })
  vars.z <- unlist(vars.z[!sapply(vars.z, is.null)])
  topvars <- data.frame(variable = vars, z = vars.z)
  topvars[order(topvars$z, decreasing = TRUE),, drop = FALSE]
}
##  winsorized statistics
winsorize <- function (x, trim = 0.1, na.rm = TRUE) {
  if ((trim < 0) | (trim > 0.5)) 
    stop("trimming must be reasonable")
  qtrim <- quantile(x, c(trim, 0.5, 1 - trim), na.rm = na.rm)
  xbot <- qtrim[1]
  xtop <- qtrim[3]
  if (trim < 0.5) {
    x[x < xbot] <- xbot
    x[x > xtop] <- xtop
  }
  else {
    x[!is.na(x)] <- qtrim[2]
  }
  return(x)
}
winsorize.sd <- function (x, trim = 0.1, na.rm = TRUE) {
  if ((trim < 0) | (trim >= 0.5)) {
    stop("trimming must be reasonable")
  }
  sqrt(var(winsorize(x, trim = trim, na.rm = na.rm), na.rm = na.rm))
}
winsorize.mean <- function (x, trim = 0.1, na.rm = TRUE) {
  if ((trim < 0) | (trim > 0.5)) 
    stop("trimming must be reasonable")
  if (trim < 0.5) {
    return(mean(winsorize(x, trim = trim, na.rm = na.rm), na.rm = na.rm))
  }
  else {
    return(median(x, na.rm = na.rm))
  }
}
