plot.clusterpro <- function(x, xvar.names=NULL, shrink=TRUE, col=TRUE,
                            col.names=NULL, sort=TRUE, cex=FALSE, breaks=10, ...) {
  o <- x
  importance <- o$importance
  x <- o$x
  xnms <- names(importance)##ordering of x features in the data
  o.pt <- 1:length(x)
  if (sort) {
    o.pt <- order(importance, decreasing=TRUE)
    x <- x[o.pt]
  }
  if (is.null(xvar.names)) {
    whichx <- 1:length(x)
  }
  else {
    if (is.character(xvar.names)) {
      whichx <- match(xvar.names, names(x))
    }
    else {
      whichx <- xvar.names[xvar.names>0 & xvar.names<length(x)]
    }
  }
  whichx.col <- NULL
  if (!is.null(col.names)) {
    if (is.character(col.names)) {
      whichx.col <- match(col.names[1], xnms)
    }
    else {
      whichx.col <- o.pt[col.names[1]]
    }
  }
    lO <- lapply(whichx, function(j) {
    ## pull the data: proceed if not NULL
    xj <- x[[j]]
    if (!is.null(xj)) {
      if (is.null(whichx.col)) {
        xvj <- xj[, names(x)[j]]
      }
      else {
        xvj <- xj[, whichx.col]
      }
      ## custom breaks
      breaks <- unique(c(min(xvj, na.rm=TRUE) - 1, quantile(xvj, c(1:breaks)/breaks)))
      xvdj <- as.numeric(factor(cut(xvj, breaks=breaks, labels=FALSE)))
      ##shrink the release variable to zero when applying umap
      if (shrink) {
        xj[, names(x)[j]] <- 0
      }
      ##umap analysis: (TBD incorportate UMAP settings into hidden options)
      custom.settings <- umap::umap.defaults
      custom.settings$n_neighbors <- min(custom.settings$n_neighbors, round(nrow(xj)/2))
      um <- tryCatch({suppressWarnings(umap(xj, config=custom.settings))}, error=function(ex){NULL})
      ##plot-it
      if (!is.null(um)) {
        mycol <- "black"
        if (col) {
          mycol <- hcl.colors(length(unique(xvdj)), "YlOrRd", rev = TRUE)[xvdj]
        }
        if (cex) {
          plot(um$layout, xlab="umap 1", ylab="umap 2", col=mycol, pch=16, cex=log(xvdj))
        }
        else {
          plot(um$layout, xlab="umap 1", ylab="umap 2", col=mycol, pch=16)
        }
        title <- names(x)[j]
        mtext(title, side=3, line=1)
      }
    }
  })
  #invisible()
}
