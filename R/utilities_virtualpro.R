###################################################################
### <March 13 2025>Hemant Ishwaran
### ---------------------------------------------------------------
### prototype for active DALR using virtual twins
### ---------------------------------------------------------------
###  Written by:
###
###  Hemant Ishwaran                     hemant.ishwaran@gmail.com
###  Division of Biostatistics           
###  Clinical Research Building
###  1120 NW 14th Street
###  University of Miami, Miami FL 33136
###
###  https:
####################################################################
vt.release <- function(object,
                       newdata,
                       max.rules.tree = 150,
                       max.tree = 150,
                       papply = mclapply,
                       reduce = TRUE, cutoff = .79) {
  ## allow only random forests and varpro objects
  if (!inherits(object, "varpro")) {
    if (sum(inherits(object, c("rfsrc", "grow"), TRUE) == c(1, 2)) != 2) {
      stop("This function only works for objects of class 'varpro' or `(rfsrc, grow)'")
    }
    else {
      o <- object
      xvar.names <- o$xvar.names
      if (missing(newdata)) {##use the original data-restore
        newdata  <- object$xvar
      }
    }
  }
  else {
    o <- object$rf
    xvar.names <- o$xvar.names
    if (missing(newdata)) {##use the original data-restore
      newdata <- object$x
    }
    else {
        ##      newdata <- varPro:::get.hotencode.test(object$x, newdata)
        newdata <- get.hotencode.test(object$x, newdata)
    }
  }
  ## call varpro.strength with test data option
  vt <- varpro.strength(object = o,
                        newdata = newdata,
                        max.rules.tree = max.rules.tree,
                        max.tree = max.tree)
  ## extract key items
  sarr <- vt$strengthArray
  tree <- vt$strengthTreeID
  memb <- vt$testCaseTermID
  xvar <- sarr$xReleaseID
  rule <- match(paste(sarr$treeID, sarr$nodeID), unique(paste(sarr$treeID, sarr$nodeID)))
  ## loop over trees, acquiring release cases --> sorted by order of test data
  rO <- papply(1:ncol(memb), function(b) {
    pt <- which(sarr$treeID == tree[b] & is.element(sarr$nodeID, memb[,b]))
    comp <- vt$compMembership[pt]
    xvar <- xvar[pt]
    rule <- rule[pt]
    sp <- split(1:length(pt), sarr$nodeID[pt])
    ##0/0 oobCT compCT
    ##mmatch <- match(memb[,b], sort(unique(memb[,b])))
    mmatch <- match(memb[,b], sort(unique(sarr$nodeID[pt])))
    list(
      ##comp membership, in order of test data
      comp = lapply(mmatch, function(ix) {
        unlist(comp[sp[[ix]]])
      }),
      ## xrelease variable, in order of test data, replicated to match comp membership
      xvar = lapply(mmatch, function(ix) {
        rep(xvar[sp[[ix]]], sapply(comp[sp[[ix]]], length))
      }),
      ## rule identifier, in order of test data, replicated to match comp membership
      rule = lapply(mmatch, function(ix) {
        rep(rule[sp[[ix]]], sapply(comp[sp[[ix]]], length))
      })
    )
  })
  ## dimension reduction?
  if (reduce && inherits(object, "varpro")) {
    v <- get.orgvimp(object)
    reduce <- v$variable[v$z >= cutoff]
  }
  else {
    reduce <- xvar.names
  }
  ## assemble in order of the test data
  comp <- lapply(rO, function(oo) {oo$comp})
  xvar <- lapply(rO, function(oo) {oo$xvar})
  rule <- lapply(rO, function(oo) {oo$rule})
  list(
    comp = lapply(1:nrow(newdata), function(i) {
      unlist(sapply(comp, "[", i))
    }),
    xvar = lapply(1:nrow(newdata), function(i) {
      unlist(sapply(xvar, "[", i))
    }),
    rule = lapply(1:nrow(newdata), function(i) {
      unlist(sapply(rule, "[", i))
    }),
    x = newdata,
    xvar.names = xvar.names,
    xvar.selected = reduce
  )
}
###################################################################
### 
### 
### 
###  utilities
###
###
###
####################################################################
vt.distance <- function(vt, metric, neighbor = 5, papply = lapply, wt = TRUE, reduce = TRUE) {
  ## assign the metric
  if (missing(metric)) {
    metric <- gini
  }
  ## dimension reduction?
  if (reduce) {
    whichx <- which(is.element(vt$xvar.names,vt$xvar.selected))
  }
  else {
    whichx <- 1:length(vt$xvar.names)
  }
  ## matching score
  score <- papply(seq_along(vt$xvar), function(idx) {
    pt <- which(vt$xvar[[idx]] %in% whichx)
    t1 <- vt$xvar[[idx]][pt]
    t2 <- vt$comp[[idx]][pt]
    tb <- table(t1, t2)
    stat <- apply(tb, 2, metric)
    w <- colSums(tb)
    if (wt) {
      stat * w
    }
    else {
      w
    }
  })
  ## neighbors
  neighbor <- lapply(seq_along(vt$xvar), function(idx) {
    pt <- order(score[[idx]], decreasing = TRUE)[1:neighbor]
    score <- score[[idx]][pt]
    data.frame(index = pt, score = score, id = as.numeric(names(score)))
  })
  ## return the goodies
  list(score=score, neighbor=neighbor)
}
###################################################################
### 
### 
### 
###  metrics
###
###
###
####################################################################
gini <- function(x) {
  p <- x / sum(x)
  mean(p * (1 - p))
}
entropy <- function(x) {
  p <- x / sum(x)
  - mean(p * log(p * (p>0) + 1 * (p==0)))
}
chisq <- function(x, pvalue = TRUE) {
  chisq_test <- tryCatch({suppressWarnings(chisq.test(x, p = rep(1/length(x), length(x))))},
                         error = function(ex){NULL})
  if (!is.null(chisq_test)) {
    if (pvalue) {
      chisq_test$p.value
    }
    else {
      - chisq_test$statistic / chisq_test$parameter
    }
  }
  else {
    NA
  }
}
chisq.pvalue <- function(x) {chisq(x, pvalue=TRUE)}
chisq.stat <- function(x) {chisq(x, pvalue=FALSE)}
