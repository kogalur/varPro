## set nodesize
set.nodesize <- function(n, p, nodesize = NULL) {

  if (is.null(nodesize)) {
    
    if (n <= 300 & p > n) {
      nodesize <- 2
    }
    else if (n <= 300 & p <= n) {
      nodesize <- 5
    }
    else if (n > 300 & n <= 2000) {
      nodesize <- 10
    }
    else {
      nodesize <- n / 200
    }

  }

  nodesize

}

## set nodedepth for dimension reducing forest
set.nodedepth.reduce <- function(n, p, nodedepth = NULL) {

  if (is.null(nodedepth)) {

    if (n <= 200) {
      nodedepth <- 2
    }
    else {
      nodedepth <- 3
    }
    
  }

  nodedepth
  
}


## set cv.nodesize
set.cv.nodesize <- function(n, p, nodesize = NULL) {

  if (is.null(nodesize)) {

    if (n <= 300 & p > n) {
      nodesize <- 2
    }
    else if (n <= 300 & p <= n) {
      nodesize <- 10
    }
    else if (n > 300 & n <= 2000) {
      nodesize <- 20
    }
    else {
      nodesize <- n / 100
    }
      
  }

  nodesize

}

## set use.vimp 
set.use.vimp <- function(n, p, use.vimp = NULL) {

  if (is.null(use.vimp)) {
    
    if (n <= 250) {
      use.vimp <- TRUE
    }
    else if (n > 250 & n <= 1000) {
      use.vimp <- p < 10000
    }
    else if (n > 1000 & n <= 2000) {
      use.vimp <- p < 5000
    }
    else if (n > 2000 & n <= 5000) {
      use.vimp <- p < 1000
    }
    else if (n > 5000 & n <= 15000) {
      use.vimp <- p < 500
    }
    else {
      use.vimp <- FALSE
    }

  }

  use.vimp

}
  
## set fast filtering cutoffs
set.xvar.cut <- function(xvar.used, n, dimension.n = 15000, dimension.q = .90) {

  if (n >= dimension.n) {
    quantile(xvar.used, prob = dimension.q, na.rm = TRUE)
  }
  else {
    1
  }

}

  

## tolerance function for keeping weights from becomming too small
## important to also preserve the original order of weights
tolerance <- function(xvar.wt, split.weight.tolerance) {
  pt <- xvar.wt <= split.weight.tolerance
  if (any(pt)) {
    rank.wt <- rank(xvar.wt[pt], ties.method = "random")
    xvar.wt[pt] <- split.weight.tolerance * rank.wt / max(rank.wt, na.rm = TRUE)
  }
  xvar.wt
}


## process hidden variables
get.varpro.hidden <- function(user.option, ntree) {


  if (!is.null(user.option$sampsize)) {
    sampsize <- user.option$sampsize
  }
  else {
    sampsize <- function(x) {x * .632}
  }


  if (!is.null(user.option$nsplit)) {
    nsplit <- function(x) {
      eval(user.option$nsplit)
    }
  }
  else {
    nsplit <- function(x) {
      if (x > 15000) {
        50
      }
      else {
        0
      }
    }
  }

  if (!is.null(user.option$ntree.external)) {
    ntree.external <- user.option$ntree.external
  }
  else {
    ntree.external <- ntree
  }


  if (!is.null(user.option$ntime.external)) {
    ntime.external <- user.option$ntime.external
  }
  else {
    ntime.external <- 100
  }


  if (!is.null(user.option$ntree.reduce)) {
    ntree.reduce <- user.option$ntree.reduce
  }
  else {
    ntree.reduce <- 100
  }

  if (!is.null(user.option$dimension.index)) {
    dimension.index <- function(x) {
      eval(user.option$dimension.index)
    }
  }
  else {
    dimension.index <- function(x) {
      if (x >= 100) {
        20
      }
      else if (50 <= x & x < 100) {
        10
      }
      else if (20 <= x & x < 50) {
        5
      }
      else {
        2
      }
    }
  }

  if (!is.null(user.option$use.rfq)) {
    use.rfq <- user.option$use.rfq
  }
  else {
    use.rfq <- TRUE
  }

  if (!is.null(user.option$iratio.threshold)) {
    iratio.threshold <- user.option$iratio.threshold
  }
  else {
    iratio.threshold <- 2
  }

  if (!is.null(user.option$rmst)) {
    rmst <- user.option$rmst
  }
  else {
    rmst <- NULL
  }

  if (!is.null(user.option$use.coxnet)) {
    use.coxnet <- user.option$use.coxnet
  }
  else {
    use.coxnet <- FALSE
  }

  if (!is.null(user.option$maxit)) {
    maxit <- user.option$maxit
  }
  else {
    maxit <- 7500
  }

  if (!is.null(user.option$split.weight.only)) {
    split.weight.only <- user.option$split.weight.only
  }
  else {
    split.weight.only <- FALSE
  }

  if (!is.null(user.option$split.weight.tolerance)) {
    split.weight.tolerance <- user.option$split.weight.tolerance
  }
  else {
    split.weight.tolerance <- 1e-50
  }

  
  if (!is.null(user.option$use.lasso)) {
    use.lasso <- user.option$use.lasso
  }
  else {
    use.lasso <- TRUE
  }

  if (!is.null(user.option$nfolds)) {
    nfolds <- user.option$nfolds
  }
  else {
    nfolds <- 10
  }



  list(sampsize = sampsize,
       nsplit = nsplit,
       ntree.external = ntree.external,
       ntime.external = ntime.external,
       ntree.reduce = ntree.reduce,
       dimension.index = dimension.index,
       use.rfq = use.rfq,
       iratio.threshold = iratio.threshold,
       rmst = rmst,
       use.coxnet = use.coxnet,
       maxit = maxit,
       split.weight.only = split.weight.only,
       split.weight.tolerance = split.weight.tolerance,
       use.lasso = use.lasso,
       nfolds = nfolds)

}



## list hidden variables
show.varpro.hidden <- function() {

  c("sampsize",
    "nsplit",
    "ntree.external",
    "ntime.external",
    "ntree.reduce",
    "dimension.index",
    "use.rfq = use.rfq",
    "iratio.threshold",
    "rmst",
    "use.coxnet",
    "maxit",
    "split.weight.only",
    "split.weight.tolerance",
    "use.lasso",
    "nfolds")

}


## extract varpro formal names and hidden options
get.varpro.names <- function (hidden = TRUE) {

  vnames <- names(formals(varpro))

  if (hidden) {
    
    vnames <- c(vnames,
                "sampsize", "nsplit",
                "ntree.external", "ntime.external",
                "ntree.reduce", 
                "dimension.index",
                "use.rfq",
                "iratio.threshold",
                "rmst",
                "use.coxnet",
                "maxit",
                "split.weight.only",
                "split.weight.tolerance",
                "use.lasso",
                "nfolds")
                
  }

  vnames

}


