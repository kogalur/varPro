## process hidden variables
get.varpro.hidden <- function(user.option, ntree) {
  if (!is.null(user.option$nodesize.external)) {
    nodesize.external <- user.option$nodesize.external
  }
  else {
    nodesize.external <- NULL
  }
  if (!is.null(user.option$ntime.external)) {
    ntime.external <- user.option$ntime.external
  }
  else {
    ntime.external <- 100
  }
  if (!is.null(user.option$nodesize.reduce)) {
    nodesize.reduce <- user.option$nodesize.reduce
  }
  else {
    nodesize.reduce <- NULL
  }
  if (!is.null(user.option$ntree.reduce)) {
    ntree.reduce <- user.option$ntree.reduce
  }
  else {
    ntree.reduce <- 100
  }
  if (!is.null(user.option$nodedepth.reduce)) {
    nodedepth.reduce <- user.option$nodedepth.reduce
  }
  else {
    nodedepth.reduce <- 3
  }
  if (!is.null(user.option$dimension.n)) {
    dimension.n <- user.option$dimension.n
  }
  else {
    dimension.n <- 15000
  }
  if (!is.null(user.option$dimension.p)) {
    dimension.p <- user.option$dimension.p
  }
  else {
    dimension.p <- 300
  }
  if (!is.null(user.option$dimension.q)) {
    dimension.q <- user.option$dimension.q
  }
  else {
    dimension.q <- .90
  }
  if (!is.null(user.option$dimension.index)) {
    dimension.index <- user.option$dimension.index
  }
  else {
    dimension.index <- 2
  }
  if (!is.null(user.option$dimension.ir)) {
    dimension.ir <- user.option$dimension.ir
  }
  else {
    dimension.ir <- 2
  }
  if (!is.null(user.option$rmst)) {
    rmst <- user.option$rmst
  }
  else {
    rmst <- NULL
  }
  if (!is.null(user.option$other.external)) {
    other.external <- user.option$other.external
  }
  else {
    other.external <- FALSE
  }
  if (!is.null(user.option$maxit)) {
    maxit <- user.option$maxit
  }
  else {
    maxit <- 2500
  }
  if (!is.null(user.option$split.weight.only)) {
    split.weight.only <- user.option$split.weight.only
  }
  else {
    split.weight.only <- FALSE
  }
  if (!is.null(user.option$use.lasso)) {
    use.lasso <- user.option$use.lasso
  }
  else {
    use.lasso <- TRUE
  }
  if (!is.null(user.option$use.vimp)) {
    use.vimp <- user.option$use.vimp
  }
  else {
    use.vimp <- TRUE
  }
  list(ntree.external = ntree,
       nodesize.external = nodesize.external,
       ntime.external = ntime.external,
       nodesize.reduce = nodesize.reduce,
       ntree.reduce = ntree.reduce,
       nodedepth.reduce = nodedepth.reduce,
       dimension.n = dimension.n,
       dimension.p = dimension.p,
       dimension.q = dimension.q,
       dimension.index = dimension.index,
       dimension.ir = dimension.ir,
       rmst = rmst,
       other.external = other.external,
       maxit = maxit,
       split.weight.only = split.weight.only,
       use.lasso = use.lasso,
       use.vimp = use.vimp)
}
## list hidden variables
show.varpro.hidden <- function() {
  c("ntree.external",
    "nodesize.external",
    "ntime.external",
    "nodesize.reduce",
    "ntree.reduce",
    "nodedepth.reduce",
    "dimension.n",
    "dimension.p",
    "dimension.q",
    "dimension.index",
    "dimension.ir",
    "rmst",
    "other.external",
    "maxit",
    "split.weight.only",
    "use.lasso",
    "use.vimp")
}
## extract varpro formal names and hidden options
get.varpro.names <- function (hidden = TRUE) {
  vnames <- names(formals(varpro))
  if (hidden) {
    vnames <- c(vnames, "nodesize.external", "ntime.external",
                "nodesize.reduce", "ntree.reduce", "nodedepth.reduce",
                "dimension.n", "dimension.p", "dimension.q", "dimension.index",
                "dimension.ir", "rmst", "other.external", "maxit", "split.weight.only",
                "use.lasso", "use.vimp")
  }
  vnames
}
