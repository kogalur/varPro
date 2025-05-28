isopro <- function(object,
                   method = c("unsupv", "rnd", "auto"),
                   sampsize = function(x){min(2^6, .632 * x)},
                   ntree = 500, nodesize = 1,
                   formula = NULL, data = NULL, ...) {
  ## ------------------------------------------------------------------------
  ##
  ## coherence checks: determine if this is a varpro object, or formula/data
  ##
  ## ------------------------------------------------------------------------
  ## must be a varpro object
  if (is.null(formula) && is.null(data) && !inherits(object, "varpro")) {
    stop("object must be a varpro object")
  }
  ## convert data to a data fram
  if (!is.null(data) && !is.data.frame(data)) {
    data <- data.frame(data)
  }
  ## if this is a varpro object use this to filter the data
  no.formula.data.flag <- FALSE
  if (is.null(formula) && is.null(data)) {
    no.formula.data.flag <- TRUE
    topvars <- get.topvars(object)
    data <- object$x[, topvars]
  }
  ## set method
  method <- match.arg(method, c("unsupv", "rnd", "auto"))
  ## coherence check for supervised analysis
  if (!is.null(formula) && !is.null(data) && missing(object)) {
    method <- "supv"
  }
  if (method == "supv" && no.formula.data.flag && missing(object)) {
    stop("supervised method requires formula/data to be provided or a varpro object has to be provided")
  }
  ## obtain family and other details for supervised problems
  if (method == "supv") {
    formula <- as.formula(formula)
    o.stump <- get.stump(formula, data)
    family <- o.stump$family
    yvar.names <- o.stump$yvar.names
  }
  ## ------------------------------------------------------------------------
  ##
  ## special treament for imbalanced classification case
  ##
  ## ------------------------------------------------------------------------
  imbalanced.flag <- FALSE
  if (method == "supv" && get.varpro.hidden(NULL, NULL)$use.rfq) {
    if (family == "class" && length(levels(data[, yvar.names])) == 2) {
      y.frq <- table(data[, yvar.names])
      class.labels <- names(y.frq)
      iratio <- max(y.frq, na.rm = TRUE) / min(y.frq, na.rm = TRUE)
      imbalanced.flag <- iratio > get.varpro.hidden(NULL, NULL)$iratio.threshold
    }
  }
  ##--------------------------------------------------------------
  ##
  ## extract additional options specified by user
  ## we lock this down to allowed types
  ##
  ##--------------------------------------------------------------
  ## list of (non-hidden) forest parameters
  rfnames <- names(formals(rfsrc))
  ## restrict to allowed values
  rfnames <- rfnames[rfnames != "formula" &
                     rfnames != "data" &
                     rfnames != "sampsize" &
                     rfnames != "ntree" &
                     rfnames != "nodesize" &
                     rfnames != "perf.type"]
  ## get the permissible hidden options
  dots <- list(...)
  dots <- dots[names(dots) %in% rfnames]
  ## ------------------------------------------------------------------------
  ##
  ## unsupervised iso forests
  ##
  ## ------------------------------------------------------------------------ 
  if (method == "unsupv") {
    if (is.null(dots$mtry)) {
      dots$ytry <- min(ceiling(sqrt(ncol(data))), ncol(data) - 1)
      dots$mtry <- Inf
    }
    o.iso <- do.call("rfsrc", c(list(data = data,
                   sampsize = sampsize,
                   ntree = ntree,
                   nodesize = nodesize,
                   perf.type = "none"), dots))
  }
  ## ------------------------------------------------------------------------
  ##
  ## random split iso
  ##
  ## ------------------------------------------------------------------------ 
  if (method == "rnd") {
    dots$splitrule <- NULL
    o.iso <- do.call("rfsrc", c(list(formula = yxyz123~.,
                   data = data.frame(yxyz123 = rnorm(nrow(data)), data),
                   splitrule = "random",
                   sampsize = sampsize,
                   ntree = ntree,
                   nodesize = nodesize,
                   perf.type = "none"), dots))
  }
  ## ------------------------------------------------------------------------
  ##
  ## multivariate (auto-encoder) iso
  ##
  ## ------------------------------------------------------------------------ 
  if (method == "auto") {
    o.iso <- do.call("rfsrc", c(list(formula = get.mv.formula(paste0("y.", colnames(data))),
                   data = data.frame(y=data, data),
                   sampsize = sampsize,
                   ntree = ntree,
                   nodesize = nodesize,
                   perf.type = "none"), dots))
  }
  ## ------------------------------------------------------------------------
  ##
  ##   ## supervised  iso
  ##
  ## ------------------------------------------------------------------------ 
  if (method == "supv") {
    ## check if this is imbalanced using default threshold setting
    ## by default brf is used, unless the user over-rides this using "use.brf"
    if (imbalanced.flag) {
      ## gini unweighting is exceptionally slow for imbalanced data - turn this off
      if (is.null(dots$splitrule)) {
      #  dots$splitrule <- "gini.unwt"
      }
      if (is.null(dots$brf) || dots$brf == TRUE) {
        dots$brf <- dots$sampsize <- NULL
        o.iso <- do.call("imbalanced", c(list(formula = formula, data = data,
                           method = "brf",
                           ntree = ntree,
                           nodesize = nodesize,
                           perf.type = "none"), dots))
      }
      else {
        dots$sampsize <- sampsize
        dots$brf <- NULL
        o.iso <- do.call("imbalanced", c(list(formula = formula, data = data,
                           ntree = ntree,
                           nodesize = nodesize,
                           perf.type = "none"), dots))
      }
    }
    ## default setting: for now we turn off unweighted splitting - more analysis required
    else {
      if (is.null(dots$splitrule) && family == "regr") {
        #dots$splitrule <- "mse.unwt"
      }
      if (is.null(dots$splitrule) && family == "class") {
        #dots$splitrule <- "gini.unwt"
      }
      o.iso <- do.call("rfsrc", c(list(formula = formula, data = data,
                       sampsize = sampsize,
                       ntree = ntree,
                       nodesize = nodesize,
                       perf.type = "none"), dots))
    }
  }
  ## ------------------------------------------------------------------------
  ##
  ##
  ## case depth values
  ##
  ##
  ## ------------------------------------------------------------------------
  case.depth <- colMeans(predict.rfsrc(o.iso, data, case.depth = TRUE)$case.depth, na.rm = TRUE)
  cdf <- ecdf(case.depth)
  howbad <- cdf(case.depth)
  ## ------------------------------------------------------------------------
  ##
  ##
  ## return the goodies
  ##
  ##
  ## ------------------------------------------------------------------------
  rO <- list(case.depth = case.depth,
             howbad = howbad,
             cdf = cdf,
             isoforest = o.iso)
  class(rO) <- "isopro"
  rO
}
