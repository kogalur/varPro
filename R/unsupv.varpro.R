unsupv.varpro <- function(data,
                          method = c("auto", "unsupv", "rnd"),
                          ntree = 200, nodesize = NULL,
                          max.rules.tree = 50, max.tree = 200,
                          papply = mclapply, verbose = FALSE, seed = NULL,
                          ...)
{		   
  ##------------------------------------------------------------------
  ##
  ##
  ## pre-processing 
  ##
  ##
  ##------------------------------------------------------------------
  # set method
  method <- match.arg(method, c("auto", "unsupv", "rnd"))
  ##--------------------------------------------------------------
  ##
  ## define the entropy function (or obtain user specified one)
  ##
  ## can be a number or a list
  ## for numbers: this is the importance
  ## for lists:   (a) entry 1 = importance: (b) entry 2 -> returned 
  ## 
  ##
  ##--------------------------------------------------------------
  dots <- list(...)
  custom.entropy.flag <- FALSE
  ## default entropy returns a list
  ## first entry = total variance
  ## second entry = pc-simple results
  if (is.null(dots$entropy)) {
    entropy.function <- entropy.default
    entropy.importance.function <- entropy.default.importance
  }
  ## user specified entropy function
  else {
    custom.entropy.flag <- TRUE
    entropy.function <- dots$entropy
  }
  ##--------------------------------------------------------------
  ##
  ## extract additional options specified by user
  ## we lock this down to allowed types
  ## define the entropy function used for importance
  ##
  ##--------------------------------------------------------------
  ## parameters used with default entropy function(s)
  alpha <- switch(1+(is.null(dots$alpha)), dots$alpha, .025)
  beta <- switch(1+(is.null(dots$beta)), dots$beta, FALSE)
  nlegit <- switch(1+(is.null(dots$nlegit)), dots$nlegit, 25)
  dots.entropy <- list()
  dots.entropy$alpha <- alpha
  dots.entropy$beta <- beta
  dots.entropy$nlegit <- nlegit
  user.provided.varpro.flag <- FALSE
  ## special feature allowing user to pass in an arbitrary varpro object
  ## the purpose of this is to allow access to the entropy function framework
  if (!is.null(dots$object)) {
    user.provided.varpro.flag <- TRUE
    o <- dots$object
    ## over-ride the supplied data if this is a varpro object
    if (inherits(o, "varpro", TRUE)) {
      data <- o$x[, o$xvar.names, drop = FALSE]
    }
  }
  ## list of (non-hidden) forest parameters
  rfnames <- names(formals(rfsrc))
  ## restrict to allowed values
  rfnames <- rfnames[rfnames != "formula" &
                     rfnames != "data" &
                     rfnames != "ntree" &
                     rfnames != "nodesize" &
                     rfnames != "perf.type"]
  ## get the permissible hidden options for rfrsc
  dots <- dots[names(dots) %in% rfnames]
  ##-----------------------------------------------------------------
  ##
  ## process data
  ##
  ##
  ##------------------------------------------------------------------
  ## remove any column with less than two unique values
  void.var <- sapply(data, function(x){length(unique(x, na.rm = TRUE)) < 2})
  if (sum(void.var) > 0) {
    data[, which(void.var)] <- NULL
  }
  ## hot encode the data
  data <- get.hotencode(data, papply)
  ## assign the xvar names
  xvar.names <- colnames(data)
  ##------------------------------------------------------------------
  ##
  ##
  ## unsupervised forests
  ##
  ##
  ##------------------------------------------------------------------
  if (method == "unsupv" && !user.provided.varpro.flag) {
    if (is.null(dots$ytry)) {
      dots$ytry <- min(ceiling(sqrt(ncol(data))), ncol(data) - 1)
    }
    o <- do.call("rfsrc", c(list(
                   data = data,
                   ntree = ntree,
                   nodesize = set.unsupervised.nodesize(nrow(data), ncol(data), nodesize),
                   perf.type = "none"), dots))
  }
  ##------------------------------------------------------------------
  ##
  ##
  ## pure random forests
  ##
  ##
  ##------------------------------------------------------------------
  if (method == "rnd" && !user.provided.varpro.flag) {
    dots$splitrule <- NULL
    o <- do.call("rfsrc", c(list(formula = yxyz123~.,
                   data = data.frame(yxyz123 = rnorm(nrow(data)), data),
                   splitrule = "random",
                   ntree = ntree,
                   nodesize = set.unsupervised.nodesize(nrow(data), ncol(data) + 1, nodesize),
                   perf.type = "none"), dots))
  }
  ##------------------------------------------------------------------
  ##
  ##
  ## auto-encoder (regr+)
  ##
  ##
  ##------------------------------------------------------------------
  if (method == "auto" && !user.provided.varpro.flag) {
    ## call regr+
    o <- do.call("rfsrc", c(list(formula = get.mv.formula(xvar.names),
                   data = data.frame(y = data, data),
                   ntree = ntree,
                   nodesize = set.unsupervised.nodesize(nrow(data), ncol(data), nodesize),
                   perf.type = "none"), dots))
  }
  ##------------------------------------------------------------------
  ##
  ##
  ## call varpro.strength and extract necessary information
  ##
  ##
  ##------------------------------------------------------------------
  ## switch for varpro strength depends on whether o is a forest or not
  oo <- get.varpro.strength(o, membership = TRUE, max.rules.tree = max.rules.tree, max.tree = max.tree)
  ## identify useful rules and variables at play
  keep.rules <- which(oo$strengthArray$oobCT > 0 & oo$strengthArray$compCT > 0)
  ## membership lists 
  oobMembership <- oo$oobMembership
  compMembership <- oo$compMembership
  ## keep track of which variable is released for a rule
  xreleaseId <- oo$strengthArray$xReleaseID
  ## standardize x
  x <- scaleM(data, center = FALSE)
  ## used to store the new importance values
  results <- oo$strengthArray[, 1:5, drop = FALSE]
  colnames(results) <- c("tree", "branch", "variable", "n.oob", "imp")
  results$imp <- NA
  ##------------------------------------------------------------------
  ##
  ##
  ## obtain the "X" importance values
  ## - uses the default entropy function (can be user specified) 
  ## - data is ordered so that first coordinate is the target variable
  ##   potentially this allows refined/customization of the entropy function 
  ##
  ##------------------------------------------------------------------
  ## add some useful information for the entropy function
  dots.entropy$xvar.names <- xvar.names
  dots.entropy$data <- data
  ## set the dimension
  p <- ncol(x)
  ## extract entropy
  if (length(keep.rules) > 0) {
    impO <- papply(keep.rules, function(i) {
      ordernms <- c(xreleaseId[i], setdiff(1:p, xreleaseId[i]))
      dots.entropy$oobMembership <- oobMembership[[i]]
      dots.entropy$compMembership <- compMembership[[i]]
      xO <- x[oobMembership[[i]], ordernms, drop = FALSE]
      xC <- x[compMembership[[i]], ordernms, drop = FALSE]
      val <- do.call("entropy.function", c(list(xC, xO), dots.entropy))
      if (!is.list(val)) {
        list(imp = val, attr = NULL, xvar = xreleaseId[i])
      }
      else {
        list(imp = val[[1]], attr = val[[2]], xvar = xreleaseId[i])
      }
    })
    ## extract importance
    imp <- unlist(lapply(impO, "[[", 1))
    results$imp[keep.rules] <- imp
    ## extract attributes
    entropy.imp <- lapply(impO, "[[", 2)
    if (length(!sapply(entropy.imp, is.null)) == 0) {
      entropy.imp <- NULL
    }
    else {
      xreleaseId <- unlist(lapply(impO, "[[", 3))
      xreleaseIdUnq <- sort(unique(xreleaseId))
      entropy.imp <- lapply(xreleaseIdUnq, function(k) {
        ii <- entropy.imp[xreleaseId == k]
        ii[!sapply(ii, is.null)]        
      })
      names(entropy.imp) <- xvar.names[xreleaseIdUnq]
    }
  }
  else {
    entropy.imp <- NULL
  }
  ##------------------------------------------------------------------
  ##
  ##
  ## for default entropy, package up pc-simple results 
  ##
  ##
  ##------------------------------------------------------------------
  if (!custom.entropy.flag) {
    entropy.imp <- do.call("entropy.importance.function",
                           c(list(entropy.imp, xvar.names), dots.entropy))
  }
  ##------------------------------------------------------------------
  ##
  ##
  ## package results up as a varpro object
  ##
  ##
  ##------------------------------------------------------------------
  rO <- list()
  rO$results <- results
  rO$x <- data
  rO$y <- NULL
  rO$y.org <- NULL
  rO$xvar.names <- xvar.names
  rO$xvar.wt <- rep(1, length(xvar.names))
  rO$max.rules.tree <- max.rules.tree
  rO$max.tree <- max.tree
  rO$entropy <- entropy.imp
  rO$family <- "unsupv"
  class(rO) <- "varpro"
  rO
}
unsupv <- unsupv.varpro
