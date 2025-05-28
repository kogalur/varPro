uvarpro <- function(data,
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
  ## data must be a data frame without missing values
  data <- data.frame(na.omit(data))
  ## droplevels
  data <- droplevels(data)
  ## initialize the seed
  seed <- get.seed(seed)
  ##--------------------------------------------------------------
  ##
  ## define the entropy function (or obtain user specified one)
  ##
  ## can be a number or a list
  ## for numbers: this is the importance
  ## for lists: entry 1 = importance; entry 2 = entropy values
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
    get.entropy <- get.entropy.default
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
  enames <- names(formals(entropy.function))[-(1:2)]
  enames <- setdiff(enames, "...")
  dots.entropy <- dots[names(dots) %in% enames]
  diffnames <- setdiff(enames, names(dots.entropy))
  if (length(diffnames) > 0) {
    dots.entropy <- append(dots.entropy, formals(entropy.function)[diffnames])
  }
  user.provided.varpro.flag <- FALSE
  ## special feature allowing user to pass in an arbitrary varpro object
  ## the purpose of this is to allow access to the entropy function framework
  if (!is.null(dots$object)) {
    user.provided.varpro.flag <- TRUE
    o <- dots$object
    ## over-ride the supplied data if this is a varpro object
    if (inherits(o, "varpro")) {
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
  #void.var <- sapply(data, function(x){length(unique(x, na.rm = TRUE)) < 2})
  #if (sum(void.var) > 0) {
  #  data[, which(void.var)] <- NULL
  #}
  ## save the original names
  xvar.org.names <- colnames(data)
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
    o <- do.call("rfsrc", c(list(formula = get.mv.formula(paste0("y.", xvar.names)),
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
  ## switch for varpro strength depends on whether object is a forest 
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
    ## extract entropy values -- this allows for customization via the second slot
    entropy.values <- lapply(impO, "[[", 2)
    if (length(!sapply(entropy.values, is.null)) == 0) {
      entropy.values <- NULL
    }
    else {
      xreleaseId <- unlist(lapply(impO, "[[", 3))
      xreleaseIdUnq <- sort(unique(xreleaseId))
      entropy.values <- lapply(xreleaseIdUnq, function(k) {
        ii <- entropy.values[xreleaseId == k]
        ii[!sapply(ii, is.null)]        
      })
      names(entropy.values) <- xvar.names[xreleaseIdUnq]
    }
  }
  ## no viable rules
  else {
    entropy.values <- NULL
  }
  ##------------------------------------------------------------------
  ##
  ##
  ## gets default entropy values and packages them up nicely
  ##
  ##
  ##------------------------------------------------------------------
  if (!custom.entropy.flag) {
    getnames <- names(formals(get.entropy))[-(1:2)]
    getnames <- setdiff(getnames, "...")
    dots.get <- dots[names(dots) %in% getnames]
    diffnames <- setdiff(getnames, names(dots.get))
    if (length(diffnames) > 0) {
      dots.get <- append(dots.get, formals(get.entropy)[diffnames])
    }
    entropy.values <- do.call("get.entropy",
                           c(list(entropy.values, xvar.names), dots.get))
  }
  ##------------------------------------------------------------------
  ##
  ##
  ## package results up as a varpro object
  ##
  ##
  ##------------------------------------------------------------------
  rO <- list()
  rO$rf <- o
  rO$results <- results
  rO$x <- data
  rO$xvar.names <- xvar.names
  rO$xvar.org.names <- xvar.org.names
  rO$y <- NULL
  rO$y.org <- NULL
  rO$xvar.wt <- rep(1, length(xvar.names))
  rO$max.rules.tree <- max.rules.tree
  rO$max.tree <- max.tree
  rO$entropy <- entropy.values
  rO$family <- "unsupv"
  class(rO) <- "uvarpro"
  rO
}
