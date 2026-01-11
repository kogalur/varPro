uvarpro <- function(data,
                    method = c("auto", "unsupv", "rnd"),
                    ntree = 200, nodesize = NULL,
                    max.rules.tree = 20, max.tree = 200,
                    verbose = FALSE, seed = NULL,
                    ...)
{
  ##------------------------------------------------------------------
  ##
  ## pre-processing
  ##
  ##------------------------------------------------------------------
  ## set method
  method <- match.arg(method, c("auto", "unsupv", "rnd"))
  ## data must be a data frame without missing values
  data <- data.frame(na.omit(data))
  ## droplevels
  data <- droplevels(data)
  ## initialize the seed
  seed <- get.seed(seed)
  ## get options
  dots <- list(...)
  ##--------------------------------------------------------------
  ##
  ## define the entropy function (or obtain user specified one)
  ##
  ##--------------------------------------------------------------
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
  ## get the permissible hidden options for rfsrc
  dots <- dots[names(dots) %in% rfnames]
  ##-----------------------------------------------------------------
  ##
  ## process data
  ##
  ##------------------------------------------------------------------
  ## save the original names
  xvar.org.names <- colnames(data)
  ## hot encode the data
  data <- get.hotencode(data)
  ## assign the xvar names
  xvar.names <- colnames(data)
  ##------------------------------------------------------------------
  ##
  ## unsupervised forests
  ##
  ##------------------------------------------------------------------
  if (method == "unsupv" && !user.provided.varpro.flag) {
    dots$ytry <- set.unsupervised.ytry(nrow(data), ncol(data), dots$ytry)
    o <- do.call("rfsrc", c(list(
      data = data,
      ntree = ntree,
      nodesize = set.unsupervised.nodesize(nrow(data), ncol(data), nodesize),
      perf.type = "none"), dots))
  }
  ##------------------------------------------------------------------
  ##
  ## pure random forests
  ##
  ##------------------------------------------------------------------
  if (method == "rnd" && !user.provided.varpro.flag) {
    dots$splitrule <- NULL
    o <- do.call("rfsrc", c(list(formula = yxyz123 ~ .,
      data = data.frame(yxyz123 = rnorm(nrow(data)), data),
      splitrule = "random",
      ntree = ntree,
      nodesize = set.unsupervised.nodesize(nrow(data), ncol(data) + 1, nodesize),
      perf.type = "none"), dots))
  }
  ##------------------------------------------------------------------
  ##
  ## auto-encoder (regr+)
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
  ## call varpro.strength and extract necessary information
  ##
  ##------------------------------------------------------------------
  ## switch for varpro strength depends on whether object is a forest
  oo <- get.varpro.strength(o, membership = TRUE,
                            max.rules.tree = max.rules.tree,
                            max.tree = max.tree)
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
  ## obtain the "X" importance values
  ##
  ##------------------------------------------------------------------
  ## add some useful information for the entropy function
  dots.entropy$xvar.names <- xvar.names
  dots.entropy$data <- data
  ## set the dimension
  p <- ncol(x)
  entropy.values <- NULL
  ## extract entropy
  if (length(keep.rules) > 0L) {
    ## if entropy is the package default, ordering does not matter and we can
    ## avoid building per-rule column permutations.
    need.order <- isTRUE(custom.entropy.flag)
    dots.entropy.base <- dots.entropy
    res.list <- lapply(keep.rules, function(i) {
      ## per-rule args (membership)
      de <- dots.entropy.base
      de$oobMembership <- oobMembership[[i]]
      de$compMembership <- compMembership[[i]]
      ## subset (and optionally re-order) so the release variable is first
      if (need.order) {
        rel <- xreleaseId[i]
        ## faster than setdiff(1:p, rel)
        ordernms <- c(rel, seq_len(p)[-rel])
        xO <- x[oobMembership[[i]], ordernms, drop = FALSE]
        xC <- x[compMembership[[i]], ordernms, drop = FALSE]
      } else {
        xO <- x[oobMembership[[i]], , drop = FALSE]
        xC <- x[compMembership[[i]], , drop = FALSE]
      }
      val <- do.call(entropy.function, c(list(xC, xO), de))
      if (!is.list(val)) {
        list(imp = val, attr = NULL)
      } else {
        list(imp = val[[1]], attr = val[[2]])
      }
    })
    ## extract importance (robust to length>1 outputs)
    imp <- vapply(res.list,
                  function(z) {
                    v <- z$imp
                    v <- as.numeric(v)
                    if (length(v)) v[1] else NA_real_
                  },
                  FUN.VALUE = numeric(1))
    results$imp[keep.rules] <- imp
    ## build entropy values grouped by released variable
    attr.list <- lapply(res.list, `[[`, "attr")
    has.attr <- !vapply(attr.list, is.null, logical(1))
    if (any(has.attr)) {
      rel.id <- xreleaseId[keep.rules]
      rel.id <- rel.id[has.attr]
      urel <- sort(unique(rel.id))
      entropy.values <- setNames(vector("list", length(urel)), xvar.names[urel])
      ## append in keep.rules order (matches original behavior)
      idx <- which(has.attr)
      for (j in idx) {
        nm <- xvar.names[xreleaseId[keep.rules][j]]
        entropy.values[[nm]] <- c(entropy.values[[nm]], list(attr.list[[j]]))
      }
    } else {
      entropy.values <- NULL
    }
  }
  ##------------------------------------------------------------------
  ##
  ## gets default entropy values and packages them up nicely
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
    entropy.values <- do.call(get.entropy,
                              c(list(entropy.values, xvar.names), dots.get))
  }
  ##------------------------------------------------------------------
  ##
  ## package results up as a varpro object
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
