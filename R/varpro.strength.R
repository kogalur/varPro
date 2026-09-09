varpro.strength <- function(object,
                            newdata,
                            m.target = NULL,
                            max.rules.tree = 150,
                            max.tree = 150,
                            stat = c("importance", "complement", "oob", "none"),
                            membership = FALSE,
                            neighbor = 5,
                            seed = NULL,
                            do.trace = FALSE,
                            ...)
{
    ## only applies to rfsrc grow objects
    if ( (sum(inherits(object, c("rfsrc", "grow"), TRUE) == c(1, 2)) == 2) ||
         (sum(inherits(object, c("rhf", "grow"), TRUE) == c(1, 2)) == 2) ) {
        ## pass
    }
    else {
        stop("This function only works for objects of class '(rfsrc, grow)' and '(rhf, grow)'")
    }
    is.rhf.grow <- sum(inherits(object, c("rhf", "grow"), TRUE) == c(1, 2)) == 2
    if (is.rhf.grow) {
        ## default pseudo-response for RHF
        w.ir <- object$int.haz.oob
        if (is.null(w.ir)) {
            stop("For RHF, supply w.ir or fit/store int.haz.inbag.")
        }
    }
    else {
        w.ir <- NULL
    }
  ## get any hidden options
  user.option <- list(...)
  if(max.rules.tree > 2^31 - 1) {
      stop("max.rules.tree must be less than 2^31 - 1:  ", max.rules.tree)
  }
  if(max.tree > 2^31 - 1) {
      stop("max.tree must be less than 2^31 - 1:  ", max.tree)
  }
  stat <- match.arg(stat, c("importance", "complement", "oob", "none"))
  stat.bits = get.stat.bits(stat)
  ## set restore.mode and the ensemble option
  if (missing(newdata)) {##restore: no data
    restore.mode <- TRUE
  }
  else {##not restore: test data present
    restore.mode <- FALSE
    stat <- "none"
    membership <- TRUE
  }
  ## inbag or oob?
  oob.bits <- get.varpro.strength.bits(user.option$oob.bits, restore.mode)
  freq.table.bits <- get.freq.table.bits(user.option$freq.table.flag, restore.mode)
  ## check if this is an anonymous object
  ## coerce values as necessary
  ## graceful return if restore.mode = TRUE which is not allowed for anonymous
  if (inherits(object, "anonymous")) {
    anonymize.bits <- 2^26
    if (restore.mode) {
      stop("in order to predict with anonymous forests please provide a test data set")
    }
  }
  else {
    anonymize.bits <- 0
  }
  ## set the family
  family <- object$family
  ## pull the x-variable and y-outcome names from the grow object
  xvar.names <- object$xvar.names
  yvar.names <- object$yvar.names
  ## Initialize the seed.
  seed <- get.seed(seed)
  ## REDUCES THE OBJECT TO THE FOREST -- REDUCTION STARTS HERE
  object <- object$forest
  ## Determine the immutable yvar factor map which is needed for
  ## classification sexp dimensioning.  But, first convert object$yvar
  ## to a data frame which is required for factor processing
  #object$yvar <- as.data.frame(object$yvar)
  #colnames(object$yvar) <- yvar.names
  yfactor <- object$yvar.factor
  ## multivariate family details
  m.target.idx <- get.outcome.target(family, yvar.names, m.target)
  ## Short cut to get the y-outcome type and number of levels.
  yvar.types <- yfactor$types
  yvar.nlevels  <- yfactor$nlevels
  yvar.numeric.levels <- yfactor$numeric.levels
  ## Slight differences in RSF vs RHF:
  if (sum(inherits(object, c("rfsrc", "forest"), TRUE) == c(1, 2)) == 2) {
      ## Recover the individual subject identifiers, if they exist.
      subj <- object$subj
  }
  else if (sum(inherits(object, c("rhf", "forest"), TRUE) == c(1, 2)) == 2) {
      ## Recover the individual subject identifiers, if they exist.
      subj <- object$id
  }
  ## Get event information for survival families.
  event.info <- object$event.info
  event.type <- event.info$event.type
  ## CR.bits assignment
  cr.bits <- get.cr.bits(family)
  ## Determine the immutable xvar factor map.
  xfactor <- object$xvar.factor
  any.xvar.factor <-  (length(xfactor$factor) + length(xfactor$order)) > 0
  ## Short cut to get the x-variable type and number of levels.
  xvar.types <- xfactor$types
  xvar.nlevels <- xfactor$nlevels
  xvar.numeric.levels <- xfactor$numeric.levels
  ## Set dimensions.
  n.xvar <- length(xvar.names)
  n <- object$n
  r.dim <- event.info$rdim
  ## Outcome will always equal train.
  ## Data conversion for y-training data.
  yvar <- as.matrix(data.matrix(data.frame(object$yvar)))
  ##--------------------------------------------------------
  ##
  ## process x and y: test data is present
  ##
  ##--------------------------------------------------------
  if (!restore.mode) {
    ## obtain the dimension
    n.newdata <- nrow(newdata)
    ## restrict xvar to the training xvar.names
    xvar.newdata <- newdata[, xvar.names, drop=FALSE]
    ## extract test yvar (if present)
    yvar.present <- sum(is.element(yvar.names, names(newdata))) > 0
    if (yvar.present) {
      yvar.newdata <- as.matrix(newdata[, yvar.names, drop = FALSE])
    }
    else {
      yvar.newdata <-  NULL
    }
    ## coherent setting for nearest neighbor
    neighbor <- min(neighbor, n)
    ## reduce option
    if (is.null(user.option$reduce)) {
      x.reduce.idx <- 1:n.xvar
    }
    else {
      x.reduce.idx <- user.option$reduce
    }
  }
  else {    
    ## There cannot be test data in restore mode
    ## The native code switches based on n.newdata being zero (0).  Be careful.
    n.newdata <- 0
    xvar.newdata <- yvar.newdata <- NULL
    neighbor  <- NULL
    x.reduce.idx  <- NULL
  }
  ## Respect the training options related to bootstrapping:
  if (sum(inherits(object, c("rfsrc", "forest"), TRUE) == c(1, 2)) == 2) {
      sampsize <- round(object$sampsize(n))
      case.wt <- object$case.wt
      samp <- object$samp
      bootstrap.bits <- get.bootstrap.bits(object$bootstrap)
      samptype.bits <- get.samptype.bits(object$samptype)
  }
  else if (sum(inherits(object, c("rhf", "forest"), TRUE) == c(1, 2)) == 2) {
      sampsize <- object$parms$sampsize
      case.wt <- object$parms$case.wt
      samp <- object$parms$samp
      bootstrap.bits <- get.bootstrap.bits(object$parms$bootstrap)      
      samptype.bits <- get.samptype.bits(object$parms$samptype)
  }
  terminal.qualts.bits <- get.terminal.qualts.restore.bits(object$terminal.qualts)
  terminal.quants.bits <- get.terminal.quants.restore.bits(object$terminal.quants)
  ## Data conversion for x-training data.
  if (anonymize.bits == 0) {
      xvar <- as.matrix(data.matrix(object$xvar))
      rownames(xvar) <- colnames(xvar) <- NULL
  }
  else {
      xvar <- NULL
  }
  ## set the data.pass flags: currently the training data.pass flag must be asserted
  ## for the program to work. This means that the training data sent into RF-SRC
  ## must contain no missing data. If this condition is not satisfied the program
  ## will segfault. TBD2
  data.pass.bits <- get.data.pass.bits(object$data.pass)
  ## testing data.pass is na.action AND restore.mode dependent
  ## Initialize the number of trees in the forest.
  ntree <- object$ntree
  ## We always process all trees.
  get.tree <- get.tree.index(NULL, ntree)
  ## Set the user defined trace.
  do.trace <- get.trace(do.trace)
  ## Check that hdim is initialized.  If not, set it zero.
  ## This is necessary for backwards compatibility with 2.3.0
  if (is.null(object$hdim)) {
    hdim <- 0
  }
  else {
    hdim <- object$hdim
  }
  ## Start the C external timer.
  ctime.external.start  <- proc.time()
##    source("varProStrength_call_dump.R")
##    source("varProStrength_call_dump_INSERT.R")
  nativeOutput <- tryCatch({.Call("varProStrength",
                                  as.integer(do.trace),
                                  as.integer(seed),
                                  as.integer(bootstrap.bits +
                                             cr.bits),                 ## low option byte
                                  as.integer(samptype.bits +
                                             terminal.qualts.bits +
                                             terminal.quants.bits +
                                             data.pass.bits),          ## high option byte
                                  as.integer(stat.bits + oob.bits + freq.table.bits), ## varpro option byte
                                  as.integer(ntree),
                                  as.integer(n),
                                  list(as.integer(length(case.wt)),
                                       if (is.null(case.wt)) NULL else as.double(case.wt),
                                       as.integer(sampsize),
                                       if (is.null(samp)) NULL else as.integer(samp)),
                                  list(if (is.null(m.target.idx)) as.integer(0) else as.integer(length(m.target.idx)),
                                       if (is.null(m.target.idx)) NULL else as.integer(m.target.idx)),
                                  list(if (is.null(yvar.types)) NULL else as.integer(length(yvar.types)),
                                       if (is.null(yvar.types)) NULL else as.character(yvar.types),
                                       if (is.null(yvar.types)) NULL else as.integer(yvar.nlevels),
                                       if (is.null(yvar.numeric.levels)) NULL else sapply(1:length(yvar.numeric.levels), function(nn) {as.integer(length(yvar.numeric.levels[[nn]]))}),
                                       if (is.null(subj)) NULL else as.integer(subj),
                                       if (is.null(event.type)) NULL else as.integer(length(event.type)),
                                       if (is.null(event.type)) NULL else as.integer(event.type)),
                                  if (is.null(yvar.numeric.levels)) {
                                      NULL
                                  }
                                  else {
                                      lapply(1:length(yvar.numeric.levels),
                                             function(nn) {as.integer(yvar.numeric.levels[[nn]])})
                                  },
                                  if (is.null(yvar.types)) NULL else as.double(as.vector(yvar)),
                                  if (is.null(w.ir)) NULL else as.double(w.ir),
                                  list(as.integer(n.xvar),
                                       if (is.null(xvar.types)) NULL else as.character(xvar.types),
                                       if (is.null(xvar.types)) NULL else as.integer(xvar.nlevels),
                                       if (is.null(xvar.numeric.levels)) NULL else sapply(1:length(xvar.numeric.levels), function(nn) {as.integer(length(xvar.numeric.levels[[nn]]))}),
                                       NULL,
                                       NULL),
                                  if (is.null(xvar.numeric.levels)) {
                                      NULL
                                  }
                                  else {
                                      lapply(1:length(xvar.numeric.levels),
                                             function(nn) {as.integer(xvar.numeric.levels[[nn]])})
                                  },
                                  as.double(as.vector(xvar)),
                                  list(if(is.null(event.info$time.interest)) as.integer(0) else as.integer(length(event.info$time.interest)),
                                       if(is.null(event.info$time.interest)) NULL else as.double(event.info$time.interest)),
                                  as.integer(n.newdata),
                                  if (is.null(yvar.newdata)) NULL else as.double(as.vector(yvar.newdata)),
                                  if (is.null(xvar.newdata)) NULL else as.double(as.vector(data.matrix(xvar.newdata))),
                                  list(if (is.null(neighbor)) 0 else as.integer(neighbor),
                                       if (is.null(x.reduce.idx)) as.integer(0) else as.integer(length(x.reduce.idx)),
                                       if (is.null(x.reduce.idx)) NULL else as.integer(x.reduce.idx)),
                                  as.integer(object$totalNodeCount),
                                  as.integer(object$leafCount),
                                  list(as.integer(object$seed)),
                                  as.integer(hdim),
                                  as.integer((object$nativeArray)$treeID),
                                  as.integer((object$nativeArray)$nodeID),
                                  as.integer((object$nativeArray)$nodeSZ),
                                  as.integer((object$nativeArray)$brnodeID),
                                  ## This is hc_zero.  It is never NULL.
                                  list(as.integer((object$nativeArray)$parmID),
                                  as.double((object$nativeArray)$contPT),
                                  as.integer((object$nativeArray)$mwcpSZ),
                                  as.integer((object$nativeArray)$fsrecID),
                                  if (is.null((object$nativeFactorArray)$mwcpPT)) NULL else as.integer((object$nativeFactorArray)$mwcpPT)),
                                  if (sum(inherits(object, c("rfsrc", "forest"), TRUE) == c(1, 2)) == 2)
                                      as.integer(object$nativeArrayTNDS$tnRMBR)
                                  else if (sum(inherits(object, c("rhf", "forest"), TRUE) == c(1, 2)) == 2) 
                                      as.integer(object$trmbrCaseId),
                                  if (sum(inherits(object, c("rfsrc", "forest"), TRUE) == c(1, 2)) == 2)
                                      as.integer(object$nativeArrayTNDS$tnAMBR)
                                  else if (sum(inherits(object, c("rhf", "forest"), TRUE) == c(1, 2)) == 2) 
                                      NULL,
                                  if (sum(inherits(object, c("rfsrc", "forest"), TRUE) == c(1, 2)) == 2)
                                      as.integer(object$nativeArrayTNDS$tnOMBR)
                                  else if (sum(inherits(object, c("rhf", "forest"), TRUE) == c(1, 2)) == 2) 
                                      as.integer(object$tombrCaseId),
                                  if (sum(inherits(object, c("rfsrc", "forest"), TRUE) == c(1, 2)) == 2)
                                      as.integer(object$nativeArrayTNDS$tnIMBR)
                                  else if (sum(inherits(object, c("rhf", "forest"), TRUE) == c(1, 2)) == 2) 
                                      as.integer(object$timbrCaseId),
                                  if (sum(inherits(object, c("rfsrc", "forest"), TRUE) == c(1, 2)) == 2)
                                      as.integer(object$nativeArrayTNDS$tnRCNT)
                                  else if (sum(inherits(object, c("rhf", "forest"), TRUE) == c(1, 2)) == 2) 
                                      as.integer(object$trmbrCaseCt),
                                  if (sum(inherits(object, c("rfsrc", "forest"), TRUE) == c(1, 2)) == 2)
                                      as.integer(object$nativeArrayTNDS$tnACNT)
                                  else if (sum(inherits(object, c("rhf", "forest"), TRUE) == c(1, 2)) == 2) 
                                      NULL,
                                  if (sum(inherits(object, c("rfsrc", "forest"), TRUE) == c(1, 2)) == 2)
                                      as.integer(object$nativeArrayTNDS$tnOCNT)
                                  else if (sum(inherits(object, c("rhf", "forest"), TRUE) == c(1, 2)) == 2) 
                                      as.integer(object$tombrCaseCt),
                                  if (sum(inherits(object, c("rfsrc", "forest"), TRUE) == c(1, 2)) == 2)
                                      as.integer(object$nativeArrayTNDS$tnICNT)
                                  else if (sum(inherits(object, c("rhf", "forest"), TRUE) == c(1, 2)) == 2) 
                                      as.integer(object$timbrCaseCt),
                                  if (sum(inherits(object, c("rfsrc", "forest"), TRUE) == c(1, 2)) == 2)
                                      as.integer(object$nativeArrayTNDS$oobSZ)
                                  else if (sum(inherits(object, c("rhf", "forest"), TRUE) == c(1, 2)) == 2) 
                                      as.integer(object$oobSizeCase),    
                                  if (sum(inherits(object, c("rfsrc", "forest"), TRUE) == c(1, 2)) == 2)
                                      as.integer(object$nativeArrayTNDS$ibgSZ)
                                  else if (sum(inherits(object, c("rhf", "forest"), TRUE) == c(1, 2)) == 2) 
                                      as.integer(object$ibgSizeCase),
                                  if (sum(inherits(object, c("rfsrc", "forest"), TRUE) == c(1, 2)) == 2)
                                      as.double((object$nativeArrayTNDS$tnSURV))
                                  else NULL,
                                  if (sum(inherits(object, c("rfsrc", "forest"), TRUE) == c(1, 2)) == 2)
                                      as.double((object$nativeArrayTNDS$tnMORT))
                                  else NULL,
                                  if (sum(inherits(object, c("rfsrc", "forest"), TRUE) == c(1, 2)) == 2)
                                      as.double((object$nativeArrayTNDS$tnNLSN))
                                  else NULL,
                                  if (sum(inherits(object, c("rfsrc", "forest"), TRUE) == c(1, 2)) == 2)
                                      as.double((object$nativeArrayTNDS$tnCSHZ))
                                  else NULL,
                                  if (sum(inherits(object, c("rfsrc", "forest"), TRUE) == c(1, 2)) == 2)
                                      as.double((object$nativeArrayTNDS$tnCIFN))
                                  else NULL,
                                  if (sum(inherits(object, c("rfsrc", "forest"), TRUE) == c(1, 2)) == 2)
                                      as.double((object$nativeArrayTNDS$tnREGR))
                                  else NULL,
                                  if (sum(inherits(object, c("rfsrc", "forest"), TRUE) == c(1, 2)) == 2)
                                      as.integer((object$nativeArrayTNDS$tnCLAS))
                                  else NULL,
                                  as.integer(max.rules.tree),
                                  as.integer(max.tree),
                                  as.integer(get.tree),
                                  as.integer(get.rf.cores()))},
                           ## error = function(e) {NULL}
                           ## comment above and uncomment below to
                           ## reveal more deets if the .Call() is
                           ## failing before entry.c :
                           error = function(e) { print(e); NULL }
                           )
  ## Stop the C external timer.
  ctime.external.stop <- proc.time()
  ## check for error return condition in the native code
  if (is.null(nativeOutput)) {
    stop("An error has occurred in prediction.  Please turn trace on for further analysis.")
  }
  ## Matrix output representing what Hemant wants.
  strengthArraySize <- length(nativeOutput$treeID)
  strengthArray <- as.data.frame(cbind(nativeOutput$treeID[1:strengthArraySize],
                                       nativeOutput$nodeID[1:strengthArraySize],
                                       nativeOutput$xReleaseID[1:strengthArraySize],
                                       nativeOutput$cmpCT[1:strengthArraySize]))
  strengthTreeID  <- nativeOutput$strengthTreeID
  if (!restore.mode) {
      testCaseTermID  <- matrix(nativeOutput$testCaseTermID, nrow = n.newdata)
      score  <- vector("list", length = n.newdata)
      offset  <- 0
      for (i in 1: n.newdata) {
          score[[i]]$stat <- nativeOutput$twinStat[(offset+1):(offset+neighbor)]
          score[[i]]$id   <- nativeOutput$twinStatID[(offset+1):(offset+neighbor)]
          offset  <- offset + neighbor
      }
      if (freq.table.bits > 0) {
          offset  <- 0
          dim1 <- neighbor
          dim2 <- if (is.null(x.reduce.idx)) n.xvar else length(x.reduce.idx)
          offset.incr <- dim1 * dim2
          for (i in 1: n.newdata) {
              score[[i]]$freq.table <- matrix(nativeOutput$twinFreqTable[(offset+1):(offset + offset.incr)], nrow = dim1, byrow = TRUE)
              offset  <- offset + offset.incr
          }
      }
  }
  else {
      testCaseTermID  <- NULL
      score           <- NULL
  }
  #####################################################################
  ##
  ## !!!!!!!!!!! DO NOT CHANGE THE NAMES OF THESE COLUMNS !!!!!!!!!!!!
  ##
  ##
  strengthArrayHeader <- c("treeID", "nodeID", "xReleaseID", "compCT")
  ##
  ##
  #####################################################################
  ## We consider "R", "I", and "C" outcomes.  The outcomes are grouped
  ## by type and sequential.  That is, the first "C" encountered in the
  ## response type vector is in position [[1]] in the classification output
  ## list, the second "C" encountered is in position [[2]] in the
  ## classification output list, and so on.  The same applies to the
  ## regression outputs.  We also have a mapping from the outcome slot back
  ## to the original response vector type, given by the following:
  ## Given yvar.types = c("R", "C", "R", "C", "R" , "I")
  ## regr.index[1] -> 1
  ## regr.index[2] -> 3
  ## regr.index[3] -> 5
  ## clas.index[1] -> 2
  ## clas.index[2] -> 4
  ## clas.index[3] -> 6
  ## This will pick up all "C" and "I".
  class.index <- which(yvar.types != "R")
  class.count <- length(class.index)
  regr.index <- which(yvar.types == "R")
  regr.count <- length(regr.index)
  if(family == "surv") {
      strengthArray <- as.data.frame(cbind(strengthArray,
                                           nativeOutput$brmCT[1:strengthArraySize]))
      strengthArrayHeader <- c(strengthArrayHeader, "oobCT")
      if(stat == "importance") {
          strengthArray = as.data.frame(cbind(strengthArray,
                                              nativeOutput$statImportance[1:strengthArraySize]))
          strengthArrayHeader <- c(strengthArrayHeader, "importance")
      }
      else if(stat == "complement") {
          strengthArray = as.data.frame(cbind(strengthArray,
                                              nativeOutput$statComplement[1:strengthArraySize]))
          strengthArrayHeader <- c(strengthArrayHeader, "mortalityComplement")
      }
      else if(stat == "oob") {
          strengthArray = as.data.frame(cbind(strengthArray,
                                              nativeOutput$statBranch[1:strengthArraySize]))
          strengthArrayHeader <- c(strengthArrayHeader,
                             if (is.null(w.ir)) "mortalityOOB" else "wirOOB")
      }
  }
  else if(family == "regr") {
      strengthArray <- as.data.frame(cbind(strengthArray,
                                           nativeOutput$brmCT[1:strengthArraySize]))
      strengthArrayHeader <- c(strengthArrayHeader, "oobCT")
      if(stat == "importance") {
          strengthArray = as.data.frame(cbind(strengthArray,
                                              nativeOutput$statImportance[1:strengthArraySize]))
          strengthArrayHeader <- c(strengthArrayHeader, "importance")
      }
      else if(stat == "complement") {
          strengthArray = as.data.frame(cbind(strengthArray,
                                              nativeOutput$statComplement[1:strengthArraySize]))
          strengthArrayHeader <- c(strengthArrayHeader, "meanComplement")
      }
      else if(stat == "oob") {
          strengthArray = as.data.frame(cbind(strengthArray,
                                              nativeOutput$statBranch[1:strengthArraySize]))
          strengthArrayHeader <- c(strengthArrayHeader, "meanOOB")
      }
  }
  else if (family == "regr+") {
      strengthArray <- as.data.frame(cbind(strengthArray,
                                           nativeOutput$brmCT[1:strengthArraySize]))
      strengthArrayHeader <- c(strengthArrayHeader, "oobCT")
      if(stat == "importance") {
          ## From the native code:
          ##   "statImportance"
          ## -> of dim [regr.count] x [strengthArraySize]
          ## To the R code:
          ## -> of dim  [strengthArraySize] x [regr.count]
          strengthArray = as.data.frame(cbind(strengthArray,
                                              array(nativeOutput$statImportance, c(strengthArraySize, regr.count))))
          impArrayHeader <- NULL
          ## We don't support targets yet.
          impArrayHeader <- yvar.names[regr.index]
          impArrayHeader <- paste("imp[", impArrayHeader, "]", sep="")
          strengthArrayHeader <- c(strengthArrayHeader, impArrayHeader)
      }
      else if(stat == "complement") {
          ## From the native code:
          ##   "statComplement"
          ## -> of dim [regr.count] x [strengthArraySize]
          ## To the R code:
          ## -> of dim  [strengthArraySize] x [regr.count]
          strengthArray = as.data.frame(cbind(strengthArray,
                                              array(nativeOutput$statComplement, c(strengthArraySize, regr.count))))
          impArrayHeader <- NULL
          ## We don't support targets yet.
          impArrayHeader <- yvar.names[regr.index]
          impArrayHeader <- paste("compMean[", impArrayHeader, "]", sep="")
          strengthArrayHeader <- c(strengthArrayHeader, impArrayHeader)
      }
      else if(stat == "oob") {
          ## From the native code:
          ##   "statBranch"
          ## -> of dim [regr.count] x [strengthArraySize]
          ## To the R code:
          ## -> of dim  [strengthArraySize] x [regr.count]
          strengthArray = as.data.frame(cbind(strengthArray,
                                              array(nativeOutput$statBranch, c(strengthArraySize, regr.count))))
          impArrayHeader <- NULL
          ## We don't support targets yet.
          impArrayHeader <- yvar.names[regr.index]
          impArrayHeader <- paste("oobMean[", impArrayHeader, "]", sep="")
          strengthArrayHeader <- c(strengthArrayHeader, impArrayHeader)
      }
  }
  else if (family == "class") {
      ## The incoming vector from the c-side is virtualized as
      ## [strengthArraySize] x [1] x [1 + levels.count[]]
      ## Here is an example for the offsets
      ## We assume the iris data set.
      ## The unconditional value plus the conditional values result in a vector of length 4.
      ## For example: the indices of the unconditional vector will be offset, offset + (1 + levels.count[]), offset + (2 x (1 + levels.count[])),
      ## offset + (3 x (1 + levels.count)), ... , offset + ((strengthArraySize - 1) x (1 + levels.count[]))
      ## offset = seq(from = 0, by = 1 + levels.count, length.out = strengthArraySize)
      ## [1] x [1 + levels.count[]] x [strengthArraySize]
      offset <- 0
      strengthArray <- as.data.frame(cbind(strengthArray,
                                           nativeOutput$brmCT[(offset + 1):(offset + strengthArraySize)]))
      strengthArrayHeader <- c(strengthArrayHeader, "oobCT")
      for(i in 1:yfactor$nlevels) {
          offset <- offset + strengthArraySize
          strengthArray <- as.data.frame(cbind(strengthArray,
                                               nativeOutput$brmCT[(offset + 1):(offset + strengthArraySize)]))
          strengthArrayHeader <- c(strengthArrayHeader, paste("oobCT.", i, sep=""))
      }
      offset <- 0
      if(stat == "importance") {
          strengthArray = as.data.frame(cbind(strengthArray,
                                              nativeOutput$statImportance[(offset + 1):(offset + strengthArraySize)]))
          strengthArrayHeader <- c(strengthArrayHeader, "importance")
          for(i in 1:yfactor$nlevels) {
              offset <- offset + strengthArraySize
              strengthArray = as.data.frame(cbind(strengthArray,
                                                  nativeOutput$statImportance[(offset + 1):(offset + strengthArraySize)]))
              strengthArrayHeader <- c(strengthArrayHeader, paste("importance.", i, sep=""))
          }
      }
      else if(stat == "complement") {
          strengthArray = as.data.frame(cbind(strengthArray,
                                              nativeOutput$statComplement[(offset + 1):(offset + strengthArraySize)]))
          strengthArrayHeader <- c(strengthArrayHeader, "importance")
          for(i in 1:yfactor$nlevels) {
              offset <- offset + strengthArraySize
              strengthArray = as.data.frame(cbind(strengthArray,
                                                  nativeOutput$statComplement[(offset + 1):(offset + strengthArraySize)]))
              strengthArrayHeader <- c(strengthArrayHeader, paste("complementFreq.", i, sep=""))
          }
      }
      else if(stat == "oob") {
          strengthArray = as.data.frame(cbind(strengthArray,
                                              nativeOutput$statBranch[(offset + 1):(offset + strengthArraySize)]))
          strengthArrayHeader <- c(strengthArrayHeader, "importance")
          for(i in 1:yfactor$nlevels) {
              offset <- offset + strengthArraySize
              strengthArray = as.data.frame(cbind(strengthArray,
                                                  nativeOutput$statBranch[(offset + 1):(offset + strengthArraySize)]))
              strengthArrayHeader <- c(strengthArrayHeader, paste("oobFreq.", i, sep=""))
          }
      }
  }
  else {
      ## Unsupervised!
      strengthArray <- as.data.frame(cbind(strengthArray,
                                           nativeOutput$brmCT[1:strengthArraySize]))
      strengthArrayHeader <- c(strengthArrayHeader, "oobCT")
  }
  names(strengthArray) <- strengthArrayHeader
  ## -----------------------------------------------------------------
  ##
  ##
  ## return branch and complementary membership indices for each rule
  ##
  ##
  ## -----------------------------------------------------------------
  if (membership) {
    ## number of records in strengthArray
    membershipListSize <- nrow(strengthArray)
    ## initialize the complement count vectors using doubles to avoid
    ## integer overflow for very large forests / membership payloads.
    count <- as.double(strengthArray$compCT)
    if (any(!is.finite(count)) || any(count < 0)) {
      stop("Encountered invalid complement membership counts in 'compCT'.")
    }
    countTo <- cumsum(count)
    countFrom <- if (membershipListSize > 0L) {
      c(1, head(countTo, -1L) + 1)
    } else {
      numeric(0)
    }
    ## create complement membership list that will contain the complement
    ## members for each tree, branch, and xReleaseID
    compMembershipList <- vector("list", length = membershipListSize)
    if (membershipListSize > 0L) {
      n.comp <- length(nativeOutput$complementMembers)
      if (tail(countTo, 1L) > n.comp) {
        stop("Complement membership offsets exceed the length of 'complementMembers'.")
      }
      ## initialize the complement membership list
      for (i in seq_len(membershipListSize)) {
        if (countTo[i] >= countFrom[i]) {
          compMembershipList[[i]] <- nativeOutput$complementMembers[
            seq.int(countFrom[i], countTo[i])
          ]
        } else {
          compMembershipList[[i]] <- list(NULL)
        }
      }
    }
    ## create and zero the maximum vector.  Keep this double-valued as well
    ## so the cumulative branch counts remain safe at large scale.
    countBRM <- numeric(membershipListSize)
    ## k will count the number of branches which is less than or equal to the
    ## number of records (membershipListSize) because of the different
    ## xRelease variables for each branch
    k <- 0L
    ## Initialize countBRM.
    if (membershipListSize > 0L) {
      for (i in seq_len(membershipListSize)) {
        if (i == 1L) {
          k <- k + 1L
          countBRM[k] <- as.double(strengthArray$oobCT[i])
        } else {
          if ((strengthArray$nodeID[i] != strengthArray$nodeID[i - 1L]) ||
              (strengthArray$treeID[i] != strengthArray$treeID[i - 1L])) {
            k <- k + 1L
            countBRM[k] <- as.double(strengthArray$oobCT[i])
          }
        }
      }
    }
    ## initialize the branch count vectors
    countToBRM <- cumsum(countBRM)
    countFromBRM <- if (membershipListSize > 0L) {
      c(1, head(countToBRM, -1L) + 1)
    } else {
      numeric(0)
    }
    ## create BRM membership list that will contain the BRM
    ## members for each tree, branch, and xReleaseID
    branchMembershipList <- vector("list", length = membershipListSize)
    ## j will count the number of branches
    j <- 0L
    if (membershipListSize > 0L) {
      n.branch <- length(nativeOutput$branchMembers)
      if (max(countToBRM, na.rm = TRUE) > n.branch) {
        stop("Branch membership offsets exceed the length of 'branchMembers'.")
      }
      for (i in seq_len(membershipListSize)) {
        if (i == 1L) {
          j <- j + 1L
          if (countToBRM[j] >= countFromBRM[j]) {
            branchMembershipList[[i]] <- nativeOutput$branchMembers[
              seq.int(countFromBRM[j], countToBRM[j])
            ]
          } else {
            branchMembershipList[[i]] <- list(NULL)
          }
        } else {
          if ((strengthArray$nodeID[i] == strengthArray$nodeID[i - 1L]) &&
              (strengthArray$treeID[i] == strengthArray$treeID[i - 1L])) {
            branchMembershipList[[i]] <- branchMembershipList[[i - 1L]]
          } else {
            j <- j + 1L
            if (countToBRM[j] >= countFromBRM[j]) {
              branchMembershipList[[i]] <- nativeOutput$branchMembers[
                seq.int(countFromBRM[j], countToBRM[j])
              ]
            } else {
              branchMembershipList[[i]] <- list(NULL)
            }
          }
        }
      }
    }
    ## clean up the lists
    branchMembershipList <- lapply(branchMembershipList, unlist)
    compMembershipList <- lapply(compMembershipList, unlist)
  }
  ## return NULL otherwise
  else {
    branchMembershipList <- compMembershipList <- NULL
  }
  ## make the output object
  list(
    call = match.call(),
    strengthArray = strengthArray,
    strengthTreeID = strengthTreeID,
    testCaseTermID = testCaseTermID,
    score          = score,
    oobMembership = branchMembershipList,
    compMembership = compMembershipList,
    ctime.internal = nativeOutput$cTimeInternal,
    ctime.external = ctime.external.stop - ctime.external.start
  )
}
