###  tree rule primary call --> makes repeated calls to the workhorse
getFastTreeRule <- function(object, papply = lapply, ntreeSeq = NULL) {
  ##----------------------------------------------------------------
  ##
  ## coherence checks
  ##
  ##----------------------------------------------------------------
  if (sum(inherits(object, c("rfsrc", "grow"), TRUE) == c(1, 2)) != 2) {
    stop("This function only works for objects of class `(rfsrc, grow)'")
  }
  if (randomForestSRC:::is.forest.missing(object)) {
    stop("forest is missing.  Re-run rfsrc (grow call) with forest=TRUE")
  }
  if (inherits(object, "anonymous")) {
    stop("this does not work for anonymous random forests\n")
  }
  ##------------------------------------------------------------------------
  ##
  ## strip out the minimal information to avoid huge objects inside mclapply
  ## - thus we keep just the basic forest, variable names, factor levels
  ## - we separately store the x-var information
  ##
  ##-------------------------------------------------------------------------
  ## extract xvar.names and factor information
  xvar.names <- object$forest$xvar.names
  xvar.factor <- object$forest$xvar.factor
  ## extract the data and process it
  ## missing data not allowed
  ## convert the data to numeric mode, apply the na.action protocol
  xvar <- object$forest$xvar
  if (any(is.na(xvar))) {
    stop("missing data not allowed")
  }
  xvar <- randomForestSRC:::finalizeData(xvar.names, xvar, miss.flag = FALSE)
  ## pull the forest arrays
  arr <- object$forest$nativeArray
  arrf <- object$forest$nativeFactorArray[[1]]
  ## subset the forest arrays
  if (!is.null(ntreeSeq)) {
    pt.arr <- arr$treeID %in% ntreeSeq 
    ptf.arr <- arr$mwcpSZ != 0 
    arr <- arr[pt.arr,, drop = FALSE]
    if (sum(ptf.arr & pt.arr) > 0) {
      arrf <- arrf[pt.arr[ptf.arr]] 
    }
    else {
      arrf <- NULL
    }  
  }
  ## identify the trees to be used
  ntreeSeq <- sort(unique(arr$treeID))
  ## now reduce the object to the minimal information
  object <- list(xvar.names = xvar.names,
                 xvar.factor = xvar.factor,
                 native.array = arr,
                 native.f.array = arrf)
  ##------------------------------------------------------------------------
  ##
  ##
  ## MAIN CALL MAIN CALL MAIN CALL MAIN CALL MAIN CALL
  ##
  ## repeated call to getTree.workhorse
  ## we pass only minimal information to the workhorse
  ##
  ##
  ##-------------------------------------------------------------------------
  gc() ### <------------------  memory management for mclapply
  treeRuleO <- papply(ntreeSeq, function(b) {
    getTreeRule.workhorse(object, treeID = b)
  })
  ##------------------------------------------------------------------------
  ##
  ## extract the tree rules
  ##
  ##-------------------------------------------------------------------------  
  treeRule <- unlist(lapply(treeRuleO, function(oo) {oo$treeRule}), recursive = FALSE)
  if (!is.null(treeRule)) {## convert list of lists to a list
    treeRule <- lapply(treeRule, function(oo) {unlist(oo)})
  }
  ##------------------------------------------------------------------------
  ##
  ## extract variables used
  ##
  ##-------------------------------------------------------------------------  
  varUsed <- unlist(lapply(treeRuleO, function(oo) {oo$varUsed}), recursive = FALSE)
  varNames <- unlist(lapply(treeRuleO, function(oo) {oo$varNames}), recursive = FALSE)
  ##------------------------------------------------------------------------
  ##
  ## tree id
  ##
  ##-------------------------------------------------------------------------  
  treeID <- unlist(lapply(1:length(treeRuleO), function(j) {rep(j, length(treeRuleO[[j]]$treeRule))}))
  rm(treeRuleO)
  gc() ### <------------------  memory management for mclapply
  ##------------------------------------------------------------------------
  ##
  ## did getTree fail?
  ##
  ##-------------------------------------------------------------------------  
  ## return NULL if the extract objects are NULL (ie object was stumped)
  if (is.null(treeRule) & is.null(varUsed)) {
    return(NULL)
  }
  ##------------------------------------------------------------------------
  ##
  ## return the goodies (getTree was succesful)
  ##
  ##-------------------------------------------------------------------------  
  rO <- list(treeRule = treeRule,
             varUsed = varUsed,
             varNames = varNames,
             treeID = treeID)
  class(rO) <- "getTreeRule"
  rm(treeRule)
  gc() ### <------------------  memory management for mclapply
  rO
}
###  tree rule WORK HORSE (heavy lifting/computations are actually done here)
getTreeRule.workhorse <- function(object, treeID, tolerance = sqrt(.Machine$double.eps)) {
  ##----------------------------------------------------------------
  ##
  ## The following two utilities were copied from the BMS CRAN
  ## package.  Thanks to Martin Feldkircher and Stefan Zeugne for
  ## these little quickies.
  ##
  ##----------------------------------------------------------------
  hex2bin <- function (hexcode) {
      if (!is.character(hexcode)) 
          stop("please input a character like '0af34c'")
      hexcode <- paste("0", tolower(hexcode), sep = "")
      hexobj <- .hexcode.binvec.convert(length(hexcode) * 16L)
      return(hexobj$as.binvec(hexcode))
  }
  .hexcode.binvec.convert <- function (length.of.binvec) {
      if (length(length.of.binvec) > 1) 
          length.of.binvec = length(length.of.binvec)
      addpositions = 4 - length.of.binvec%%4
      positionsby4 = (length.of.binvec + addpositions)/4
      hexvec = c(0:9, "a", "b", "c", "d", "e", "f")
      hexcodelist = list(`0` = numeric(4),
                         `1` = c(0, 0, 0, 1), 
                         `2` = c(0, 0, 1, 0),
                         `3` = c(0, 0, 1, 1),
                         `4` = c(0, 1, 0, 0),
                         `5` = c(0, 1, 0, 1),
                         `6` = c(0, 1, 1, 0), 
                         `7` = c(0, 1, 1, 1),
                         `8` = c(1, 0, 0, 0),
                         `9` = c(1, 0, 0, 1),
                         a   = c(1, 0, 1, 0),
                         b   = c(1, 0, 1, 1),
                         c   = c(1, 1, 0, 0),
                         d   = c(1, 1, 0, 1),
                         e   = c(1, 1, 1, 0),
                         f   = c(1, 1, 1, 1))
      return(list(
          as.hexcode = function(binvec) {
              incl = c(numeric(addpositions), binvec)
              dim(incl) = c(4, positionsby4)
              return(paste(hexvec[crossprod(incl, 2L^(3:0)) + 1], collapse = ""))
          },
          as.binvec = function(hexcode) {
              return(unlist(
                  hexcodelist[unlist(strsplit(hexcode, "", fixed = TRUE),
                                     recursive = FALSE, use.names = FALSE)], 
                  recursive = FALSE, use.names = FALSE)[-(1:addpositions)])
          }))
  }
  ##----------------------------------------------------------------
  ##
  ## pull object information (tree topology + other minimal info)
  ##
  ##----------------------------------------------------------------
  ## pull xvar.names
  xvar.names <- object$xvar.names
  xvar.factor <- object$xvar.factor
  ## pull the arrays
  native.array <- object$native.array
  native.f.array <- object$native.f.array
  ## added processing needed for factors
  f.ctr <- 0
  factor.flag <- FALSE
  if (!is.null(native.f.array)) {
    pt.f <- which(native.array$mwcpSZ != 0)
    factPT <- lapply(pt.f, function(j) {
      f.ctr <<- f.ctr + 1
      step <- native.array$mwcpSZ[j] - 1
      mwcpPT <- native.f.array[f.ctr:(f.ctr+step)]
      mwcpPT <- paste0(sapply(mwcpPT, function(mwc) {
        format(as.hexmode(mwcpPT), 8)
      }))      
      mwcpSZ <- hex2bin(mwcpPT)
      paste(mwcpSZ, collapse = "")
    })
    native.array$contPT[pt.f] <- factPT
    factor.flag <- TRUE
  }
  ## define the display tree
  display.tree <- native.array[native.array$treeID == treeID,, drop = FALSE]
  ## return NULL if the display tree is a stump
  if (nrow(display.tree) == 1) {
    return(NULL)
  }
  ## check to see if any factors are left
  ## store relevant information for later split-inequality encodings
  if (factor.flag) {
    pt.f <- display.tree$mwcpSZ !=0
    if (sum(pt.f) > 0) {
      f.names <- unique(xvar.names[display.tree$parmID[pt.f]])
    }
    else {
      factor.flag <- FALSE
    }
  }
  ##----------------------------------------------------------------
  ##
  ## prepare the tree to be converted into a network
  ##
  ##----------------------------------------------------------------
  ## conversion
  converted.tree <- display.tree
  vars.id <- data.frame(var = c("<leaf>", xvar.names), parmID = 0:length(xvar.names), stringsAsFactors = FALSE)
  converted.tree$var <- vars.id$var[match(display.tree$parmID, vars.id$parmID)]
  ## special symbol to be used for encoding the counter for variables (see note below)
  special <- "999_999"
  # note: we append a counter to the variables, because the data.tree package has trouble when
  # nodes are not unique.
  var.count <- 1:nrow(converted.tree)
  lapply(unique(converted.tree$var), function(vv) {
    pt <- converted.tree$var == vv
    var.count[which(pt)] <<- 1:sum(pt)
  })
  converted.tree$var_count <- var.count
  converted.tree$var_conc <- paste0(converted.tree$var, special, converted.tree$var_count)
  ##----------------------------------------------------------------
  ##
  ## convert the tree to a network data frame, using the fact that the
  ## nativeArray output is a pre-order traversal 
  ##
  ##----------------------------------------------------------------
  ## preliminary
  from_node <- ""
  network <- data.frame()
  num.children <- data.frame(converted.tree, children = 0)
  num.children <- num.children[num.children$var != "<leaf>",, drop = FALSE]
  num.children <- num.children[!duplicated(num.children$var_conc),, drop = FALSE]
  num_children <- as.list(rep(0, nrow(num.children)))
  names(num_children) <- num.children$var_conc
  ## loop (using lapply)
  lapply(1:nrow(converted.tree), function(i) {
    rowi <- converted.tree[i, ]
    xs <- converted.tree$contPT[converted.tree$var_conc == from_node]
    if (i == 1){
      from_node <<- rowi$var_conc
    }
    else{
      ## develop the split encoding
      if (num_children[[from_node]] == 0) {#left split
        side <- "<="
        contPT.pretty <- round(as.numeric(xs, 3))
        split_ineq_pretty <- paste0(side, contPT.pretty)
      }
      else {#right split
        side <- ">"
        split_ineq_pretty <- ""
      }
      ## both numeric and factors are encoded as <= > but factors are secretely in hex notation
      ## !!!!!!!!!!!! ADD MACHINE TOLERANCE TO SPLIT VALUE TO FIX WHEN SPLIT IS ON ACTUAL X-VALUE !!!!!!!!!!!!!!!
      if (is.numeric(xs)) {
        xs <- xs + tolerance
      }
      split_ineq <- paste0(side, xs)
      ## update the network
      to_node <- rowi$var_conc
      new_node <- list(from = from_node, to = to_node, split = split_ineq, split.pretty = split_ineq_pretty)
      network <<- data.frame(rbind(network, new_node, stringsAsFactors = FALSE))
      num_children[[from_node]] <<- num_children[[from_node]] + 1
      if(rowi$var != "<leaf>")
        from_node <<- to_node
      else{
        if(i != nrow(converted.tree)){
          while(num_children[[from_node]] == 2){
            from_node <<- network$from[network$to == from_node]
          }
        }
      }
    }
  })
  ##----------------------------------------------------------------
  ##
  ## process network for factors - clean up the split encoding
  ## encode as complementary pair string
  ##
  ##----------------------------------------------------------------
  if (factor.flag) {
    ## identify which splits need to be cleaned up
    from.names <- network$from
    pt.f <- sort(unique(unlist(lapply(f.names, function(ptrn) {
      grep(ptrn, from.names)
    }))))
    ## identify levels of the factor
    ## otherwise we would have huge sets full of levels that aren't used
    fs <- gsub(paste0(special,".*"),"",from.names[pt.f])
    fs.levels <- sapply(fs, function(fsn) {
      #length(levels(x.org.data[, fsn]))
      xvar.factor$nlevels[which(fsn == xvar.names)]
    })
    ## clean the splits up and encode as complementary pair sets
    split.str <- lapply(1:length(pt.f), function(j) {
      str <- network$split[pt.f[j]]
      ## left split
      if (grepl("<=", str)) {
        str <- sub("<=", "", str)
        str <- strsplit(str, "")[[1]]
        cpr <- 1 + length(str) - which(str != "0")
        cpr <- cpr[cpr <= fs.levels[j]]
        paste0("{", paste(cpr, collapse = ","), "}")
      }
      ## right split
      else {
        str <- sub(">", "", str)
        str <- strsplit(str, "")[[1]]
        cpr <- 1 + length(str) - which(str == "0")
        cpr <- cpr[cpr <= fs.levels[j]]
        paste0("{", paste(cpr, collapse = ","), "}")
      }
    })
    ## pretty complementary pairs
    split.str.pretty <- lapply(1:length(pt.f), function(j) {
      str <- network$split[pt.f[j]]
      ## left split
      if (grepl("<=", str)) {
        str <- sub("<=", "", str)
        str <- strsplit(str, "")[[1]]
        cpr <- 1 + length(str) - which(str != "0")
        cpr <- cpr[cpr <= fs.levels[j]]
        paste0("{", paste(cpr, collapse = ","), "}")
      }
      else {
        ""
      }
    })
    ## replace the previous fake encoding with the now correct "set encoding"
    network$split[pt.f] <- split.str
    network$split.pretty[pt.f] <- split.str.pretty
  }
  ##----------------------------------------------------------------
  ##
  ## pull the decision rule for a given leaf
  ##
  ##----------------------------------------------------------------
  ## set up the data tree network
  data.tree.network <- data.tree::FromDataFrameNetwork(network, "split")
  ## set up the list and counters for storing the decision rule
  ctr <- 0
  treerule <- varUsed <- varNames <- list()
  ## loop through the leaves of the tree, each time getting the path down to the leaf, and using this path
  ## to establish a filtering condition to obtain all cases that match the splitting on the way to this leaf.
  ## this provides us with the cases for a leaf and the leaf itself
  lapply(data.tree.network$leaves, function(node) {
    ## pull relevant information
    path_list <- node$path
    var_list <- sapply(path_list, function(x){strsplit(x, special)[[1]][1]})
    var_list[length(var_list)] <- ""
    node_iter <- data.tree.network
    ## make boolean string operator - save the list of variable names
    varnames <- NULL
    call <- lapply(2:(length(path_list)), function(i) {
      node_iter <<- node_iter[[path_list[[i]]]]
      str <- node_iter$split
      varnames <<- c(varnames, var_list[i-1])
      ## numeric boolean operator
      if (!any(grepl("\\{", str))) {
        str <- paste0("", paste0(var_list[i-1], str))
      }
      ## complementary pair boolean operator
      else {
        str <- gsub("\\{", "", str)
        str <- gsub("\\}", "", str)
        str <- strsplit(str, ",")[[1]]
        str <- paste("==", str, sep = "")
        str <- paste0("(",paste(paste0("", var_list[i-1], str), collapse = "|"),")")
      }
      str
    })
    names(varnames) <- NULL
    ## update the counter and save the results
    ctr <<- ctr + 1
    treerule[[ctr]] <<- call
    varUsed[[ctr]] <<- sort(unique(varnames))
    varNames[[ctr]] <<- varnames
  })
  ## return the desired goodies
  list(treeRule = treerule, varUsed = varUsed, varNames = varNames)
}
## parse rule and assemble it as a character string
parseRule <- function(rule) {
  anyC <- grepl("\\(", rule)
  if (sum(anyC) == 0) {
    paste0("x$", rule, collapse=" & ")
  }
  else {
    paste0(unlist(sapply(1:length(rule), function(j) {
      rj <- rule[j]
      if (anyC[j]) {
        rj <- sub("\\(", "", rj)
        rj <- sub("\\)", "", rj)
        rj <- strsplit(rj, "\\|")[[1]]
        paste0("(", paste0(unlist(lapply(rj, function(rr) {
          paste0("x$", rr)
        })), collapse = " | "), ")") 
      }
      else {
        paste0("x$", rj, collapse = " & ")
      }
    })), collapse = " & ")
  }
}
