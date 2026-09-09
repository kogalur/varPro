predict.ivarpro <- function(object,
                            newdata = NULL,
                            model = NULL,
                            noise.na = NULL,
                            path.store.membership = FALSE,
                            save.data = TRUE,
                            ...) {
  ## ------------------------------------------------------------
  ## Helpers: resolve common path + model from object (supports compact multivariate output)
  ## ------------------------------------------------------------
  .is_list_out <- function(x) {
    is.list(x) && !inherits(x, "data.frame")
  }
  .has_rule_meta <- function(p) {
    !is.null(p) &&
      !is.null(p$rule.tree) &&
      !is.null(p$rule.branch) &&
      !is.null(p$rule.variable)
  }
  .get_common_path <- function(obj) {
    if (.is_list_out(obj)) {
      ## New compact style: common path stored on the list
      p_common <- attr(obj, "ivarpro.path")
      if (.has_rule_meta(p_common)) return(p_common)
      ## Legacy style: common path stored on each element
      if (length(obj) == 0L) stop("ivarpro object is an empty list.")
      p0 <- attr(obj[[1]], "ivarpro.path")
      if (.has_rule_meta(p0)) return(p0)
      stop("Missing rule metadata in ivarpro.path (rule.tree/branch/variable).")
    } else {
      p0 <- attr(obj, "ivarpro.path")
      if (.has_rule_meta(p0)) return(p0)
      stop("Missing 'ivarpro.path' attribute (object not from current ivarpro()).")
    }
  }
  .get_model <- function(obj) {
    m0 <- attr(obj, "model")
    if (!is.null(m0)) return(m0)
    if (.is_list_out(obj)) {
      m1 <- attr(obj[[1]], "model")
      if (!is.null(m1)) return(m1)
    }
    NULL
  }
  .get_rule_imp <- function(obj_j) {
    p <- attr(obj_j, "ivarpro.path")
    if (!is.null(p) && !is.null(p$rule.imp)) return(p$rule.imp)
    NULL
  }
  ## ------------------------------------------------------------
  ## Resolve common path + model
  ## ------------------------------------------------------------
  path0 <- .get_common_path(object)
  input.is.prediction <- isTRUE(path0$prediction)
  if (is.null(model)) model <- .get_model(object)
  if (is.null(model)) {
    stop("ivarpro prediction requires the original model; use save.model = TRUE ",
         "in ivarpro() or supply model to predict().")
  }
  ## Noise behavior default: inherit from path if not supplied
  if (is.null(noise.na)) {
    if (!is.null(path0$noise.na)) noise.na <- isTRUE(path0$noise.na) else noise.na <- TRUE
  }
  flags <- list(noise.na = noise.na,
                path.store.membership = path.store.membership,
                save.data = save.data)
  for (nn in names(flags)) {
    z <- flags[[nn]]
    if (!is.logical(z) || length(z) != 1L || is.na(z)) {
      stop(nn, " must be TRUE or FALSE")
    }
  }
  xnames <- path0$xvar.names
  if (!is.character(xnames) || !length(xnames) ||
      anyNA(xnames) || any(!nzchar(xnames)) || anyDuplicated(xnames)) {
    stop("ivarpro.path must contain unique, nonempty predictor names")
  }
  for (nn in c("rule.tree", "rule.branch", "rule.variable")) {
    z <- path0[[nn]]
    if (!is.numeric(z) || any(!is.finite(z)) ||
        any(z < 1 | z != floor(z))) {
      stop("Invalid ", nn, " in ivarpro.path")
    }
  }
  rule.tree   <- as.integer(path0$rule.tree)
  rule.branch <- as.integer(path0$rule.branch)
  rule.var    <- as.integer(path0$rule.variable)
  if (any(rule.var > length(xnames))) {
    stop("Rule variable indices exceed the predictor count")
  }
  R <- length(rule.tree)
  if (R == 0L) stop("No rules found in ivarpro.path (rule.tree length is 0).")
  if (length(rule.branch) != R || length(rule.var) != R) {
    stop("Inconsistent rule metadata lengths in ivarpro.path.")
  }
  ## ------------------------------------------------------------
  ## Prepare rf + (optionally) newx
  ## ------------------------------------------------------------
  rf <- NULL
  if (!is.null(newdata) && !is.data.frame(newdata)) newdata <- as.data.frame(newdata)
  new.rows <- if (!is.null(newdata)) rownames(newdata) else NULL
  if (inherits(model, "varpro")) {
    if (!is.null(newdata) && isTRUE(attr(model$x, "hotencode"))) {
      ## Use the complete training encoding, as in predict.varpro().
      newdata <- get.hotencode.test(model$x, newdata)
      if (nrow(newdata) != length(new.rows)) {
        stop("Hot-encoding changed the number of rows in newdata")
      }
      rownames(newdata) <- new.rows
    }
    rf <- model$rf
  } else if (inherits(model, "rfsrc") && inherits(model, "grow")) {
    rf <- model
  } else {
    stop("model must be either a 'varpro' object or an 'rfsrc' grow object.")
  }
  ## ------------------------------------------------------------
  ## Build membership list for prediction cases
  ## ------------------------------------------------------------
  memb_list <- NULL
  n_total   <- NULL
  case.names <- NULL
  training.x <- if (inherits(model, "varpro")) model$x else rf$xvar
  ## Prefer case labels when available; unnamed, full-size membership
  ## matrices retain the forest's usual input-row ordering.
  .membership_rows <- function(memb, case.names) {
    if (!is.matrix(memb)) stop("Forest membership must be a matrix")
    rn <- rownames(memb)
    if (!is.null(rn) && !anyDuplicated(rn) &&
        all(rn %in% case.names)) {
      return(match(rn, case.names))
    }
    if (nrow(memb) == length(case.names)) return(seq_along(case.names))
    stop("Could not align membership rows with the prediction cases")
  }
  if (!is.null(newdata)) {
    if (!all(xnames %in% colnames(newdata))) {
      missing_cols <- setdiff(xnames, colnames(newdata))
      stop("newdata is missing required predictors: ", paste(missing_cols, collapse = ", "))
    }
    newx <- newdata[, xnames, drop = FALSE]
    n_total <- nrow(newx)
    case.names <- rownames(newx)
    if (n_total < 1L) stop("newdata must contain at least one row")
    pr <- predict.rfsrc(
      rf,
      newx,
      membership = TRUE,
      perf.type  = "none",
      ...
    )
    memb <- pr$membership
    if (is.null(memb)) stop("predict.rfsrc did not return a membership matrix (membership=TRUE).")
    row_map <- .membership_rows(memb, case.names)
    tree.id <- sort(unique(rule.tree))
    if (max(tree.id) > ncol(memb)) stop("Rule tree indices exceed membership matrix trees.")
    memb_used <- memb[, tree.id, drop = FALSE]
    tree.pos  <- match(rule.tree, tree.id)
    memb_list <- vector("list", R)
    for (tt in seq_along(tree.id)) {
      rules_tt <- which(tree.pos == tt)
      if (!length(rules_tt)) next
      nodes_tt <- memb_used[, tt]
      idx_by_node <- split(row_map, nodes_tt)
      for (r in rules_tt) {
        b <- as.character(rule.branch[r])
        idx <- idx_by_node[[b]]
        memb_list[[r]] <- if (is.null(idx)) integer(0) else as.integer(idx)
      }
    }
  } else {
    ## newdata = NULL always restores the original training OOB scores.
    ## Memberships saved on a test prediction refer to different cases.
    if (!input.is.prediction && !is.null(path0$oobMembership)) {
      memb_list <- path0$oobMembership
      if (.is_list_out(object)) {
        n_total <- nrow(object[[1]])
        case.names <- rownames(object[[1]])
      } else {
        n_total <- nrow(object)
        case.names <- rownames(object)
      }
    } else {
      ## reconstruct OOB from restore mode using inbag == 0
      pr <- predict.rfsrc(
        rf,
        membership = TRUE,
        perf.type  = "none",
        ...
      )
      memb  <- pr$membership
      inbag <- pr$inbag
      if (is.null(memb))  stop("predict.rfsrc did not return membership (membership=TRUE).")
      if (is.null(inbag)) stop("predict.rfsrc did not return inbag in restore mode; cannot rebuild OOB.")
      if (!is.matrix(memb) || !is.matrix(inbag) ||
          !identical(dim(memb), dim(inbag))) {
        stop("Forest membership and inbag matrices must have matching dimensions")
      }
      case.names <- rownames(training.x)
      if (is.null(case.names)) case.names <- as.character(seq_len(nrow(memb)))
      n_total <- length(case.names)
      row_map <- .membership_rows(memb, case.names)
      tree.id <- sort(unique(rule.tree))
      if (max(tree.id) > ncol(memb)) stop("Rule tree indices exceed membership matrix trees.")
      memb_used  <- memb[,  tree.id, drop = FALSE]
      inbag_used <- inbag[, tree.id, drop = FALSE]
      tree.pos   <- match(rule.tree, tree.id)
      memb_list <- vector("list", R)
      for (tt in seq_along(tree.id)) {
        rules_tt <- which(tree.pos == tt)
        if (!length(rules_tt)) next
        oob_rows <- which(inbag_used[, tt] == 0)
        if (!length(oob_rows)) next
        nodes_oob <- memb_used[oob_rows, tt]
        idx_by_node <- split(row_map[oob_rows], nodes_oob)
        for (r in rules_tt) {
          b <- as.character(rule.branch[r])
          idx <- idx_by_node[[b]]
          memb_list[[r]] <- if (is.null(idx)) integer(0) else as.integer(idx)
        }
      }
    }
  }
  if (!is.list(memb_list) || length(memb_list) != R) {
    stop("Memberships must contain one entry per retained rule")
  }
  for (idx in memb_list) {
    if (length(idx) && (!is.numeric(idx) || any(!is.finite(idx)) ||
        any(idx < 1 | idx > n_total | idx != floor(idx)))) {
      stop("Stored memberships do not match the prediction cases")
    }
  }
  if (is.null(case.names)) case.names <- as.character(seq_len(n_total))
  ## Store shared memberships only once for multi-target output. Rule
  ## diagnostics continue to describe the original training estimates.
  .prediction_path <- function(path) {
    if (.has_rule_meta(path)) {
      path$noise.na <- noise.na
      path$prediction <- !is.null(newdata)
      path$oobMembership <- if (isTRUE(path.store.membership)) memb_list else NULL
      if (!isTRUE(path.store.membership) || !is.null(newdata) || input.is.prediction) {
        path$compMembership <- NULL
      }
    } else {
      path$oobMembership <- path$compMembership <- NULL
    }
    path
  }
  ## ------------------------------------------------------------
  ## Aggregate to case x variable gradients using workhorse
  ## ------------------------------------------------------------
  .predict_one <- function(rule_imp, path_spec = NULL) {
    if (is.null(rule_imp)) stop("Missing rule.imp in ivarpro.path (required for prediction).")
    if (length(rule_imp) != R) stop("Length of rule.imp does not match number of rules.")
    ## A failed local estimate may have been stored as zero when the
    ## original analysis used noise.na = FALSE. Its missing slope identifies
    ## it as unavailable without confusing a valid zero effect with failure.
    source.path <- if (is.null(path_spec)) path0 else path_spec
    slope <- source.path$rule.slope
    if (!is.null(slope)) {
      if (length(slope) != R) stop("Length of rule.slope does not match the rules")
      rule_imp[!is.finite(slope)] <- NA_real_
    }
    csO <- list(
      results       = data.frame(variable = rule.var, imp = as.numeric(rule_imp)),
      xvar.names    = xnames,
      oobMembership = memb_list,
      n             = n_total
    )
    out_j <- csimp.varpro.workhorse(csO, noise.na = noise.na)
    rownames(out_j) <- case.names
    ## Retain the selected-rule diagnostics needed for further prediction.
    if (is.null(path_spec)) {
      path_pred <- path0
    } else {
      path_pred <- path_spec
    }
    attr(out_j, "ivarpro.path") <- .prediction_path(path_pred)
    out_j
  }
  .finish_prediction <- function(out) {
    if (isTRUE(save.data)) {
      if (!is.null(newdata)) {
        saved <- data.frame(newx, check.names = FALSE)
      } else if (!input.is.prediction && !is.null(attr(object, "data"))) {
        saved <- attr(object, "data")
      } else {
        saved <- data.frame(training.x[, xnames, drop = FALSE], check.names = FALSE)
      }
      if (nrow(saved) != n_total) {
        stop("Stored plotting data do not match the prediction cases")
      }
      rownames(saved) <- case.names
      attr(out, "data") <- saved
    }
    attr(out, "target") <- attr(object, "target", exact = TRUE)
    attr(out, "model") <- model
    class(out) <- unique(c("ivarpro", class(out)))
    out
  }
  ## ------------------------------------------------------------
  ## Return object in same shape as input
  ## ------------------------------------------------------------
  if (.is_list_out(object)) {
    ## Detect compact style input: common path on list + per-response rule.imp in element paths
    input_common <- attr(object, "ivarpro.path")
    compact_in <- .has_rule_meta(input_common)
    out <- vector("list", length(object))
    names(out) <- names(object)
    if (compact_in) {
      ## Predict each response using element-specific rule.imp
      for (j in seq_along(object)) {
        rule_imp_j <- .get_rule_imp(object[[j]])
        path_spec  <- attr(object[[j]], "ivarpro.path")
        out[[j]] <- .predict_one(rule_imp_j, path_spec = path_spec)
      }
      ## Attach common path to list-level for compact output
      attr(out, "ivarpro.path") <- .prediction_path(input_common)
    } else {
      ## Legacy input: each element carries full path, including rule.imp
      for (j in seq_along(object)) {
        path_j <- attr(object[[j]], "ivarpro.path")
        if (is.null(path_j)) stop("One element of the ivarpro list is missing 'ivarpro.path'.")
        out[[j]] <- .predict_one(path_j$rule.imp, path_spec = path_j)
      }
    }
    .finish_prediction(out)
  } else {
    ## Single-output object
    out <- .predict_one(path0$rule.imp, path_spec = path0)
    .finish_prediction(out)
  }
}
