# varPro 3.2.2

## New features

* `cv.varpro()` gains optional outer cross-validation through `cv.folds` and
  `foldid`, with held-out prediction errors, selection frequencies, and
  importance-stability summaries for all three selection rules. The default,
  `cv.folds = 0`, retains OOB-based selection without outer cross-validation.
* `varpro()` objects now include `model.info`, recording original and working
  responses, survival targets and RMST horizons, class-label mappings, and
  effective forest settings. `model.info$observations` also records input,
  retained, and omitted row counts, including in `split.weight.only`
  results (#7).

## Interface changes

* Removed iVarPro cut-ladder storage and plotting bands. `plot()` for iVarPro
  objects no longer supports `ladder`, `ladder.cuts`, or `ladder.max.segments`;
  remove these arguments from existing calls.
* `varpro()`, `partialpro()`, `rf.learner()`, `gbm.learner()`, and
  `bart.learner()` now reject unnamed options, unrecognized names, and
  duplicate names in `...` (#7). This includes unsupported `na.action`
  in `varpro()` and `RMST`, `rmst`, and `time` in `partialpro()`.
* `rf.learner()` now rejects user-supplied `formula`, `data`, `xvar.wt`,
  and `perf.type`, which the wrapper sets internally (#7).

## Bug fixes and refinements

* Importance summaries now map encoded columns to original predictors exactly,
  avoiding substring and regular-expression name collisions and incorrect
  pooling across responses. `get.orgvimp()` now reuses a supplied `vmp` summary.
* Fixed multivariate importance-column handling and CV importance extraction
  for empty selections and forced one-variable models.
* Improved hot-encoding of single-level factors and factors with nonsyntactic
  names. Internal survival-response names now avoid predictor-name collisions.
* Fixed `ivarpro()` to honor `scale = "global"` and `scale = "none"` throughout
  neighborhood searches and exclude invalid local fits with `use.loo = FALSE`.
* Corrected multivariate VarPro OOB-prediction extraction in `ivarpro()` and
  preserved all columns of user-supplied `y.external` responses.
* Fixed iVarPro prediction to reuse training hot-encoding, preserve row
  alignment, honor `noise.na` overrides, and restore training OOB scores when
  `predict()` is called without `newdata`, including on a prediction result.
* Improved iVarPro plotting with a single retained predictor, numeric gradient
  matrices, and plotting data containing unrelated nonnumeric columns.
* Corrected survival CRPS evaluation to use the evaluation outcomes and
  matching censoring predictions, including in the fast fitting path.
  Evaluation with `nblocks = 1` now respects supplied held-out data.
* Improved `cv.varpro()` input validation, cutoff ordering, and handling of
  unavailable prediction errors, with warnings when no candidate error is finite.
* Applied RFQ imbalance controls consistently in the rule-generating forest
  and cross-validation candidate forests.
* Fixed `get.rmst()` to use full-ensemble survival estimates with an OOB
  fallback, validate time horizons, and preserve matrix dimensions for small
  inputs and response identities for multiple horizons. The integral now
  uses survival at each interval's left endpoint (#7).
* Fixed `varpro.strength(..., stat = "oob")` to return `meanOOB` for regression.
* Fixed `sdependent()` to align importance-matrix rows and columns by variable
  name before clearing self-links, fill missing release rows with zeros, and
  validate matrix values and names.
* `varpro()` now warns when preprocessing omits observations with missing
  values, reporting the input, omitted, and retained counts (#7).

## Documentation

* Expanded documentation and examples for VarPro selection, cross-validation,
  iVarPro fitting and prediction, outPro scoring and calibration, and UVarPro
  local lasso analysis and dependence graphs.
* Documented `shap.ivarpro()` with the iVarPro plotting methods and
  `get.beta.entropy()` and `sdependent()` with `uvarpro()`.
* Documented validation of additional arguments, supported learner controls,
  and observation counts (#7).

# varPro 3.2.0

## New features

* `partialpro()` gains a new `vt.filter` argument for selecting the virtual-twin filtering engine.  The default, `vt.filter = "isopro"`, preserves the existing isolation-forest filtering behavior.  New alternatives are `vt.filter = "outpro"`, which uses `outpro`-based out-of-distribution support, and `vt.filter = "none"`, which disables VT filtering.
* Added `outpro`-based VT filtering to `partialpro()`.  For `vt.filter = "outpro"`, virtual twins are scored by an `outpro` distance, calibrated against an `outpro.null()` reference distribution, and converted to a support score.  The existing `cut` option is retained: larger values require stronger support and `cut = 0` disables VT filtering.
* Added `distancef = "knn"` to `outpro()`.  The KNN distance is computed in the standardized selected predictor subspace and provides a faster option for large prediction or virtual-twin grids because it does not require the forest-neighborhood distance construction.
* The `outpro` VT filter in `partialpro()` uses KNN distance by default through the hidden option `out.distancef = "knn"`.  Additional advanced controls are available through `...`, including `out.neighbor`, `out.reduce`, `out.cutoff`, `out.max.rules.tree`, `out.max.tree`, `out.knn.chunk.size`, and `out.null`.
* `outpro()` now supports `newdata.xscale`, allowing package-internal callers to pass new data that are already aligned to the fitted VarPro x-scale.  This is useful for functions such as `partialpro()`, where virtual data are constructed directly from the stored VarPro design matrix.
* `outpro.null()` now supports `nulldata.xscale`, providing the corresponding x-scale option for null/reference data.

## Documentation
* Expanded the `partialpro()` help file with a fuller description of the case-local partial-profile method, virtual-twin filtering, local polynomial smoothing, classification log-odds handling, binary-variable handling, and advanced options passed through `...`.
* Expanded the `outpro()` documentation to describe the KNN distance option and the x-scale handling used by package-internal calls.

## Bug fixes and refinements
* Fixed hidden-option parsing in `partialpro()` so that `nodesize` is read from `nodesize`, not from `ntree`.
* `outpro.null()` now uses `cutoff = NULL` by default, matching the main `outpro()` cutoff-selection rule and keeping null calibration consistent with ordinary `outpro()` calls.

# varPro 3.1.0

## Breaking changes

- `importance()` is now a true S3 generic rather than an alias-style front end.
- `partial.ivarpro()` has been replaced by `plot.ivarpro()`.
- The supported user-facing interfaces for fitted objects are now the corresponding S3 generics, such as `importance()`, `predict()`, and `plot()`.

## S3 interface cleanup

- Registered `importance()` methods for `"varpro"` and `"uvarpro"` objects.
- Registered `plot()` methods for `"ivarpro"` and `"partialpro"` objects.
- Continued support for class-specific `predict()` methods through standard S3 dispatch for `"varpro"`, `"uvarpro"`, `"ivarpro"`, and `"isopro"` objects.

## Documentation

- Help topics retain dotted method names such as `plot.ivarpro`, `plot.partialpro`, `predict.ivarpro`, `predict.varpro`, `predict.uvarpro`, and `predict.isopro` so that method pages remain easy to find in the reference manual and via `?topic`.
- Usage sections were updated to show S3 method signatures consistently, for example `\method{plot}{ivarpro}(x, ...)` and `\method{predict}{ivarpro}(object, ...)`.
- Examples were updated to use the generic forms `plot(x, ...)`, `predict(object, ...)`, and `importance(object)`.
- The iVarPro plotting documentation now uses `data` for the original feature matrix and documents `target` explicitly for multivariate and multiclass outputs.

## Migration notes

- Replace calls of the form `partial.ivarpro(iv, var = ...)` with `plot(iv, var = ...)`.
- Prefer `importance(fit)` over direct calls to `importance.varpro(fit)`.
- Prefer `predict(fit, ...)` over direct calls to `predict.class(fit, ...)`.

# varPro 3.0.0

## Improvements

* Refactored `varpro.strength()` to reduce R-side post-processing overhead after the native `varProStrength` call, improving performance on large forests and large membership reconstructions.
* Improved scalability and stability of `varpro.strength(..., membership = TRUE)` for very large analyses.
* For RHF grow objects, `varpro.strength()` now uses the integrated hazard exposure values stored on the fitted object (`int.haz.oob`) as the default working response when available.
* Internal cleanup of native-output decoding and membership reconstruction logic.

## Bug fixes

* Fixed a failure that could occur on very large analyses when rebuilding membership lists in R after native execution, which could previously surface as an integer-overflow warning from `cumsum()` followed by a downstream missing-value error in membership reconstruction.

# varPro 2.1.0

* Major refactoring and enhancement to functions downstream from the entry `varpro()` function.

# varPro 2.0.0

* Improved `ivarPro`.
* Refactored code to improve speed.
* Eliminated or replaced `mclapply()` with PSOCK-based parallel execution, improving Windows compatibility.

# varPro 1.0.1

* CRAN compliance fixes.

# varPro 1.0.0

* Initial release.
