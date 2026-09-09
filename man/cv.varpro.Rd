\name{cv.varpro}
\alias{cv.varpro}
\title{Select VarPro Predictors and Assess Selection by Cross-Validation}

\description{
  Selects predictors by comparing the prediction error of random
  forests built from candidate VarPro variable sets. Returns the
  minimum-error selection and conservative and liberal alternatives.
  Optional outer cross-validation assesses predictive performance
  and the stability of variable selection.
}

\usage{
cv.varpro(formula, data, nvar = 30, ntree = 150,
          local.std = TRUE, zcut = seq(0.1, 2, length = 50), nblocks = 10,
          split.weight = TRUE, split.weight.method = NULL, sparse = TRUE,
          nodesize = NULL, max.rules.tree = 150, max.tree = min(150, ntree),
          verbose = FALSE, seed = NULL,
          fast = FALSE, crps = FALSE,
          cv.folds = 0, foldid = NULL, ...)
}

\arguments{
  \item{formula}{Formula specifying the response and predictors. The
    supported outcome families are those of \code{varpro}: regression,
    multivariate regression, classification, and right-censored
    survival. See Details for multivariate scoring.}

  \item{data}{Training data frame. Incomplete observations are
    removed before ranking predictors and evaluating candidate models.}

  \item{nvar}{Maximum number of processed predictor columns retained
    by the preliminary split-weight screening in \code{varpro}.
    Candidate models are subsequently formed on the original-variable
    scale. Ignored by the screening step when computed and custom
    split-weights are both absent.}

  \item{ntree}{Number of trees in the VarPro rule forest, each
    candidate prediction forest, and each outer assessment forest.}

  \item{local.std}{Should locally standardized rule comparisons be
    used to calculate importance? See \code{\link{varpro}}.}

  \item{zcut}{Numeric vector of positive importance cutoffs.
    Predictors with \code{z >= zcut} enter a candidate model.
    Cutoffs are sorted into increasing order, and each distinct
    variable set is evaluated once.}

  \item{nblocks}{Positive integer controlling the subdivision of
    each candidate forest for estimating prediction error and its
    variation. Larger values give smaller subforests. Use
    \code{nblocks = 1} to evaluate the whole forest. See Details.}

  \item{split.weight}{Should preliminary split-weights guide the
    VarPro screening and rule generation? See \code{\link{varpro}}.}

  \item{split.weight.method}{Preliminary weighting methods passed
    to \code{varpro}, for example \code{c("lasso", "tree")}.
    The default chooses the combination automatically.}

  \item{sparse}{Should preliminary weighting concentrate more strongly
    on promising predictors? Passed to \code{varpro}.}

  \item{nodesize}{Minimum terminal node size for the VarPro
    forest and, with \code{fast = FALSE}, the candidate prediction
    forests. Chosen automatically when \code{NULL}. Fast candidate
    forests use the settings of \code{rfsrc.fast}.}

  \item{max.rules.tree, max.tree}{Rule-extraction limits passed to
    \code{varpro}. They determine the comparisons available for the
    importance ranking.}

  \item{verbose}{Should progress and candidate-model performance
    summaries be printed?}

  \item{seed}{Seed for VarPro, candidate forests, and optional outer
    cross-validation. Also call \code{set.seed} before
    \code{cv.varpro} to control all random sampling.}

  \item{fast}{Should candidate prediction forests use
    \code{randomForestSRC::rfsrc.fast}? This uses smaller per-tree
    samples and a shared holdout for evaluation when sufficient data
    are available. See Details.}

  \item{crps}{For survival, should candidate models be evaluated using
    a time-normalized integrated censoring-weighted Brier score?
    With \code{FALSE}, the forest's default concordance error is
    used. See Details.}

  \item{cv.folds}{Number of outer cross-validation folds. The default,
    zero, uses the blocked-OOB procedure alone. A value of at least
    two adds held-out performance and selection stability summaries
    for all three rules. Generated folds are stratified by response
    class or survival event status, as appropriate. The fold count
    is reduced with a warning when the data cannot support the
    requested number. This is separate from \code{nblocks} and the
    preliminary lasso's \code{nfolds} option.}

  \item{foldid}{Optional vector of fold labels, one per input data
    row, to supply the outer partition. Retained observations must
    have consecutive integer labels starting at one. Entries for
    omitted observations are ignored. Supplying \code{foldid} enables
    outer CV even when \code{cv.folds = 0}; a positive
    \code{cv.folds} must match the number of supplied folds.
    Use common labels to keep records from the same subject together.}

  \item{...}{Additional named arguments passed to \code{varpro},
    including \code{parallel}, \code{cores}, \code{nfolds},
    \code{rmst}, and external-forest controls. The \code{sampsize}
    argument also controls candidate-forest sampling. See Details.}
}

\details{
  \subsection{Variable ranking and selection}{
    VarPro importance summarizes response differences between rule
    regions and their near-miss sets; see \code{\link{varpro}}. At each
    cutoff in \code{zcut}, predictors with scores at or above the
    cutoff form a candidate variable set. Higher cutoffs retain
    fewer variables. A prediction forest is used to evaluate each
    distinct set.

    Selection is reported for the original variables. For a
    hot-encoded categorical predictor, its score is the largest
    importance among its encoded columns. Multivariate regression
    also takes the maximum across responses; classification uses
    overall importance. Prediction forests use the original outcome
    and predictors, so selecting a categorical variable retains
    all its levels.
  }

  \subsection{Prediction error and the three selection rules}{
    By default, candidate models are compared using out-of-bag
    predictions. Each forest is evaluated in blocks of trees,
    giving a mean prediction error and a standard deviation across
    blocks. The error path is used to choose an importance cutoff.
    With \code{nblocks = 1}, the whole forest is evaluated and the
    block deviation is zero.

    The three rules offer different balances between prediction
    accuracy and the number of selected variables:
    \describe{
      \item{Minimum error}{The set with the lowest mean error,
        returned in \code{imp}.}
      \item{Conservative}{The smallest set within the error tolerance
        whose error is also below 1, returned in \code{imp.conserve}.}
      \item{Liberal}{The largest set within the error tolerance,
        returned in \code{imp.liberal}.}
    }
    The tolerance is the minimum mean error plus the average block
    standard deviation across candidate models. This allows smaller
    and larger sets with similar predictive performance.

    In OOB-only mode, \code{imp.conserve} is \code{NULL} when no
    model meets the conservative criterion. With outer
    cross-validation, an empty selection uses the highest-ranked
    variable. If no candidate model has a finite error, a warning
    is issued and all three rules return the unfiltered ranking
    with cutoffs set to zero.
  }

  \subsection{Performance measures}{
    Lower error is better for every supported family.
    \describe{
      \item{Regression}{Mean squared prediction error divided by the
        response variance. Multivariate regression uses the first
        response for prediction error and combines responses for
        the importance ranking.}

      \item{Classification}{Normalized Brier prediction error. For
        imbalanced two-class data, RFQ uses geometric-mean error,
        \eqn{1 - \sqrt{\mathrm{sensitivity} \times \mathrm{specificity}}}{
          1 - sqrt(sensitivity * specificity)}. The controls
        \code{use.rfq} and \code{iratio.threshold} apply to both
        variable ranking and candidate-model evaluation.}

      \item{Survival, \code{crps = FALSE}}{Concordance error,
        \eqn{1-C}, from the survival forest.}

      \item{Survival, \code{crps = TRUE}}{A censoring-weighted
        integrated Brier score for survival probabilities, divided
        by the maximum evaluation time.}
    }
    Supplying \code{rmst} changes the survival summary used for the
    VarPro ranking. Prediction error is evaluated against the
    original survival outcome using concordance error or CRPS.
  }

  \subsection{Outer cross-validation}{
    Set \code{cv.folds = 5}, for example, to assess how well the
    complete variable-selection procedure predicts held-out
    observations. Variable selection and model training use the
    remaining folds. All three selection rules are assessed using
    the same outcome-specific scoring as the main analysis.

    The returned \code{imp}, \code{imp.conserve}, and
    \code{imp.liberal} tables contain the full-data selections and
    importance scores. Additional columns show how often each
    variable was selected and how its importance varied across
    training samples. Held-out prediction errors are available in
    each table's \code{cv} attribute; see Value and the
    \code{peakVO2} example.

    Outer cross-validation requires additional computation because
    the analysis is repeated for each training sample. Supply
    \code{foldid} to use a particular partition, for example to
    keep records from the same subject together.
  }

  \subsection{Computational controls}{
    \code{fast = TRUE} uses \code{rfsrc.fast} for candidate prediction
    forests. Evaluation uses a shared holdout when sufficient data
    are available and out-of-bag predictions otherwise. Variable
    ranking uses all observations available to that selection
    analysis. Supply \code{sampsize} to control per-tree sample size.

    The \code{parallel} and \code{cores} arguments control the
    preliminary lasso calculations. Forest threading is controlled
    by \pkg{randomForestSRC}.
  }
}

\value{
  An object of class \code{"cv.varpro"} containing:
  \item{imp}{Original-variable importance table for the minimum-error
    selection, with columns \code{variable} and \code{z}.}
  \item{imp.conserve}{Corresponding conservative selection table.}
  \item{imp.liberal}{Corresponding liberal selection table.}
  \item{err}{Prediction-error path with columns \code{zcut},
    \code{nvar}, \code{err}, and \code{sd}: the importance cutoff,
    number of selected original predictors, mean block error, and
    standard deviation across tree blocks.}
  \item{zcut, zcut.conserve, zcut.liberal}{Cutoffs for the three
    full-data selections. See Details for fallback behavior.}

  With outer cross-validation, \code{variable} and \code{z} retain
  their full-data values and four columns are appended to each
  importance table:
  \describe{
    \item{\code{cv.select}}{Percentage of outer folds in which the
      variable was selected under that table's rule, including
      top-variable fallbacks. A variable excluded during preliminary
      screening counts as unselected.}
    \item{\code{cv.z.mean}}{Mean importance score across outer
      training samples. Uses all available scores, including those
      from folds where the variable was not selected.}
    \item{\code{cv.z.sd}}{Sample standard deviation of those
      importance scores. Larger values indicate greater variation
      in importance across training samples. It is \code{NA} when
      fewer than two scores are available.}
    \item{\code{cv.z.n}}{Number of training-fold importance scores
      contributing to \code{cv.z.mean} and \code{cv.z.sd}. This can
      be smaller than the number of folds when preliminary screening
      excludes a variable or its score is unavailable.}
  }
  These importance summaries use finite scores before applying the
  selection cutoff. With five folds, a variable scored in every fold
  and selected in two has \code{cv.z.n = 5} and \code{cv.select = 40}.
  The mean is \code{NA} when no score is available.

  Each importance table has a \code{cv} attribute containing its
  mean held-out error (\code{err}), standard deviation of fold
  errors (\code{sd}), performance measure (\code{perf.type}), and
  per-fold results (\code{folds}). It also records selection
  percentages for all original predictors
  (\code{selection.frequency}) and whether the full-data selection
  used the top-variable fallback (\code{fallback}). For example,
  use \code{attr(o$imp, "cv")} to inspect the minimum-error rule's
  assessment. Fold assignments are available through
  \code{attr(o, "cv")$foldid}, in input-row order, with \code{NA}
  for omitted observations.

  Attributes also retain the processed-column importance object
  (\code{imp.org}), processed and original predictor names
  (\code{xvar.names}, \code{xvar.org.names}), and the VarPro forest
  family (\code{family}). Use \code{get.vimp(o)} to inspect
  processed-column scores and \code{get.orgvimp(o, pretty = FALSE)}
  for original-variable scores with unselected variables filled
  by zero.

  The print method displays the selections and a compact outer-CV
  performance summary when available. To construct a final prediction
  model, train the chosen learner using a returned variable set,
  as shown in Examples.
}

\note{
  For serial lasso calculations, supply \code{parallel = FALSE}.
  Outer error summaries are \code{NA} when a fold error is undefined
  or folds use different performance measures. Inspect
  \code{attr(o$imp, "cv")$folds} for the individual fold results.
}

\author{Min Lu and Hemant Ishwaran}

\references{
  Lu, M. and Ishwaran, H. (2024). Model-independent variable selection
  via the rule-based variable priority. arXiv:2409.09003.
  \doi{10.48550/arXiv.2409.09003}.

  O'Brien, R. and Ishwaran, H. (2019). A random forests quantile
  classifier for class imbalanced data. \emph{Pattern Recognition},
  90, 232--249.
}

\seealso{
  \code{\link{varpro}}, \code{\link{importance.varpro}},
  \code{\link{predict.varpro}},
  \code{\link[randomForestSRC:rfsrc]{randomForestSRC::rfsrc}},
  \code{\link[randomForestSRC:rfsrc.fast]{randomForestSRC::rfsrc.fast}}.
}

\examples{
\donttest{
## Nonlinear regression with a categorical effect and known signal set.
set.seed(137)
n <- 500
x <- matrix(runif(n * 10), nrow = n)
colnames(x) <- sprintf("x\%02d", seq_len(ncol(x)))
group <- factor(sample(c("low", "middle", "high"), n, replace = TRUE),
                levels = c("low", "middle", "high"))
mu <- 10 * sin(pi * x[, 1] * x[, 2]) +
      20 * (x[, 3] - 0.5)^2 + 10 * x[, 4] + 5 * x[, 5] +
      3 * (group == "high")
d <- data.frame(y = mu + rnorm(n), x, group)
signal <- c(colnames(x)[1:5], "group")

set.seed(137)
o <- cv.varpro(y ~ ., d, ntree = 80,
               zcut = seq(0.1, 3, length.out = 10), nblocks = 6,
               max.tree = 60, max.rules.tree = 40,
               nfolds = 5, parallel = FALSE)

## Compare the three full-data selections.
print(o$imp)
print(o$imp.conserve)
print(o$imp.liberal)

print(data.frame(
  rule = c("minimum", "conservative", "liberal"),
  cutoff = c(o$zcut, o$zcut.conserve, o$zcut.liberal),
  nvar = c(NROW(o$imp), NROW(o$imp.conserve), NROW(o$imp.liberal))))
print(data.frame(variable = o$imp$variable, z = o$imp$z,
                 signal = o$imp$variable \%in\% signal))

## Original-variable scores and encoded-column scores at each cutoff.
print(get.orgvimp(o, pretty = FALSE))
print(get.vimp(o, pretty = FALSE))

## Plot the error path. Bars show one tree-block standard deviation.
e <- as.data.frame(o$err)
e <- e[is.finite(e$err) & is.finite(e$sd), , drop = FALSE]
if (nrow(e) > 0) {
  ylim <- range(c(pmax(0, e$err - e$sd), e$err + e$sd))
  plot(e$zcut, e$err, type = "b", pch = 19, ylim = ylim,
       xlab = "Importance cutoff",
       ylab = "Mean tree-block prediction error")
  arrows(e$zcut, pmax(0, e$err - e$sd),
         e$zcut, e$err + e$sd, angle = 90, code = 3, length = 0.04)
  abline(h = min(e$err) + mean(e$sd), lty = 2)
  abline(v = o$zcut, lty = 3)
}

## Train a final learner with the minimum-error variable set.
selected <- o$imp$variable
if (length(selected) > 0 && any(is.finite(o$err[, "err"]))) {
  final <- randomForestSRC::rfsrc(
    y ~ ., d[, c("y", selected), drop = FALSE], ntree = 300)
}

## Outer cross-validation: survival prediction with peakVO2.
## Each fold repeats ranking and cutoff selection on its training data.
library(survival)
data(peakVO2, package = "randomForestSRC")
peak <- na.omit(peakVO2)

set.seed(137)
peak.cv <- cv.varpro(Surv(ttodead, died) ~ ., peak,
                     cv.folds = 5, ntree = 100, seed = 137,
                     zcut = seq(0.1, 3, length.out = 12), nblocks = 6,
                     max.tree = 75, max.rules.tree = 40,
                     nfolds = 5, parallel = FALSE)

## The selections and z scores come from the full-data analysis.
## cv.select gives the percentage of outer folds selecting each variable.
print(peak.cv$imp)
print(peak.cv$imp.conserve)
print(peak.cv$imp.liberal)

## Compare held-out performance for the three rules.
## Error is 1 - C; lower is better. cv.sd is the deviation across folds.
## nvar.full counts predictors selected in the full-data analysis.
rules <- c("imp", "imp.conserve", "imp.liberal")
cv.summary <- do.call(rbind, lapply(rules, function(rule) {
  tab <- peak.cv[[rule]]
  assessment <- attr(tab, "cv")
  data.frame(rule = rule, nvar.full = nrow(tab),
             cv.err = assessment$err, cv.sd = assessment$sd,
             perf.type = assessment$perf.type)
}))
print(cv.summary)

## Inspect fold errors, variable counts, cutoffs, and fallback status.
print(attr(peak.cv$imp, "cv")$folds)

## Compare selection percentages for all original predictors,
## including those absent from the full-data selections.
cv.select <- sapply(rules, function(rule) {
  attr(peak.cv[[rule]], "cv")$selection.frequency
})
print(cv.select[order(cv.select[, "imp"], decreasing = TRUE),
                , drop = FALSE])

## Check the sizes of the actual outer folds.
print(table(attr(peak.cv, "cv")$foldid))

## Survival: RMST defines the ranking target; CRPS evaluates the
## resulting survival prediction models on their OOB observations.
library(survival)
data(pbc, package = "randomForestSRC")
pbc <- na.omit(pbc)
set.seed(137)
s <- cv.varpro(Surv(days, status) ~ ., pbc,
               rmst = 1000, crps = TRUE, fast = FALSE,
               ntree = 100, nblocks = 6,
               zcut = seq(0.1, 3, length.out = 12),
               max.tree = 75, max.rules.tree = 40,
               nfolds = 5, parallel = FALSE)
print(s$imp)
print(s$err)
}
}
\keyword{models}
\keyword{tree}
