\name{cv.varpro}
\alias{cv.varpro}
\title{Select VarPro Predictors and Assess Selection by Cross-Validation}

\description{
  Uses VarPro importance scores and random forest prediction error
  to select predictors. Returns a minimum-error selection and
  conservative and liberal alternatives. Optional cross-validation
  assesses how well the selection procedure predicts new observations
  and how consistently variables are selected.
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

  \item{nvar}{Maximum number of predictor columns kept during
    preliminary split-weight screening in \code{varpro}. Categorical
    predictors may contribute several indicator columns. Candidate
    models use the original predictors. Ignored by the screening step
    when neither computed nor custom split-weights are used.}

  \item{ntree}{Number of trees in the VarPro rule forest, each
    candidate prediction forest, and each outer assessment forest.}

  \item{local.std}{Should locally standardized rule comparisons be
    used to calculate importance? See \code{\link{varpro}}.}

  \item{zcut}{Numeric vector of positive importance cutoffs.
    Predictors with \code{z >= zcut} enter a candidate model.
    Cutoffs are sorted into increasing order, and each distinct
    variable set is evaluated once.}

  \item{nblocks}{Positive integer controlling how each candidate
    forest is divided into blocks of trees to calculate prediction
    error and its variation. Larger values give smaller blocks. Use
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
    \code{randomForestSRC::rfsrc.fast}? This uses smaller samples to
    grow each tree. When enough data are available, a common test
    sample is set aside to compare the candidate forests. See Details.}

  \item{crps}{For survival, should prediction error be based on
    predicted survival probabilities using an integrated Brier score
    adjusted for censoring? With \code{FALSE}, the forest's default
    concordance error is used. See Details for the score definitions.}

  \item{cv.folds}{Number of cross-validation folds for assessing
    the selection procedure. The default, zero, performs variable
    selection only. A value of at least two also evaluates prediction
    error on left-out observations and summarizes how often variables
    are selected under each of the three rules. Generated folds are
    stratified by response class or survival event status, as
    appropriate. The fold count is reduced with a warning when the
    data cannot support the requested number. This is separate from
    \code{nblocks} and the preliminary lasso's \code{nfolds} option.}

  \item{foldid}{Optional numeric vector of fold labels, one per input data
    row, to specify the cross-validation groups. Observations used
    in the analysis must have consecutive integer labels starting at
    one. Entries for omitted observations are ignored. Supplying
    \code{foldid} enables cross-validation even when
    \code{cv.folds = 0}; a positive
    \code{cv.folds} must match the number of supplied folds.
    Use common labels to keep records from the same subject together.}

  \item{...}{Additional named arguments passed to \code{varpro},
    including \code{parallel}, \code{cores}, \code{nfolds},
    \code{rmst}, and external-forest controls. The \code{sampsize}
    argument also controls candidate-forest sampling. See Details.}
}

\details{
  \code{cv.varpro} helps choose which predictors to use in a
  prediction model. It ranks predictors using \code{\link{varpro}},
  then compares the prediction errors of random forests built with
  different sets of higher-scoring variables. The result is a
  minimum-error selection, with conservative and liberal alternatives
  that allow fewer or more variables for a similar prediction error.

  By default, the function selects variables using the supplied data.
  Optional cross-validation also assesses prediction on observations
  left out of variable selection and model fitting, and shows how
  consistently variables are selected. Use \code{cv.folds} to request
  this additional assessment.

  \subsection{Choosing variables}{
    The \code{zcut} argument gives the importance cutoffs to compare.
    For each cutoff, variables whose importance score \code{z} is at
    least that cutoff are used together in a prediction forest.
    Increasing the cutoff removes lower-scoring variables. Each
    distinct variable set is evaluated once.

    Selection uses the original variables. For a categorical
    predictor represented by several indicator columns, its score
    is the largest importance among those columns. Multivariate
    regression also takes the largest score across responses;
    classification uses overall importance. Prediction forests use
    the original outcome and predictors, so selecting a categorical
    variable includes all its levels.
  }

  \subsection{Comparing prediction errors}{
    By default, candidate forests are compared using out-of-bag
    (OOB) predictions: each observation is predicted using trees
    grown without that observation. Each forest is evaluated in
    blocks of trees, giving one prediction error per block. The mean
    of these errors is used to compare variable sets, and their
    standard deviation describes variation across blocks. With
    \code{nblocks = 1}, the whole forest is evaluated and the block
    standard deviation is zero.

    To allow alternatives with similar prediction errors, an error
    limit is calculated as the smallest mean error plus the average
    block standard deviation across candidate models. Three
    selections are returned:
    \describe{
      \item{Minimum error}{The variable set with the lowest mean
        error, returned in \code{imp}.}
      \item{Conservative}{The smallest variable set with mean error
        at or below the error limit and also below 1, returned in
        \code{imp.conserve}.}
      \item{Liberal}{The largest variable set with mean error at or
        below the error limit, returned in \code{imp.liberal}.}
    }
    Use the \code{variable} column in a returned table to fit a final
    prediction model with those predictors; see Examples.

    Without outer cross-validation, \code{imp.conserve} is
    \code{NULL} when no model meets the conservative criterion.
    With outer cross-validation, an empty selection is replaced by
    the highest-ranked variable from the data used for that selection.
    This applies to the full-data analysis and each fold's training
    sample. If no candidate model has a finite error, a warning is
    issued and all three rules return the
    importance ranking without applying a cutoff, with cutoffs
    set to zero.
  }

  \subsection{Performance measures}{
    Lower error is better for every supported family.
    \describe{
      \item{Regression}{Mean squared prediction error divided by the
        response variance. Multivariate regression uses the first
        response for prediction error and combines responses for
        the importance ranking.}

      \item{Classification}{Normalized Brier prediction error. For
        imbalanced two-class data, the random forests quantile
        classifier (RFQ) uses geometric-mean error,
        \eqn{1 - \sqrt{\mathrm{sensitivity} \times \mathrm{specificity}}}{
          1 - sqrt(sensitivity * specificity)}. The controls
        \code{use.rfq} and \code{iratio.threshold} apply to both
        variable ranking and candidate-model evaluation.}

      \item{Survival, \code{crps = FALSE}}{Concordance error,
        \eqn{1-C}, from the survival forest.}

      \item{Survival, \code{crps = TRUE}}{An integrated Brier score
        for predicted survival probabilities, adjusted for censoring
        and divided by the maximum evaluation time.}
    }
    Supplying \code{rmst} changes the survival summary used for the
    VarPro ranking. Prediction error is still evaluated against the
    original survival outcome using concordance error or CRPS.
  }

  \subsection{Checking performance with cross-validation}{
    The errors above guide variable selection. Outer cross-validation
    checks prediction on observations left out of both variable
    selection and model fitting.

    For example, \code{cv.folds = 5} divides the observations into
    five groups, called folds. Each fold is left out in turn. The
    remaining folds are used to repeat variable ranking and cutoff
    selection, then fit prediction forests for the three selected
    variable sets. These forests predict the left-out observations.
    The selected variables can differ between folds.

    Each fold gives one prediction error per selection rule, using
    the whole prediction forest. The mean and standard deviation of
    these fold errors summarize performance. The tree blocks
    controlled by \code{nblocks} are used to choose variables within
    each training sample.

    The returned \code{imp}, \code{imp.conserve}, and
    \code{imp.liberal} tables give the selections and importance
    scores from the analysis of all available data. Added columns
    show how often each variable was selected across folds and how
    much its importance varied. Prediction errors on the left-out
    folds are summarized in each table's \code{cv} attribute; see
    Value and the \code{peakVO2} example.

    Cross-validation takes additional computation because the
    analysis is repeated for each fold. Supply \code{foldid} to
    specify the groups, for example to keep records from the same
    subject together.
  }

  \subsection{Computing options}{
    \code{fast = TRUE} uses \code{rfsrc.fast} for candidate prediction
    forests. When enough data are available, a common test sample is
    set aside to compare these forests. Otherwise, they are compared
    using OOB predictions. The VarPro ranking uses all observations
    available in each selection run. Supply \code{sampsize} to
    control the number of observations sampled to grow each tree.

    The \code{parallel} and \code{cores} arguments control the
    preliminary lasso calculations. Forest threading is controlled
    by \pkg{randomForestSRC}.
  }
}

\value{
  An object of class \code{"cv.varpro"} containing:
  \item{imp}{Selected predictors and their importance scores for
    the minimum-error selection, with columns \code{variable} and
    \code{z}. Predictors are identified by their original names.}
  \item{imp.conserve}{Corresponding conservative selection table.}
  \item{imp.liberal}{Corresponding liberal selection table.}
  \item{err}{Prediction errors for the candidate variable sets in
    the full-data analysis, with columns \code{zcut}, \code{nvar},
    \code{err}, and \code{sd}:
    the importance cutoff, number of selected original predictors,
    mean block error, and standard deviation across tree blocks.}
  \item{zcut, zcut.conserve, zcut.liberal}{Cutoffs for the three
    full-data selections. See Details for fallback behavior.}

  With outer cross-validation, \code{variable} and \code{z} still
  describe the selection using all available data. Four columns are
  added to each importance table:
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

  With outer cross-validation, each importance table has a \code{cv}
  attribute containing its mean fold error (\code{err}, with equal
  weight for each fold), standard deviation of fold errors (\code{sd}),
  performance measure (\code{perf.type}), and
  per-fold results (\code{folds}). It also records selection
  percentages for all original predictors
  (\code{selection.frequency}) and whether the full-data selection
  used the top-variable fallback (\code{fallback}). For example,
  use \code{attr(o$imp, "cv")} to inspect the minimum-error rule's
  assessment. Fold assignments are available through
  \code{attr(o, "cv")$foldid}, in input-row order, with \code{NA}
  for omitted observations.

  Other attributes contain the importance object for individual
  predictor columns (\code{imp.org}), processed and original predictor
  names (\code{xvar.names}, \code{xvar.org.names}), and the VarPro
  forest family (\code{family}). Use \code{get.vimp(o)} to inspect
  scores for individual columns, including indicator columns for
  categorical predictors. Use \code{get.orgvimp(o, pretty = FALSE)}
  for original-variable scores with unselected variables filled
  by zero.

  The print method displays the selections and a compact
  cross-validation performance summary when available.
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
