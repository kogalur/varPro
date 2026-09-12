\name{varpro}
\alias{varpro}
\title{Model-Independent Variable Selection via the Rule-Based Variable Priority (VarPro)}

\description{
  Identifies predictors associated with a response by comparing forest
  rule regions with their near-miss sets. Supports regression, multivariate
  regression, classification, and right-censored survival. The returned
  object contains the rule-generating forest and rule-level scores;
  \code{importance} summarizes these scores and \code{cv.varpro} selects
  an importance cutoff using predictive performance.
}

\usage{
varpro(formula, data, nvar = 30, ntree = 500,
       split.weight = TRUE, split.weight.method = NULL, sparse = TRUE,
       nodesize = NULL, max.rules.tree = 150, max.tree = min(150, ntree),
       parallel = TRUE, cores = get.mc.cores(),
       verbose = FALSE, seed = NULL, ...)
}

\arguments{
  \item{formula}{Formula specifying the response and predictors, such as
    \code{y ~ .}, \code{cbind(y1, y2) ~ .}, or
    \code{Surv(time, status) ~ .}. Use a factor response for
    classification and numeric responses for regression. Survival
    status is coded 0 for censoring and 1 for an event.}

  \item{data}{Data frame containing the response and predictors.
    Incomplete observations are removed, unused factor levels are
    dropped, and categorical predictors are hot-encoded. If observations
    are omitted, a warning reports the input, omitted, and retained counts.
    See Details.}

  \item{nvar}{Maximum number of processed predictor columns retained
    by split-weight screening before rule generation. A categorical
    predictor can contribute several columns. Applies when computed
    or custom split-weights are used; \code{split.weight = FALSE}
    without custom weights uses all processed predictors.}

  \item{ntree}{Number of trees in the rule-generating forest.}

  \item{split.weight}{Should preliminary split-weights guide rule
    generation? Positive-weight predictors are screened to at most
    \code{nvar} columns, and candidate split variables are sampled
    according to their retained weights.}

  \item{split.weight.method}{Character string or vector selecting
    preliminary weighting methods: \code{"lasso"}, \code{"tree"}, and
    \code{"vimp"}. Methods can be combined, for example
    \code{c("lasso", "tree")}. The default \code{NULL} selects a
    combination automatically. See Details.}

  \item{sparse}{Should preliminary weighting concentrate more strongly
    on promising predictors? Useful when relatively few predictors
    are expected to carry signal. See Details.}

  \item{nodesize}{Minimum terminal node size for the rule-generating
    forest. Larger values produce broader regions with more
    observations for the local comparisons. The default is chosen
    automatically from the data dimensions.}

  \item{max.rules.tree}{Maximum number of tree branches sampled per
    selected tree. A branch can yield several comparisons, one for
    each variable released from its rule.}

  \item{max.tree}{Maximum number of trees sampled for rule extraction.
    Increasing this value provides more trees for importance
    aggregation, up to the number grown.}

  \item{parallel}{Should the preliminary lasso cross-validation folds
    run in parallel?}

  \item{cores}{Number of cores for lasso-fold parallelism. The default
    respects \code{options(mc.cores = ...)} or \env{MC_CORES}.
    Forest parallelism is controlled separately by
    \pkg{randomForestSRC}.}

  \item{verbose}{Should progress messages and preliminary
    split-weights be printed?}

  \item{seed}{Seed supplied to the main forest calculations. Use
    \code{set.seed} before the call to also control R-level sampling,
    lasso folds, and auxiliary calculations. See Note.}

  \item{...}{Additional named controls for split-weight construction,
    rule generation, and external survival estimation. The supported
    controls used most often are described in Details. Unnamed controls,
    unrecognized names, and duplicate names cause an error.}
}

\details{
  \subsection{What VarPro measures}{
    VarPro studies a predictor's contribution by comparing responses
    in a rule region and its near-miss set. In regression, the target is
    the conditional mean of the response; in classification, it is
    the vector of conditional class probabilities. Survival analysis
    uses a forest estimate of mortality or restricted mean survival
    time as its response summary.

    The workflow has three steps. \code{varpro} prepares the data,
    generates rules, and stores their release comparisons.
    \code{importance} combines the comparisons into variable-level
    scores. \code{cv.varpro} uses the scores to form candidate variable
    sets and chooses among them using forest prediction error.
  }

  \subsection{Rules and near-miss comparisons}{
    A root-to-terminal-node path defines a region \eqn{R} by combining
    its split conditions. Releasing predictor \eqn{s} removes every
    condition involving that predictor while retaining the conditions
    on the other predictors. This produces the enlarged release region
    \eqn{R^{(s)}}{R^(s)}. The code compares responses
    in \eqn{R} with those in the near-miss set
    \deqn{
      C_s = R^{(s)} \setminus R = R^c \cap R^{(s)}.
    }{
      C_s = R^(s) minus R = (complement of R) intersect R^(s).
    }

    For example, a rule \eqn{x_1 \leq 0.5,\ x_2 > 0.3}{x1 <= 0.5 and x2 > 0.3}
    becomes \eqn{x_2 > 0.3}{x2 > 0.3} when \eqn{x_1}{x1} is released.
    Its near-miss set contains observations with
    \eqn{x_1 > 0.5,\ x_2 > 0.3}{x1 > 0.5 and x2 > 0.3}.
    Comparing responses in the original region and the near-miss set
    measures the local contribution of \eqn{x_1}{x1} while preserving
    the restriction on \eqn{x_2}{x2}.

    Rules are extracted from the forest. At most \code{max.tree} trees and
    \code{max.rules.tree} branches per selected tree contribute to the
    analysis. Original-region observations are out-of-bag for the
    corresponding tree. Repeated comparisons allow a predictor to
    contribute differently in different regions of the feature space.
  }

  \subsection{Guided rule generation and the lasso}{
    Preliminary split-weights help the forest form informative rules.
    Three sources of information are available:
    \describe{
      \item{\code{"lasso"}}{Absolute coefficients from a cross-validated
        penalized regression on standardized predictors. This favors
        predictors with strong linear effects or categorical contrasts.}
      \item{\code{"tree"}}{Split frequencies from a shallow forest,
        which can identify nonlinear effects and interactions.}
      \item{\code{"vimp"}}{Positive permutation importance from a
        preliminary forest, measuring a predictor's contribution
        to prediction accuracy.}
    }
    Combining methods uses complementary evidence when forming the
    weights. With \code{split.weight.method = NULL}, the combination
    is chosen automatically. Set an explicit method to control which
    sources are used.

    The lasso penalty is selected by cross-validation. The weights use
    the more regularized solution within one standard error of the
    minimum error. Coefficient magnitudes are combined across responses
    or classes when needed. For survival, the lasso uses the response
    summary described below.

    With \code{sparse = TRUE}, stronger preliminary weights receive
    greater emphasis. Use \code{FALSE} to spread the emphasis more
    broadly. At most \code{nvar} positive-weight processed columns are
    retained for rule generation. Increase \code{nvar} to explore more
    predictors, or use \code{split.weight = FALSE} to generate rules
    using all processed predictors without preliminary weighting.

    The retained weights guide candidate-variable sampling in the
    forest. Final VarPro importance is calculated from the resulting
    release comparisons.
  }

  \subsection{Importance scores and selection}{
    Call \code{importance(o)} to summarize a \code{varpro} object.
    Its default \code{local.std = TRUE} recomputes locally
    standardized comparisons from the region memberships. For
    regression, the score comparing \eqn{R} and its near-miss set
    \eqn{C_s} is the absolute Welch statistic
    \deqn{
      T = \frac{|\bar y_R - \bar y_{C_s}|}
        {\sqrt{s_R^2/n_R + s_{C_s}^2/n_{C_s}}}.
    }{
      T = abs(mean(y_R) - mean(y_Cs)) /
          sqrt(var(y_R) / n_R + var(y_Cs) / n_Cs).
    }
    The same calculation is applied separately to each numeric
    response, including external survival
    summaries. For classification, the overall comparison is the
    square root of a Pearson chi-squared statistic divided by its
    degrees of freedom, with continuity correction for two-class
    tables. Class-specific comparisons use absolute
    standardized differences in proportions.

    Within each tree, comparisons for a predictor are averaged with
    weights equal to their original-region sample sizes. Class-specific
    summaries use the corresponding class counts. The tree-level
    scores are then averaged after winsorization at the
    lower and upper 10 percent quantiles by default. With local
    standardization, this mean is the reported \code{z} score. With
    \code{local.std = FALSE},
    \code{importance} summarizes the stored rule scores and divides
    their winsorized mean by their winsorized standard deviation
    across trees.

    Larger scores indicate stronger response changes under release.
    The default \code{importance} cutoff of 0.79 is used
    to highlight variables in its plots. To choose a cutoff
    from predictive performance, use \code{cv.varpro}. Its optional
    \code{cv.folds} argument assesses the complete selection procedure
    on held-out folds and appends stability summaries to the returned
    importance tables. See
    \code{\link{importance.varpro}} for the summary and plotting
    controls.
  }

  \subsection{Categorical predictors and outcome families}{
    Numeric predictors retain their values. Two-level categorical
    predictors become binary columns; predictors with more levels
    become indicator columns. Screening, forest rules, and
    \code{importance(o)} operate on these processed columns.
    \code{o$x} stores the complete processed predictor data, while
    \code{o$xvar.names} identifies the columns retained for rule
    generation.

    \code{get.orgvimp(o)} summarizes scores at the original-variable
    level using the largest score among a predictor's encoded
    columns. For multivariate regression it also takes the maximum
    across outcomes. \code{cv.varpro} uses this original-variable
    summary for its candidate sets. \code{get.vimp(o)} returns named
    scores for the represented processed columns. With
    \code{pretty = FALSE}, \code{get.vimp} and \code{get.orgvimp}
    return all processed or original predictors, respectively,
    filling absent scores with zero.

    In binary classification, the working response labels are 0 for
    the majority class and 1 for the minority class. For imbalanced
    classes, the default handling uses AUC
    splitting and RFQ with geometric-mean performance for the
    preliminary permutation-importance calculation. The original
    response labels remain in \code{y.org} and the rule-generating
    forest. The controls \code{use.rfq} and \code{iratio.threshold}
    modify this behavior.
  }

  \subsection{Survival targets}{
    A preliminary survival forest estimates a numeric response
    summary for each training observation. By default, this is
    forest mortality. Supplying \code{rmst = tau} requests restricted
    mean survival time, obtained by integrating the estimated
    survival curve up to \code{tau}. A vector of horizons supplies
    one numeric response per horizon.

    These full-ensemble training summaries are used for preliminary
    weighting and the default locally standardized importance.
    With mortality or one RMST horizon, rules are generated using
    the original survival outcome. With multiple RMST horizons,
    rule generation uses multivariate regression on the RMST
    responses. Thus the horizons determine the survival features
    being prioritized. The controls \code{ntree.external},
    \code{nodesize.external}, and \code{ntime.external} govern the
    preliminary survival forest.
  }

  \subsection{Additional controls and computation}{
    Useful arguments supplied through \code{...} include
    \code{nfolds} for lasso cross-validation, \code{rmst} for the
    survival target, and \code{ntree.external} for the preliminary
    survival forest.

    Custom split-weights can be supplied as a named numeric vector
    through \code{split.weight.custom}. Names must refer to processed
    predictor columns; omitted columns receive zero weight. Use
    \code{get.splitweight.custom(formula, data)} to obtain a correctly
    named starting vector, then assign positive weights to the
    predictors of interest.

    Reducing \code{ntree}, \code{max.tree}, or \code{max.rules.tree}
    reduces computation. Increasing \code{nodesize} produces larger
    comparison groups with less local detail. The \code{parallel} and
    \code{cores} arguments control lasso-fold parallelism; forest
    threading is controlled by \pkg{randomForestSRC}.
  }
}

\value{
  An object of class \code{"varpro"} with the following components:
  \item{rf}{The rule-generating \code{rfsrc} forest.}
  \item{split.weight}{Named weights for the retained predictor columns,
    or \code{NULL} when computed and custom weighting are both absent.}
  \item{split.weight.raw}{List of untransformed preliminary components
    actually calculated, named \code{lasso}, \code{tree}, or
    \code{vimp}. Empty when preliminary estimation is skipped.}
  \item{results}{Rule-level data frame with tree and branch identifiers,
    released-variable index, original-region sample size \code{n.oob},
    and importance columns. Indices refer to \code{xvar.names}.
    Classification includes class-specific counts and scores;
    multivariate regression includes one score per response.
    \code{importance} supplies the variable-level summary.}
  \item{x}{Complete-case processed predictor data, including columns
    removed by preliminary screening. Encoding information is stored
    as attributes.}
  \item{xvar.names}{Processed predictor names retained for rule
    generation.}
  \item{xvar.org.names}{Original predictor names before encoding.}
  \item{y}{Working response: numeric response(s), recoded class labels,
    or external survival summary values.}
  \item{y.org}{Original response values for the retained observations.
    For survival, these are the observed times and event indicators.}
  \item{yvar.names}{Names of the working response columns.}
  \item{family}{Family of the rule-generating forest: \code{"regr"},
    \code{"regr+"}, \code{"class"}, or \code{"surv"}. Multiple
    RMST horizons use \code{"regr+"}.}
  \item{max.rules.tree, max.tree}{Stored rule-extraction limits used by
    subsequent importance calculations.}
  \item{model.info}{Compact description of the analysis. Its
    \code{observations} component records \code{input}, \code{retained},
    and \code{omitted} row counts for the data supplied to this
    \code{varpro} call.}
}

\note{
  For reproducible serial analyses, call \code{set.seed} immediately
  before \code{varpro} and use \code{parallel = FALSE}. The
  \code{seed} argument controls only the forest calls to which it is
  passed. Rule resampling and lasso folds also use random numbers.
  Calling \code{importance} again can resample rules; retain a summary
  for repeated inspection of the same calculation.

  The \code{...} controls are consumed by specific stages. General
  \code{rfsrc} arguments such as \code{na.action}, \code{splitrule},
  and \code{perf.type} are not forwarded wholesale. Perform any
  desired imputation before calling \code{varpro}.
}

\author{Min Lu and Hemant Ishwaran}

\references{
  Lu, M. and Ishwaran, H. (2024). Model-independent variable selection
  via the rule-based variable priority. arXiv:2409.09003.
  \doi{10.48550/arXiv.2409.09003}.

  Friedman, J., Hastie, T. and Tibshirani, R. (2010). Regularization
  paths for generalized linear models via coordinate descent.
  \emph{Journal of Statistical Software}, 33(1), 1--22.
  \doi{10.18637/jss.v033.i01}.

  O'Brien, R. and Ishwaran, H. (2019). A random forests quantile
  classifier for class imbalanced data. \emph{Pattern Recognition},
  90, 232--249.

  Ishwaran, H. (2025). \emph{Multivariate Statistics: Classical
  Foundations and Modern Machine Learning}. Chapman and Hall/CRC.
}

\seealso{
  \code{\link{importance.varpro}}, \code{\link{cv.varpro}},
  \code{\link{predict.varpro}}, \code{\link{ivarpro}},
  \code{\link{uvarpro}}, \code{\link{partialpro}},
  \code{\link{outpro}}, \code{\link{isopro}},
  \code{\link[randomForestSRC:rfsrc]{randomForestSRC::rfsrc}},
  \code{\link[glmnet:cv.glmnet]{glmnet::cv.glmnet}}.
}

\examples{
## Small regression example.
o <- varpro(mpg ~ ., mtcars, ntree = 1)

\donttest{
## Nonlinear regression with five signal and five noise predictors.
set.seed(137)
n <- 300
x <- matrix(runif(n * 10), nrow = n)
colnames(x) <- sprintf("x\%02d", seq_len(ncol(x)))
mu <- 10 * sin(pi * x[, 1] * x[, 2]) +
      20 * (x[, 3] - 0.5)^2 + 10 * x[, 4] + 5 * x[, 5]
d <- data.frame(y = mu + rnorm(n), x)
signal <- colnames(x)[1:5]

## Shallow-tree-guided rule generation.
o <- varpro(y ~ ., d, ntree = 25, ntree.reduce = 25,
            split.weight.method = "tree", nodesize = 10,
            max.tree = 25, max.rules.tree = 20, parallel = FALSE)
imp <- importance(o)
print(data.frame(variable = rownames(imp), z = imp$z,
                 signal = rownames(imp) \%in\% signal))

## Automatic weighting with more trees and comparisons.
set.seed(137)
o <- varpro(y ~ ., d, ntree = 150, max.tree = 100,
            max.rules.tree = 50, nfolds = 5, parallel = FALSE)
imp <- importance(o, plot.it = TRUE)
print(imp)
print(get.vimp(o))
print(get.vimp(o, pretty = FALSE))
print(get.topvars(o))

## Choose a variable set using predictive performance.
set.seed(137)
## Add cv.folds = 5 to assess selection and obtain stability summaries.
cv <- cv.varpro(y ~ ., d, ntree = 100,
                zcut = seq(0.1, 3, length.out = 12), nblocks = 6,
                max.tree = 75, max.rules.tree = 40,
                nfolds = 5, parallel = FALSE)
print(cv$imp)
print(cv$imp.conserve)

## Hot-encoding: compare processed-column and original-variable scores.
## Several predictors are converted to factors before analysis.
data(BostonHousing, package = "mlbench")
Boston <- BostonHousing
Boston$zn <- factor(Boston$zn)
Boston$chas <- factor(Boston$chas)
Boston$lstat <- factor(round(0.2 * Boston$lstat))
Boston$nox <- factor(round(20 * Boston$nox))
Boston$rm <- factor(round(Boston$rm))
set.seed(137)
b <- varpro(medv ~ ., Boston, ntree = 100,
            max.tree = 75, max.rules.tree = 40,
            nfolds = 5, parallel = FALSE)

## Calculate the processed-column importance
set.seed(137)
b.imp <- importance(b)
print(b.imp)

## Map max scores back to original predictors.
set.seed(137)
b.org <- get.orgvimp(b)
print(b.org)
print(get.orgvimp(b, vmp = b.imp)) ## should be identical; no seed required

## Include every original predictor, filling absent scores with zero.
set.seed(137)
print(get.orgvimp(b, pretty = FALSE))


## Multiclass classification: overall and class-specific importance.
data(wine, package = "randomForestSRC")
wine$quality <- factor(wine$quality)
set.seed(137)
w <- varpro(quality ~ ., wine, ntree = 100,
            max.tree = 75, max.rules.tree = 40,
            nfolds = 5, parallel = FALSE)
w.imp <- importance(w)
print(w.imp$unconditional)
print(w.imp$conditional.z)

## Survival: canonical example
data(peakVO2, package = "randomForestSRC")
s <- varpro(Surv(ttodead, died)~., peakVO2)
print(importance(s))


## Survival: prioritize variables affecting RMST through 500 days.
data(pbc, package = "randomForestSRC")
pbc <- na.omit(pbc)
set.seed(137)
s <- varpro(Surv(days, status) ~ ., pbc, rmst = 500,
            ntree = 100, max.tree = 75, max.rules.tree = 40,
            nfolds = 5, parallel = FALSE)
print(importance(s))
print(get.orgvimp(s))
print(get.vimp(s, pretty = FALSE))

## Multiple horizons give one importance summary per horizon.
set.seed(137)
s.multi <- varpro(Surv(days, status) ~ ., pbc,
                  rmst = c(500, 1000), ntree = 100,
                  max.tree = 75, max.rules.tree = 40,
                  nfolds = 5, parallel = FALSE)
print(importance(s.multi))
}
}
\keyword{models}
\keyword{tree}
