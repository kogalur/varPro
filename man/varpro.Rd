\name{varpro}
\alias{varpro}
\title{Model-Independent Variable Selection via the Rule-Based Variable Priority (VarPro)}

\description{
  Identifies predictors associated with a response. VarPro uses forest
  rules to compare groups of observations and measure each predictor's
  importance. Supports regression, multivariate regression,
  classification, and right-censored survival. Use \code{importance}
  to summarize the results and \code{cv.varpro} to select variables
  using prediction error.
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

  \item{nvar}{Maximum number of processed predictor columns used
    after preliminary weighting and before rule generation. A categorical
    predictor can contribute several columns. Applies when computed
    or custom split-weights are used; \code{split.weight = FALSE}
    without custom weights uses all processed predictors.}

  \item{ntree}{Number of trees in the rule-generating forest.}

  \item{split.weight}{Use preliminary weights to choose and favor
    predictors when growing the forest? At most \code{nvar}
    positive-weight predictor columns are used. See Details.}

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

  \item{cores}{Number of cores for parallel lasso cross-validation. The default
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
  VarPro helps identify predictors that are related to an outcome.
  It compares groups of observations to measure each predictor's
  association with changes in the response. A forest defines the
  groups, allowing the relationships to differ across the data.
  The comparisons are combined into an importance score for each
  predictor.

  Start with \code{o <- varpro(y ~ ., data)}, then use
  \code{imp <- importance(o)} and \code{print(imp)} to inspect the
  scores. Use \code{importance(o, plot.it = TRUE)} to add a plot.
  To select a set of predictors using prediction error, call
  \code{cv.varpro} with the same formula and data. It performs the
  VarPro analysis and compares prediction models using different
  importance cutoffs.

  \subsection{How the comparisons work}{
    A forest rule is a set of conditions on predictor values. For
    example, a rule might require \eqn{x_1 \leq 0.5}{x1 <= 0.5} and
    \eqn{x_2 > 0.3}{x2 > 0.3}. To assess \eqn{x_1}{x1}, VarPro
    removes its condition while keeping the condition on
    \eqn{x_2}{x2}. This is called \emph{releasing} a predictor.

    The observations newly admitted by this change satisfy
    \eqn{x_1 > 0.5}{x1 > 0.5} and \eqn{x_2 > 0.3}{x2 > 0.3}.
    They form the rule's \emph{near-miss set}. Comparing their
    responses with those in the original rule region measures the
    local contribution of \eqn{x_1}{x1}, with the restriction on
    \eqn{x_2}{x2} unchanged.

    More generally, for a rule region \eqn{R}, releasing predictor
    \eqn{s} gives an enlarged region \eqn{R^{(s)}}{R^(s)}. The
    near-miss set is
    \deqn{C_s = R^{(s)} \setminus R.}{C_s = R^(s) minus R.}
    The original-region observations used in each comparison are
    out-of-bag for that tree: they were not used to grow it. Repeating
    the comparisons across rules allows a predictor to be important
    in some groups of observations and less important in others.

    For regression, the comparisons concern mean responses; for
    classification, they concern class proportions. Survival uses a
    numeric summary estimated by a preliminary survival forest, as
    described below.
  }

  \subsection{Importance scores and variable selection}{
    \code{importance(o)} combines the rule comparisons into predictor
    scores. Larger scores indicate stronger response differences when
    that predictor is released. Classification supplies both an overall
    score and class-specific scores. Multivariate regression supplies
    a separate importance table for each response.

    The default \code{local.std = TRUE} in \code{importance} accounts
    for the variability and sample sizes of each comparison. In
    regression, the rule score is the absolute difference in group
    means divided by its estimated standard error:
    \deqn{
      T = \frac{|\bar y_R - \bar y_{C_s}|}
        {\sqrt{s_R^2/n_R + s_{C_s}^2/n_{C_s}}}.
    }{
      T = abs(mean(y_R) - mean(y_Cs)) /
          sqrt(var(y_R) / n_R + var(y_Cs) / n_Cs).
    }
    This is the absolute Welch statistic. The same calculation is
    used for each numeric response, including survival summaries.
    Classification uses a chi-squared comparison of class proportions
    for the overall score and standardized differences in proportions
    for the class-specific scores. The overall rule score is
    \eqn{\sqrt{\chi^2 / d}}{sqrt(chi-squared / d)}, where \eqn{d}
    is the degrees of freedom; two-class tables use a continuity
    correction.

    Comparisons within a tree are averaged using the numbers of
    original-region observations as weights. Class-specific scores
    use the corresponding class counts. Before averaging across trees,
    unusually small and large tree scores are limited to the lower
    and upper 10 percent quantiles by default. This is
    \emph{winsorization}. The resulting mean is reported as \code{z}.
    With \code{local.std = FALSE}, \code{importance} instead summarizes
    the stored rule scores and divides their winsorized mean by their
    winsorized standard deviation across trees.

    The \code{importance} cutoff, 0.79 by default, highlights variables
    in its plots; it does not filter the returned importance table.
    \code{get.topvars(o)} gives the predictor names represented in that
    table. Use \code{cv.varpro} for selection based on prediction error.
    Its optional \code{cv.folds} argument also assesses prediction on
    observations left out of selection and reports how often each
    variable is selected. See \code{\link{importance.varpro}} and
    \code{\link{cv.varpro}}.
  }

  \subsection{Choosing predictors for rule generation}{
    By default, preliminary weights help the forest find useful
    predictors before the rule comparisons are made. The weights
    determine which predictors are used and how often they are offered
    as candidates for a split. Three weighting methods are available:
    \describe{
      \item{\code{"lasso"}}{Absolute coefficients from a penalized
        regression fitted to standardized predictors.}
      \item{\code{"tree"}}{How often each predictor is used to split
        a shallow forest.}
      \item{\code{"vimp"}}{Positive permutation importance from a
        preliminary forest.}
    }
    \code{split.weight.method = NULL} chooses a combination
    automatically. Supply a method or a combination such as
    \code{c("lasso", "tree")} to choose the sources of the weights.
    The lasso uses the more strongly penalized solution within one
    standard error of the minimum cross-validation error. Coefficient
    magnitudes are combined across responses or classes when needed.

    \code{nvar} limits the number of positive-weight predictor columns
    used to generate rules. It is a preliminary screening limit, not
    the requested number of variables in a final selection. Factors
    can contribute several columns. Increase \code{nvar} to allow more
    columns, or set \code{split.weight = FALSE} to use all processed
    predictors without preliminary weighting. With
    \code{sparse = TRUE}, the weighting puts more emphasis on
    predictors with larger preliminary weights.

    Supply \code{split.weight.custom} through \code{...} to use a
    named vector of your own weights. Names must match the processed
    predictor columns; unspecified columns receive zero weight. Use
    \code{get.splitweight.custom(formula, data)} to obtain a starting
    vector with the correct names. Custom weights also use the
    \code{nvar} limit. The final importance scores come from the rule
    comparisons, rather than from the preliminary weights.
  }

  \subsection{Categorical predictors and classification}{
    Categorical predictors are converted to numeric columns, called
    hot-encoding. A two-level factor gives one binary column; a factor
    with more levels gives indicator columns. Numeric predictors keep
    their values. Screening, rules, and \code{importance(o)} use these
    processed columns. \code{o$x} contains the full processed predictor
    data, and \code{o$xvar.names} identifies the columns used by the
    rule-generating forest.

    Use \code{get.orgvimp(o)} for scores under the original predictor
    names. It takes the largest score among a predictor's encoded
    columns and, for multivariate regression, across responses.
    \code{cv.varpro} uses this summary to form its candidate variable
    sets. \code{get.vimp(o)} gives named scores for the processed
    columns. With \code{pretty = FALSE}, these two helpers include all
    original or processed predictors, respectively, filling missing
    scores with zero.

    Binary classification uses working labels 0 for the majority class
    and 1 for the minority class. The original labels are stored in
    \code{y.org} and used by the rule-generating forest. When the
    class-imbalance rule is met, the forest uses RFQ and AUC splitting;
    preliminary permutation importance uses geometric-mean performance.
    The controls \code{use.rfq} and \code{iratio.threshold} govern
    this behavior. \code{model.info$classification} records the class
    mapping and imbalance settings.
  }

  \subsection{Survival targets}{
    For survival data, a preliminary survival forest estimates a
    numeric response summary for each training observation. The default
    is forest mortality. Supply \code{rmst = tau} through \code{...}
    to use restricted mean survival time: the estimated survival curve
    integrated up to time \code{tau}. A vector of horizons requests
    a separate summary for each horizon. Horizons use the same time
    units as the survival outcome. Beyond the last stored time point,
    the calculation continues using the last estimated survival value.

    These full-ensemble training predictions guide preliminary
    weighting and the default locally standardized importance.
    With mortality or a single RMST response, rules are generated from
    the original survival outcome. With multiple RMST responses, rules
    are generated by multivariate regression on those responses.
    \code{ntree.external}, \code{nodesize.external}, and
    \code{ntime.external} control the preliminary survival forest.

    \code{model.info} distinguishes the original outcome, the working
    numeric targets, and the family of the rule-generating forest.
    This is useful when inspecting an analysis with survival-derived
    responses.
  }

  \subsection{Computation and reproducibility}{
    \code{ntree} controls the number of trees grown. \code{max.tree}
    and \code{max.rules.tree} limit the trees and branches used for
    the comparisons. Smaller limits reduce computation. Larger
    \code{nodesize} values give broader comparison groups with less
    local detail.

    \code{parallel} and \code{cores} control the preliminary lasso
    cross-validation, with \code{nfolds} supplied through \code{...}.
    Forest threading is controlled separately by \pkg{randomForestSRC}.
    For reproducible serial calculations, use \code{set.seed} before
    the analysis and \code{parallel = FALSE}; see Note.
  }
}

\value{
  An object of class \code{"varpro"} with the following components:
  \item{rf}{The rule-generating \code{rfsrc} forest.}
  \item{split.weight}{Named weights for the predictor columns used by the forest,
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
  \item{xvar.names}{Processed predictor names used for rule generation.}
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
  \item{model.info}{Description of the original outcome (\code{original}),
    working responses (\code{working}), and rule-generating forest
    (\code{forest}), with additional survival or classification
    information when applicable. The \code{observations} component records \code{input}, \code{retained},
    and \code{omitted} row counts for the data supplied to this
    \code{varpro} call.}
}

\note{
  For reproducible serial analyses, call \code{set.seed} immediately
  before \code{varpro} and use \code{parallel = FALSE}. The
  \code{seed} argument controls only the forest calls to which it is
  passed. Rule resampling and lasso folds also use random numbers.
  Calling \code{importance} again can resample rules. Save its result
  to inspect or reuse the same summary without recalculating it.

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
