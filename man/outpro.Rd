\name{outpro}
\alias{outpro}
\alias{outpro.null}
\title{Out-of-Distribution Scoring with outPro}

\description{
  \code{outpro} identifies observations whose predictor values differ
  from those in the training data. It returns a distance score for
  each observation, with larger scores indicating greater differences
  in the predictors used for scoring. With a \code{varpro} model,
  variable priorities guide predictor selection and weighting.
  \code{outpro.null} supplies a reference distribution for deciding
  which scores are unusually large.
}

\usage{
outpro(object,
       newdata,
       neighbor = NULL,
       distancef = "knn",
       reduce = TRUE,
       cutoff = NULL,
       max.rules.tree = 150,
       max.tree = 150,
       knn.chunk.size = 100L,
       newdata.xscale = FALSE)

outpro.null(object,
            nulldata = NULL,
            neighbor = NULL,
            distancef = "knn",
            reduce = TRUE,
            cutoff = NULL,
            max.rules.tree = 150,
            max.tree = 150,
            knn.chunk.size = 100L,
            nulldata.xscale = FALSE)
}

\arguments{
  \item{object}{A fitted \code{varpro} object, or an \code{rfsrc}
    grow object with classes \code{c("rfsrc", "grow")}.}

  \item{newdata}{Data frame of observations to score. The response
    is not needed. If omitted, the training observations are scored.
    For a \code{varpro} object, supply predictors in their original
    form; factors are converted to the training model's indicator
    columns automatically. For an \code{rfsrc} object, supply the
    training predictor columns; those selected for distance
    calculation must be numeric.}

  \item{neighbor}{Number of training neighbors used for each
    observation. With \code{NULL}, the number is chosen from the
    training sample size. The value is rounded and limited to the
    number of available observations. The default distance method
    chooses the nearest training observations; the other methods
    use neighbors selected by the forest.}

  \item{distancef}{How distances are calculated. The default,
    \code{"knn"}, averages weighted Manhattan distances to the
    nearest training observations. The alternatives \code{"prod"},
    \code{"euclidean"}, \code{"mahalanobis"}, \code{"manhattan"},
    \code{"minkowski"}, and \code{"kernel"} use neighbors selected
    by the forest. See Details.}

  \item{reduce}{Which predictors to use and how to weight them.
    Use \code{TRUE} for automatic selection using varPro variable
    priorities, \code{FALSE} for all predictors in the fitted forest
    with equal weights, a character vector for named predictors with
    equal weights, or a named numeric vector for predictors and
    relative weights. For an \code{rfsrc} object, \code{TRUE} also
    uses all predictors with equal weights. See Details.}

  \item{cutoff}{Minimum variable-priority \code{z} score for
    automatic selection with a \code{varpro} object and
    \code{reduce = TRUE}. With \code{NULL}, the cutoff is chosen
    automatically. If fewer than two predictors meet the cutoff,
    all predictors in the priority table are used. This cutoff
    selects predictors; use reference percentiles from
    \code{outpro.null} to flag unusually large distances.}

  \item{max.rules.tree}{Maximum number of rules per tree used to
    select forest neighbors. Ignored when \code{distancef = "knn"}.}

  \item{max.tree}{Maximum number of trees used to select forest
    neighbors. Ignored when \code{distancef = "knn"}.}

  \item{knn.chunk.size}{Positive integer giving the number of
    observations processed together for \code{distancef = "knn"}.
    Smaller values reduce temporary memory use without changing the
    scores. Ignored by the other distance methods.}

  \item{newdata.xscale}{Advanced option for \code{varpro} objects.
    Leave \code{FALSE} for data in their original form. Set
    \code{TRUE} only when \code{newdata} already contains the
    model's encoded predictor columns. Distances still use the
    training means and standard deviations to standardize values.}

  \item{nulldata}{Optional data frame of observations considered
    typical of the population being modeled. Their distances from
    the training data form the reference distribution. Supply
    predictors in the same form as \code{newdata}. With \code{NULL},
    the training observations provide the reference distances.}

  \item{nulldata.xscale}{Advanced option for \code{varpro} objects.
    Has the same meaning for \code{nulldata} as \code{newdata.xscale}
    has for \code{newdata}.}
}

\details{
  outPro helps identify observations that differ from the data used
  to fit a model. It compares each observation with training
  observations using a selected set of predictors. With a
  \code{varpro} model, the default selection and weights emphasize
  predictors that are important to the response.

  Use \code{outpro} to calculate distance scores, then
  \code{outpro.null} to compare them with scores from a reference
  sample of typical observations. Large distances relative to this
  reference identify observations that are unusual in the predictors
  used for scoring.

  \subsection{Scoring observations}{
    Use \code{op <- outpro(object, newdata = test)} to score new
    observations. The vector \code{op$distance} contains one score
    per row of \code{test}, in the same order.

    The default method, \code{distancef = "knn"}, finds the
    \code{neighbor} closest training observations and averages their
    distances. Each distance is a weighted sum of absolute predictor
    differences, called a weighted Manhattan distance. Predictors
    are first standardized using their training means and standard
    deviations, so differences are measured in training standard
    deviations rather than the predictors' original units.

    Smaller values of \code{neighbor} focus on the closest
    observations; larger values average over a broader group. To
    score the training observations, omit \code{newdata}. The
    default method then excludes each observation from its own
    neighbors, leaving at most \code{n - 1} neighbors for a training
    sample of size \code{n}. Supplying the training data explicitly
    as \code{newdata} allows each observation to match itself.
  }

  \subsection{Comparing scores with a reference sample}{
    Use \code{ref <- outpro.null(object, nulldata = reference.data)}
    to score a separate sample of typical observations. Its
    distances provide a reference for interpreting new scores.
    The neighbors still come from the model's training data;
    \code{nulldata} supplies the observations whose scores form
    the reference distribution. Omitting \code{nulldata} uses the
    training scores as the reference instead.

    For a scoring result \code{op}, \code{ref$cdf(op$distance)}
    gives each observation's reference percentile on a zero-to-one
    scale. A value of \code{0.99} means that 99 percent of reference
    distances are no larger than that observation's distance.
    For example, \code{ref$cdf(op$distance) > 0.95} flags
    observations with reference percentiles above \code{0.95}.

    The support score \code{1 - ref$cdf(op$distance)} is the
    fraction of reference distances greater than the scored
    distance. Smaller values indicate less similarity to the
    training data relative to the reference sample. Both summaries
    describe position within the reference distribution.

    Use the same model, predictors, weights, \code{neighbor}, and
    \code{distancef} for reference and new observations. For
    forest-based distances, also use the same \code{max.tree} and
    \code{max.rules.tree}. Recalculate the reference scores when
    changing these settings. See the note on \code{"kernel"} below
    about comparing scores from separate calls.
  }

  \subsection{Choosing predictors and weights}{
    With \code{reduce = TRUE}, varPro priorities determine which
    predictors to use and give more weight to higher-priority
    predictors. The \code{cutoff} argument controls this selection.
    Use \code{reduce = FALSE} to give equal weight to all predictors
    in the fitted forest. Predictors removed during model fitting
    are outside this set. For an \code{rfsrc} object, both
    \code{TRUE} and \code{FALSE} use all forest predictors with
    equal weights.

    Automatic selection treats a factor as one predictor. Its
    indicator columns present in the fitted forest are then used
    for scoring, each with the factor's priority as its relative
    weight.

    A character vector, such as \code{reduce = c("x1", "x2")},
    selects named predictors with equal weights. A named numeric
    vector supplies relative weights. Priorities and supplied
    weights are squared and normalized to sum to one. For example,
    \code{reduce = c(x1 = 2, x2 = 1)} gives distance weights of
    \code{0.8} and \code{0.2}. Supply nonnegative weights, with at
    least one positive value.

    Names supplied in \code{reduce} must match the fitted forest's
    predictor columns: \code{object$rf$xvar.names} for a
    \code{varpro} object or \code{object$xvar.names} for an
    \code{rfsrc} object. For factors converted to indicator columns,
    use those column names. Predictors with zero or nonfinite
    training standard deviation are omitted. The returned
    \code{selected.variables} and \code{distance.args$weights.used}
    identify the predictors and weights used in the calculation.
  }

  \subsection{Other distance methods}{
    The other \code{distancef} options use neighbors selected by
    the forest rather than the closest observations under the
    default distance. They also standardize predictors using the
    training means and standard deviations.

    \describe{
      \item{\code{"prod"}}{Averages weighted geometric means of
        absolute predictor differences across neighbors. A small
        positive constant is added to each difference before
        multiplication.}

      \item{\code{"euclidean"}}{Averages weighted straight-line
        (Euclidean) distances to the neighbors.}

      \item{\code{"manhattan"}}{Averages weighted sums of absolute
        predictor differences. This uses the same distance
        calculation as \code{"knn"}, but different neighbors.}

      \item{\code{"minkowski"}}{Averages weighted Minkowski
        distances of order 4: the fourth root of the weighted sum
        of fourth powers of absolute predictor differences.}

      \item{\code{"mahalanobis"}}{Averages Mahalanobis-type
        distances, using the covariance of standardized training
        predictors to adjust weighted absolute predictor
        differences.}

      \item{\code{"kernel"}}{Calculates one minus the average
        Gaussian similarity to the neighbors. Similarity is based
        on weighted squared Euclidean distances. Its scale is
        estimated separately for each scoring call, so scores from
        separate calls can have different scales.}
    }
  }
}

\value{
  \code{outpro} returns a list with the following components:

  \item{distance}{Numeric vector with one score per observation.
    Larger values indicate greater differences from the training
    data in the predictors used for scoring.}

  \item{selected.variables}{Names of predictors used in scoring,
    after omitting those with zero or nonfinite training standard
    deviation.}

  \item{selected.weights}{Relative predictor weights after
    rescaling and squaring. These are normalized to sum to one
    for scoring; the final weights are in
    \code{distance.args$weights.used}.}

  \item{distance.args}{Distance settings, including \code{distancef}
    and \code{weights.used}. For \code{"knn"}, also includes the
    number of neighbors used (\code{knn.neighbor.used}), whether
    observations were excluded from their own neighbors
    (\code{knn.self.excluded}), and \code{knn.chunk.size}.}

  \item{neighbor}{Number of neighbors requested, after rounding
    and limiting it to the training sample size. For KNN training
    scores, \code{distance.args$knn.neighbor.used} gives the number
    used after excluding each observation from its own neighbors.}

  \item{cutoff}{Variable-priority cutoff. Affects selection only
    for \code{varpro} objects with \code{reduce = TRUE}.}

  \item{means, sds}{Training means and standard deviations for
    the predictors used in scoring.}

  \item{dropped.zero.sd.variables}{Names of predictors omitted
    because their training standard deviations were zero or
    nonfinite.}

  \item{distance.object}{Data used to calculate distances. Includes
    standardized training and scored predictors (\code{xorg.scale}
    and \code{xnew.scale}), predictor names and relative weights
    (\code{xvar.names} and \code{xvar.wt}), training means and
    standard deviations, and neighbor information. For forest-based
    methods, \code{dist.xvar} contains absolute predictor differences
    in standardized units; it is \code{NULL} for \code{"knn"}.}

  \item{score}{Information about neighbors selected by the forest.
    This is \code{NULL} for \code{distancef = "knn"}; the outlier
    scores are in \code{distance}.}

  \item{oob.bits}{\code{0} when \code{newdata} is omitted and
    \code{1} when it is supplied.}

  \item{newdata.xscale}{Whether supplied data were treated as
    already converted to the model's encoded predictor columns.}

  \item{call}{The matched function call.}

  \code{outpro.null} returns the same components, together with:

  \item{cdf}{Empirical cumulative distribution function of the
    reference distances. Use \code{ref$cdf(op$distance)} to obtain
    reference percentiles for new scores.}

  \item{quantile}{Reference percentiles for the reference
    observations themselves, on a zero-to-one scale. Computed as
    \code{cdf(distance)}.}
}

\seealso{
\code{\link{varpro}}, \code{\link[randomForestSRC]{rfsrc}}.
}

\examples{
\donttest{

## Simulate a regression problem with two signal and two noise variables.
set.seed(123)
n <- 800
dta <- data.frame(x1 = rnorm(n), x2 = rnorm(n),
                  noise1 = rnorm(n), noise2 = rnorm(n))
dta$y <- 2 * dta$x1 + dta$x2^2 + rnorm(n)

## Use separate training, reference, and test samples.
vp <- varpro(y ~ ., data = dta[1:500, ])
reference.data <- dta[501:650, ]
test.data <- dta[651:800, ]

## Shift one signal variable in half of the test observations.
shifted <- seq_len(nrow(test.data)) > 75
test.data$x1[shifted] <- test.data$x1[shifted] + 6

## Score test observations using the default KNN method.
op <- outpro(vp, newdata = test.data)
print(op$selected.variables)
print(setNames(op$distance.args$weights.used, op$selected.variables))

## Compare the test distances with an in-distribution reference sample.
ref <- outpro.null(vp, nulldata = reference.data)
percentile <- ref$cdf(op$distance)
support <- 1 - percentile
flag <- percentile > 0.95

print(head(data.frame(distance = op$distance, percentile = percentile,
                      support = support, flag = flag)))
print(tapply(percentile, shifted, median))
print(table(shifted = shifted, flagged = flag))

## Alternatively, use training distances as the reference distribution.
## Omit nulldata so each training observation excludes itself in KNN scoring.
ref.train <- outpro.null(vp)
print(head(ref.train$cdf(op$distance)))

## Use all predictors in the fitted forest with equal weights.
op.all <- outpro(vp, newdata = test.data, reduce = FALSE)

## Specify predictors with equal weights.
op.subspace <- outpro(vp, newdata = test.data,
                      reduce = c("x1", "x2"))

## Specify relative weights. Squaring and normalization give 0.8 and 0.2.
op.weighted <- outpro(vp, newdata = test.data,
                      reduce = c(x1 = 2, x2 = 1))
print(op.weighted$distance.args$weights.used)

## Use forest-derived neighborhoods with a product distance.
op.prod <- outpro(vp, newdata = test.data, distancef = "prod")
print(head(op.prod$distance))

## Recompute the reference distribution when changing scoring settings.
ref.prod <- outpro.null(vp, nulldata = reference.data, distancef = "prod")
print(head(ref.prod$cdf(op.prod$distance)))

## Compare all five methods on the same test observations.
## AUC measures discrimination using raw distances, with larger values
## identifying shifted observations. Tied distances receive average ranks.
## At a 0.95 reference-percentile cutoff, report the false-positive rate
## (FPR) among unshifted observations and the true-positive rate (TPR)
## among shifted observations, using each reference strategy.
compare.performance <- function(op, ref, ref.train) {
  distance <- op$distance
  n1 <- sum(shifted)
  n0 <- sum(!shifted)
  auc <- (sum(rank(distance, ties.method = "average")[shifted]) -
          n1 * (n1 + 1) / 2) / (n1 * n0)

  flag.reference <- ref$cdf(distance) > 0.95
  flag.training <- ref.train$cdf(distance) > 0.95

  c(AUC = auc,
    FPR.reference = mean(flag.reference[!shifted]),
    TPR.reference = mean(flag.reference[shifted]),
    FPR.training = mean(flag.training[!shifted]),
    TPR.training = mean(flag.training[shifted]))
}

## Each reference distribution uses the same scoring settings as its
## corresponding test scores. Reuse the references already computed.
performance <- rbind(
  KNN.varPro = compare.performance(op, ref, ref.train),
  KNN.all = compare.performance(
    op.all,
    outpro.null(vp, nulldata = reference.data, reduce = FALSE),
    outpro.null(vp, reduce = FALSE)),
  KNN.signal = compare.performance(
    op.subspace,
    outpro.null(vp, nulldata = reference.data,
                reduce = c("x1", "x2")),
    outpro.null(vp, reduce = c("x1", "x2"))),
  KNN.weighted = compare.performance(
    op.weighted,
    outpro.null(vp, nulldata = reference.data,
                reduce = c(x1 = 2, x2 = 1)),
    outpro.null(vp, reduce = c(x1 = 2, x2 = 1))),
  Forest.product = compare.performance(
    op.prod, ref.prod, outpro.null(vp, distancef = "prod"))
)
print(round(performance, 3))

}
}
