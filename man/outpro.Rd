\name{outpro}
\alias{outpro}
\alias{outpro.null}
\title{Out-of-Distribution Scoring with outPro}

\description{

\code{outpro} assesses how well observations are supported by the
training data in a selected set of predictors. With a \code{varpro}
object, variable priorities guide the selection and weighting of these
predictors. Larger distances indicate greater departure from the
training data. \code{outpro.null} provides a reference distribution for
interpreting the distances.

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

  \item{object}{A \code{varpro} object, or an \code{rfsrc} grow object
  with classes \code{c("rfsrc", "grow")}.}

  \item{newdata}{Data frame of observations to score. For a
  \code{varpro} object, supply predictors in their original form;
  factor predictors are hot-encoded and aligned to the training
  predictors automatically. For an \code{rfsrc} object, supply the
  same predictor columns as in the training data; predictors selected
  for distance calculation must be numeric. The response is not needed. If omitted, the training observations are scored.}

  \item{neighbor}{Number of training neighbors used for each
  observation. The default is \code{min(n / 10, 5000)}, where \code{n}
  is the number of training observations. The value is rounded and
  restricted to the available number of training observations. With
  \code{distancef = "knn"}, these are nearest neighbors in the
  selected predictor subspace. Other distance functions use
  forest-derived neighbors.}

  \item{distancef}{Distance function. The default, \code{"knn"},
  averages weighted Manhattan distances to the nearest training
  observations. The alternatives \code{"prod"}, \code{"euclidean"},
  \code{"mahalanobis"}, \code{"manhattan"}, \code{"minkowski"}, and
  \code{"kernel"} use forest-derived neighborhoods. See Details.}

  \item{reduce}{Variable selection and weighting. Use \code{TRUE}
  for automatic selection using varPro variable priorities,
  \code{FALSE} for all predictors with equal weights, a character
  vector for a specified set of predictors with equal weights, or a
  named numeric vector to specify predictors and their relative
  weights. For an \code{rfsrc} object, \code{TRUE} uses all predictors
  with equal weights. See Details for how priorities and supplied
  weights determine the distance.}

  \item{cutoff}{Minimum variable-priority \code{z} value for automatic
  selection when \code{object} is a \code{varpro} object and
  \code{reduce = TRUE}. The default is \code{0.79} when the training
  predictor representation has at most 250 columns and \code{0}
  otherwise. If fewer than two variables meet the threshold, all
  variables in the priority table are used. This argument controls
  variable selection; reference percentiles from \code{outpro.null}
  can be used to identify unusually large distances.}

  \item{max.rules.tree}{Maximum number of rules per tree used to
  construct forest-derived neighborhoods. Ignored when
  \code{distancef = "knn"}.}

  \item{max.tree}{Maximum number of trees used to construct
  forest-derived neighborhoods. Ignored when
  \code{distancef = "knn"}.}

  \item{knn.chunk.size}{Positive integer giving the number of
  observations processed together for \code{distancef = "knn"}.
  Smaller values reduce temporary memory use. This changes how the
  calculation is organized, while leaving the score unchanged.
  Ignored by the other distance functions.}

  \item{newdata.xscale}{Advanced option for \code{varpro} objects.
  Leave \code{FALSE} for data supplied in their original form. Set
  \code{TRUE} only when \code{newdata} already contains the encoded
  training predictor columns, as in \code{object$x}. Training-based
  standardization is still applied when distances are computed.}

  \item{nulldata}{Optional data frame representing the population
  regarded as in distribution. These observations are scored against
  the training data to obtain a reference distribution of distances.
  Supply predictors in the same form as \code{newdata}. If omitted,
  the training observations provide the reference distances.}

  \item{nulldata.xscale}{Advanced option for \code{varpro} objects.
  Has the same meaning for \code{nulldata} as \code{newdata.xscale}
  has for \code{newdata}.}

}

\details{

\subsection{Method}{

outPro assesses whether an observation is close to the training data in
a predictor subspace chosen for the modeling problem. For a
\code{varpro} object, variable priorities guide both predictor selection
and weighting. The resulting distance emphasizes departures in
predictors that varPro identifies as important.

The default method, \code{distancef = "knn"}, standardizes the selected
predictors using their training means and standard deviations. It then
finds the \code{neighbor} training observations closest to each scored
observation, using weighted Manhattan distance. The score is the mean
distance to these neighbors. Smaller scores indicate closer training
support; larger scores indicate greater separation. Smaller
neighborhoods emphasize immediate local support, while larger
neighborhoods average over a broader region of the training data.

}

\subsection{Choosing predictors and weights}{

Use \code{reduce = TRUE} for automatic varPro selection based on
\code{cutoff}, or \code{reduce = FALSE} to assess departures across all
predictors with equal weights. A character vector, such as
\code{reduce = c("x1", "x2")}, restricts scoring to those predictors
with equal weights.

A named numeric vector specifies predictors and relative weights.
Priorities and supplied weights are squared and normalized to sum to
one for the distance calculation. For example,
\code{reduce = c(x1 = 2, x2 = 1)} gives distance weights of \code{0.8}
and \code{0.2}. Supply nonnegative relative weights, with at least one
positive value.

Names supplied through \code{reduce} must match the training predictor
columns, including hot-encoded names in \code{object$x} for
\code{varpro} objects. Predictors with zero or nonfinite training
standard deviation are omitted. Inspect \code{selected.variables} and
\code{distance.args$weights.used} in the result to see the predictors
and weights used.

}

\subsection{Scoring observations}{

Use \code{outpro(object, newdata = newdata)} to score new observations.
The returned \code{distance} vector contains one score per row, in the
same order as \code{newdata}.

To score the training observations, omit \code{newdata}. The default
KNN method then excludes each observation from its own neighborhood,
leaving at most \code{n - 1} neighbors. Supplying the training data
explicitly as \code{newdata} retains self matches.

}

\subsection{Interpreting distances using a reference sample}{

\code{outpro.null} provides an empirical reference distribution for
interpreting distances. Supply \code{nulldata} to use a separate sample
representing the population regarded as in distribution, or omit it to
use the training observations. The model's training data remain the
source of neighbors; \code{nulldata} supplies the observations whose
distances form the reference distribution.

For a reference result \code{ref} and a scoring result \code{op},
\code{ref$cdf(op$distance)} gives each observation's reference
percentile. A value of \code{0.99} means that 99 percent of the
reference distances are no larger than that observation's distance.
Large percentiles therefore identify unusually weak training support
relative to the reference sample. A rule such as
\code{ref$cdf(op$distance) > 0.95} flags observations in the upper tail
of this distribution.

The corresponding support score, \code{1 - ref$cdf(op$distance)}, is
the fraction of reference distances greater than the scored distance.
Smaller values indicate less support. These summaries describe position
within the empirical reference distribution.

Use the same model, predictors, weights, neighborhood size, and distance
function for reference and new observations. For forest-based distances,
also keep \code{max.tree} and \code{max.rules.tree} the same. The
examples illustrate the default KNN calibration workflow.

}

\subsection{Forest-based distances}{

The other \code{distancef} options use neighborhoods defined by the
forest. Each score summarizes standardized differences between the
observation and its forest-derived neighbors:

\describe{
  \item{\code{"prod"}}{Mean across neighbors of the weighted
  geometric mean of absolute coordinate differences, with a small
  positive offset.}

  \item{\code{"euclidean"}}{Mean weighted Euclidean distance.}

  \item{\code{"manhattan"}}{Mean weighted Manhattan distance. This
  uses the same pairwise metric as \code{"knn"}, but with
  forest-derived neighbors.}

  \item{\code{"minkowski"}}{Mean weighted Minkowski distance of
  order 4.}

  \item{\code{"mahalanobis"}}{Mean Mahalanobis-type distance based
  on weighted absolute coordinate differences and the covariance of
  the standardized training predictors.}

  \item{\code{"kernel"}}{One minus the mean Gaussian similarity
  to the neighbors, based on weighted squared Euclidean distances.
  The bandwidth is estimated separately for each scoring call, so
  scores from separate calls can have different scales.}
}

}

}

\value{

\code{outpro} returns a list with the following components:

\item{distance}{Numeric vector of distances, one per scored
observation. Larger values indicate greater departure from the
training data in the selected predictor subspace.}

\item{selected.variables}{Predictor names used in scoring, after
removing variables with zero or nonfinite training standard deviation.}

\item{selected.weights}{Relative predictor weights after rescaling
and squaring. These precede the final normalization to sum to one;
\code{distance.args$weights.used} contains the weights used in the
distance calculation.}

\item{distance.args}{Distance settings used, including
\code{distancef} and \code{weights.used}. For \code{"knn"}, this
also contains the effective neighbor count
(\code{knn.neighbor.used}), whether self matches were excluded
(\code{knn.self.excluded}), and \code{knn.chunk.size}.}

\item{neighbor}{Requested neighborhood size after rounding and
restriction to the training sample size. For KNN training scores,
\code{distance.args$knn.neighbor.used} records any further reduction
to exclude self matches.}

\item{cutoff}{Variable-priority threshold setting. It affects
selection only for \code{varpro} objects with \code{reduce = TRUE}.}

\item{means, sds}{Training means and standard deviations for the
predictors used in scoring.}

\item{dropped.zero.sd.variables}{Names of predictors removed because
their training standard deviations were zero or nonfinite.}

\item{distance.object}{A list retaining the distance information,
including standardized training and scored predictors
(\code{xorg.scale} and \code{xnew.scale}), selected predictor names
and relative weights (\code{xvar.names} and \code{xvar.wt}), training
means and standard deviations, and neighborhood information. For
forest-based distances, \code{dist.xvar} contains absolute standardized
coordinate differences to the neighbors; it is \code{NULL} for
\code{"knn"}.}

\item{score}{Forest-derived neighborhood information. \code{NULL}
for \code{distancef = "knn"}.}

\item{oob.bits}{Scoring-mode indicator: \code{0} when \code{newdata}
is omitted and \code{1} when it is supplied.}

\item{newdata.xscale}{Whether supplied data were treated as already
expressed in the model's encoded predictor columns.}

\item{call}{The matched function call.}

\code{outpro.null} returns the same components, together with:

\item{cdf}{The empirical cumulative distribution function of the
reference distances. Apply this function to new distances to obtain
their reference percentiles.}

\item{quantile}{The empirical cumulative probability of each reference
observation's distance, computed as \code{cdf(distance)}.}

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

## Use all predictors with equal weights.
op.all <- outpro(vp, newdata = test.data, reduce = FALSE)

## Specify a subspace with equal weights.
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
