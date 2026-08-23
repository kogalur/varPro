\name{outpro}
\alias{outpro}
\alias{outpro.null}
\title{Subspace-aware out-of-distribution scoring with outPro}

\description{

\code{outpro} computes an out-of-distribution (OOD) distance for new
inputs using the predictor representation and variable priorities from a
fitted \code{varpro} object, or the predictor representation from an
\code{rfsrc} grow object.  The score is subspace aware: departures are
measured in selected coordinates, optionally weighted by model-derived
variable priority, rather than by an unweighted global distance over all
features.

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
  
  \item{object}{A fitted \code{varpro} object or an \code{rfsrc} object
    with classes \code{c("rfsrc", "grow")}.}
  
  \item{newdata}{New data to score. If omitted, the training design
  matrix is scored. For \code{varpro} objects and
  \code{newdata.xscale = FALSE}, \code{newdata} should be supplied on
  the original data scale and is aligned to the fitted training design by
  the package's hot-encoding helper. If \code{newdata.xscale = TRUE},
  \code{newdata} is assumed to already be on the fitted \code{varpro}
  x-scale.}

  \item{neighbor}{Number of training neighbors per case. If \code{NULL},
  the default is \code{min(n/10, 5000)}, where \code{n} is the number of
  training rows. For \code{distancef = "knn"}, this is the number of
  ordinary nearest neighbors used in the selected standardized subspace.
  For the other distance functions, this is the number of model-derived
  neighbors requested from \code{varpro.strength}.}

  \item{distancef}{Distance function. One of \code{"knn"},
  \code{"prod"}, \code{"euclidean"}, \code{"mahalanobis"},
  \code{"manhattan"}, \code{"minkowski"}, or \code{"kernel"}. The
  default is \code{"knn"}. The \code{"knn"} option uses an ordinary
  weighted Manhattan nearest-neighbor distance in the selected subspace
  and does not call \code{varpro.strength}. The remaining options use
  model-derived neighbor frames and then aggregate coordinate-wise
  deviations across those neighbors.}

  \item{reduce}{Controls variable selection and weighting. If
  \code{TRUE} with a \code{varpro} object, variables are selected using
  model-based variable priority and the threshold \code{cutoff}. If
  \code{TRUE} with an \code{rfsrc} object, all predictors are used with
  unit weights. A character vector selects variables by name. A named
  numeric vector selects variables by name and supplies their weights.
  Any other value uses all predictors with unit weights.}
  
  \item{cutoff}{Threshold used with \code{varpro} variable-priority
  \code{z} values when \code{reduce = TRUE}. If \code{NULL}, a default
  based on the number of fitted predictor columns is used: \code{.79}
  for moderate dimension and \code{0} for high dimension. This is not an
  OOD acceptance threshold; it controls only the reduced scoring
  subspace.}

  \item{max.rules.tree}{Maximum number of rules per tree used by
    \code{varpro.strength} for model-derived neighbor extraction. Ignored
    when \code{distancef = "knn"}.}
  
  \item{max.tree}{Maximum number of trees used by \code{varpro.strength}
    for model-derived neighbor extraction. Ignored when
    \code{distancef = "knn"}.}

  \item{knn.chunk.size}{Positive integer giving the number of scored rows
  processed at one time by the \code{"knn"} distance calculation. Smaller
  values use less memory; larger values can be faster. Ignored by the
  non-KNN distance functions.}

  \item{newdata.xscale}{Logical value used only for \code{varpro}
  objects. If \code{FALSE}, the default, \code{newdata} is treated as raw
  user data and hot-encoded/aligned to the fitted training design. If
  \code{TRUE}, \code{newdata} is assumed to already contain the fitted
  x-scale columns, such as a matrix created internally from
  \code{object$x}. This option is mainly intended for internal package
  use, for example when scoring virtual data already constructed on the
  fitted \code{varpro} x-scale.}

  \item{nulldata}{For \code{outpro.null}, optional in-distribution
  reference data used to form a null/reference distance distribution. If
  omitted, the training design matrix is used.}

  \item{nulldata.xscale}{Logical value used only for \code{varpro}
  objects when \code{nulldata} is supplied. It has the same meaning for
  \code{nulldata} as \code{newdata.xscale} has for \code{newdata}.}

}

\details{

For a \code{varpro} object, the training design \code{object$x} is the
canonical fitted x-scale. When the original data include factors,
\code{object$x} stores the hot-encoded representation used by the fitted
forest. Ordinary user calls should normally leave
\code{newdata.xscale = FALSE}, in which case new data are hot-encoded and
aligned to the fitted training columns. Internal code can set
\code{newdata.xscale = TRUE} when the data to score have already been
constructed on the same fitted x-scale.

The default distance function is \code{"knn"}. It standardizes the
selected variables using training means and standard deviations, computes
weighted Manhattan distances from each scored input to the training
inputs in the selected subspace, and returns the average distance to the
\code{neighbor} closest training points. When \code{newdata} is omitted
and the training data are scored against themselves, the exact self match
is excluded from the nearest-neighbor set.

The non-KNN distance functions use model-derived reference neighborhoods.
For these metrics, \code{outpro} calls \code{varpro.strength} on the
fitted forest, extracts local neighbor frames, computes standardized
coordinate-wise absolute deviations from each scored case to its
neighbors, and aggregates those deviations by the requested metric.
Available metrics are multiplicative product, weighted Euclidean,
Mahalanobis, Manhattan, Minkowski, and kernel distance.

For \code{varpro} objects with \code{reduce = TRUE}, the selected subspace
is determined from variable-priority \code{z} values using \code{cutoff}.
If one or fewer variables pass the cutoff, all variables from the
priority table are used. A character \code{reduce} value bypasses this
step and directly specifies the scoring variables. A named numeric
\code{reduce} value directly specifies both the scoring variables and
their relative weights.

All distances are computed after standardizing selected variables with
training means and scales. Variables with zero training standard
deviation are dropped automatically. Larger \code{distance} values mean
that the scored case is farther from the in-distribution reference set in
the selected subspace.

\code{outpro.null} calls \code{outpro} on a reference sample and augments
the result with the empirical CDF of the reference distances. This is
useful for calibrating a distance to a reference quantile or for
constructing a support score such as \code{1 - F_0(distance)}.

}

\value{

\code{outpro} returns a list with components:
  
\itemize{
  \item \code{distance}: numeric vector with one OOD distance per scored case.
  \item \code{distance.object}: ingredients used for distance computation, including
    \itemize{
      \item \code{score}: neighbor frames returned by \code{varpro.strength}; \code{NULL} for \code{distancef = "knn"}.
      \item \code{neighbor}: neighbor count used.
      \item \code{oob.bits}: indicator of whether scoring was done on training rows or new data.
      \item \code{xvar.names}: selected variable names after zero standard-deviation removal.
      \item \code{xvar.wt}: variable weights after internal normalization and squaring.
      \item \code{dist.xvar}: list of absolute coordinate-difference matrices in standardized units; \code{NULL} for \code{distancef = "knn"}.
      \item \code{xorg.scale}, \code{xnew.scale}: standardized training and scored matrices for the selected variables.
      \item \code{means}, \code{sds}: training means and scales for the selected variables.
      \item \code{dropped.zero.sd.variables}: variables removed because their training standard deviation was zero.
    }
  \item \code{distance.args}: list of metric arguments actually used. For \code{distancef = "knn"}, this includes the effective neighbor count, whether self matches were excluded, and \code{knn.chunk.size}.
  \item \code{score}: the neighbor information returned by \code{varpro.strength}; \code{NULL} for \code{distancef = "knn"}.
  \item \code{neighbor}: neighbor setting used.
  \item \code{cutoff}: cutoff used for variable-priority reduction.
  \item \code{oob.bits}: indicator of whether scoring was done on training rows or new data.
  \item \code{selected.variables}: variables used in scoring after all filters.
  \item \code{selected.weights}: final selected-variable weights.
  \item \code{dropped.zero.sd.variables}: variables removed due to zero standard deviation.
  \item \code{means}, \code{sds}: training means and scales for convenience.
  \item \code{newdata.xscale}: logical value indicating whether supplied \code{newdata} was treated as already on the fitted x-scale.
  \item \code{call}: the matched call.
}

\code{outpro.null} returns the same list with two additional components:

\itemize{
  \item \code{cdf}: the empirical distribution function of \code{distance}.
  \item \code{quantile}: the empirical cumulative probability for each reference case.
}

}

\section{Background}{

  The method follows a model-centered view of OOD detection. The goal is
  not to estimate a full feature density, but to measure whether an input
  is well supported in the feature directions used by the fitted
  prediction rule. The default KNN score provides a fast selected-subspace
  support distance, while the non-KNN metrics use the forest structure to
  define model-derived local neighborhoods before aggregating coordinate
  deviations.
	    
}

\seealso{
\code{\link{varpro}}, \code{\link[randomForestSRC]{rfsrc}}.
}

\examples{

\donttest{

## ------------------------------------------------

## fit a varPro model
data(BostonHousing, package = "mlbench")
smp <- sample(1:nrow(BostonHousing), size = nrow(BostonHousing) * .75)
train.data <- BostonHousing[smp,]
test.data <- BostonHousing[-smp,]
vp <- varpro(medv ~ ., data = train.data)

## Score new data with the default KNN metric
op <- outpro(vp, newdata = test.data)
head(op$distance)

## Forest-neighborhood multiplicative distance
op.prod <- outpro(vp, newdata = test.data, distancef = "prod")
head(op.prod$distance)

## Calibrate a null/reference distribution using training data
op.null <- outpro.null(vp)
head(op.null$quantile)

}
}
