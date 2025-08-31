\name{outpro}
\alias{outpro}
\alias{outpro.null}
\title{Model and subsapce aware out-of-distribution (OOD) scoring with outPro}

\description{

\code{outpro} computes an out-of-distribution (OOD) score for new
inputs using a fitted model, integrating variable prioritization and
local neighborhoods derived from the model. The procedure is model aware
and subspace aware: it scores departures in the coordinates that the
model has learned to rely on, rather than relying on a global distance
in the full feature space. Applicable across all outcome types.}

\details{

Out-of-distribution (OOD) detection is essential for determining when a
supervised model encounters inputs that differ in ways that matter for
prediction. The approach here embeds variable prioritization directly in
the detection step, constructing localized, task relevant neighborhoods
from the fitted model and aggregating coordinate wise deviations within
the selected subspace to obtain a distance value for an input.

For a \code{varpro} object, variable prioritization is obtained from the
model and controlled by \code{cutoff}. For an \code{rfsrc} object, all
predictors are used unless a reduction is supplied. Distances are
computed after standardizing the selected variables with training means
and scales. Variables with zero standard deviation in the training data
are removed automatically before scoring.

The multiplicative \code{"prod"} metric uses a small \eqn{\epsilon} to
avoid zero multiplicands. Since differences are measured on a
standardized scale, \eqn{\epsilon} is set automatically by default as a
small fraction of the median absolute coordinate difference across
variables and neighbors; users can keep the default or pass a custom
value via \code{out.distance} if calling it directly.

The Mahalanobis option uses absolute differences by design and the
covariance of standardized training features. A small ridge is added to
the covariance for numerical stability.

}

\usage{
outpro(object,
       newdata,
       neighbor = NULL,
       distancef = "prod",
       reduce = TRUE,
       cutoff = NULL,
       max.rules.tree = 150,
       max.tree = 150)

outpro.null(object,
            nulldata = NULL,
            neighbor = NULL,
            distancef = "prod",
            reduce = TRUE,
            cutoff = .79,
            max.rules.tree = 150,
            max.tree = 150)
}

\arguments{
  
  \item{object}{A fitted \code{varpro} object or an \code{rfsrc} object
    with classes \code{c("rfsrc","grow")}.}
  
  \item{newdata}{New data to score. If omitted, the training design
  matrix is used. For \code{varpro} objects, encodings are aligned to
  training with \code{get.hotencode.test}.}

  \item{neighbor}{Number of training neighbors per case, as determined
  by the model structure. If \code{NULL}, a default of \code{min(n/10,
    5000)} is used where \code{n} is the number of training rows.}

  \item{distancef}{Distance function for aggregation. One of
  \code{"prod"}, \code{"euclidean"}, \code{"mahalanobis"},
  \code{"manhattan"}, \code{"minkowski"}, \code{"kernel"}. The default
  is \code{"prod"}.}

  \item{reduce}{Controls variable selection. If \code{TRUE} with a
    \code{varpro} object, uses model based prioritization with threshold
    \code{cutoff}. A character vector selects variables by name. A named
    numeric vector supplies variable weights. Otherwise all predictors
    are used with unit weights.}
  
  \item{cutoff}{Threshold used with \code{varpro} variable importance
  \code{z}. If \code{NULL}, a default based on the number of predictors
  is used: \code{.79} when the number of predictors is not large, else
  \code{0}.}

  \item{max.rules.tree}{Maximum number of rules per tree for neighbor
    extraction.}
  
  \item{max.tree}{Maximum number of trees to use for neighbor
    extraction.}
  
  \item{nulldata}{For \code{outpro.null}, optional data representing an
  in distribution reference. If omitted, the training design matrix is
  used.}

}

\value{

  \code{outpro} returns a list with components:
  
\itemize{
  \item \code{distance}: numeric vector of length \code{nrow(newdata)} with one score per case.
  \item \code{distance.object}: ingredients used for distance computation, including
    \itemize{
      \item \code{score}: neighbor frames returned by \code{varpro.strength}.
      \item \code{neighbor}: neighbor count per case.
      \item \code{xvar.names}: selected variable names after zero sd removal.
      \item \code{xvar.wt}: variable weights used after normalization.
      \item \code{dist.xvar}: list of absolute coordinate difference matrices (neighbors by cases) in standardized units.
      \item \code{xorg.scale}, \code{xnew.scale}: standardized training and test matrices for the selected variables.
      \item \code{means}, \code{sds}: training means and scales for the selected variables.
      \item \code{dropped.zero.sd.variables}: variables removed due to zero standard deviation in training.
    }

  \item \code{distance.args}: list of metric arguments actually used,
      including \code{distancef}, \code{weights.used},
      \code{normalize.weights}, \code{p}, and \code{epsilon.used}.
  \item \code{score}: the neighbor information returned by \code{varpro.strength}.
  \item \code{neighbor}: neighbor setting used.
  \item \code{cutoff}: cutoff used for variable prioritization.
  \item \code{oob.bits}: indicator of whether scoring was done on training rows or new data.
  \item \code{selected.variables}: the variables used in scoring after all filters.
  \item \code{selected.weights}: the normalized squared weights for the selected variables.
  \item \code{means}, \code{sds}: duplicates for convenience.
  \item \code{call}: the matched call.

}

\code{outpro.null} returns the same list with two additional components:

\itemize{
  \item \code{cdf}: the empirical distribution function of \code{distance}.
  \item \code{quantile}: the empirical cumulative probability for each scored case.
}

}

\section{Background}{

  The method follows a model centered view of out-of-distribution (OOD)
  detection that is both model aware and subspace aware. Variable
  prioritization is embedded directly in the detection process to focus
  on coordinates that matter for prediction and to discount nuisance
  directions. Scoring does not rely on global feature density
  estimation. The implementation uses a random forest engine whose rule
  based structure provides localized neighborhoods reflecting the
  learned predictive mapping.
	    
}

\seealso{
\code{\link{varpro}}, \code{\link[randomForestSRC]{rfsrc}}.
}

\examples{
## ------------------------------------------------

## fit a varPro model
data(BostonHousing, package = "mlbench")
smp <- sample(1:nrow(BostonHousing), size = nrow(BostonHousing) * .75)
train.data <- BostonHousing[smp,]
test.data <- BostonHousing[-smp,]
vp <- varpro(medv ~ ., data = train.data)

## Score new data with default multiplicative metric
op <- outpro(vp, newdata = test.data)
head(op$distance)

## Calibrate a null distribution using training data
op.null <- outpro.null(vp)
head(op.null$quantile)

}
