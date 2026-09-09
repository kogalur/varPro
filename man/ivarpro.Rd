\name{ivarpro}
\alias{ivarpro}
\title{Individual Variable Priority: Case-Specific Local Gradients}

\description{
  Estimates how a prediction target changes with each predictor near
  individual training observations. Forest rules define the local
  comparisons, and local slopes provide signed, case-specific importance
  scores. Supports regression, classification, and survival analyses.
}

\usage{
ivarpro(object,
        adaptive = TRUE,
        cut = NULL,
        cut.max = 1,
        ncut = 51,
        nmin = 20, nmax = 150,
        y.external = NULL,
        noise.na = TRUE,
        max.rules.tree = NULL,
        max.tree = NULL,
        use.loo = TRUE,
        use.abs = FALSE,
        path.store.membership = TRUE,
        save.data = TRUE,
        save.model = TRUE,
        scale = c("local", "global", "none"))
}

\arguments{
  \item{object}{A \code{varpro} object, or a supervised \code{rfsrc}
    grow object with a single outcome and numeric predictors. Use a
    \code{varpro} object for categorical predictors and multivariate
    regression.}

  \item{adaptive}{Automatically limit the largest candidate neighborhood
    according to the sample size? Used when \code{cut} is not supplied.}

  \item{cut}{Optional vector of nonnegative neighborhood widths for
    continuous predictors, measured in standard deviations of the
    released-region predictor values. Supplied values are sorted and
    duplicates removed; \code{adaptive}, \code{cut.max}, and \code{ncut}
    are then ignored. Binary 0/1 predictors use a comparison of the two
    levels.}

  \item{cut.max}{Upper bound on candidate neighborhood width when
    \code{cut} is not supplied. Smaller values restrict estimation to
    more local observations.}

  \item{ncut}{Number of candidate widths when \code{cut} is not supplied.}

  \item{nmin}{Minimum number of usable observations for a local estimate.}

  \item{nmax}{Maximum number of observations used for a local estimate.
    The effective maximum is also limited according to the training
    sample size and is at least \code{nmin}.}

  \item{y.external}{Optional numeric vector or matrix containing the
    target values to explain. Rows must correspond, in order, to the
    processed training observations in \code{object}. A matrix supplies
    a separate target in each column; column names identify the targets.
    The default uses the rule-generating forest's out-of-bag predictions.}

  \item{noise.na}{Represent unavailable local estimates by \code{NA}?
    With \code{TRUE}, available rule estimates are averaged. With
    \code{FALSE}, unavailable rule estimates contribute zero, and cases
    with no applicable rules receive zero.}

  \item{max.rules.tree}{Maximum number of rules examined per tree.
    Defaults to the setting in a \code{varpro} object, or 150 for direct
    \code{rfsrc} input.}

  \item{max.tree}{Maximum number of trees used to obtain rules.
    Defaults to the setting in a \code{varpro} object, or 150 for direct
    \code{rfsrc} input.}

  \item{use.loo}{Select the continuous-predictor neighborhood by
    leave-one-out prediction error from the local regressions? With
    \code{FALSE}, use the largest valid candidate neighborhood, subject
    to \code{nmin} and \code{nmax}.}

  \item{use.abs}{Average absolute rule-level gradients? The default
    \code{FALSE} retains their signs.}

  \item{path.store.membership}{Retain rule and near-miss membership
    indices for downstream diagnostics? Setting \code{FALSE} reduces
    storage while preserving the returned gradients and ordinary plots.}

  \item{save.data}{Store processed predictors and prediction targets
    for plotting?}

  \item{save.model}{Store the supplied model for downstream use?}

  \item{scale}{Predictor scaling for the local slopes: \code{"local"}
    uses the selected neighborhood's spread, \code{"global"} uses the
    predictor's training-sample standard deviation, and \code{"none"}
    returns slopes per unit of the predictor. See Details.}
}

\details{
  \subsection{From forest rules to individual importance}{
    A variable's influence can change across observations. iVarPro
    summarizes this variation through local slopes of a prediction
    target, using forest rules to identify relevant observations.

    For a rule region \eqn{R}, releasing predictor \eqn{s} removes its
    restrictions and retains the restrictions on the other predictors.
    This gives the enlarged region \eqn{R^{(s)}}. The additional
    observations form the near-miss set
    \eqn{C_s = R^{(s)} \setminus R}.
    The local regression uses observations from both \eqn{R} and
    \eqn{C_s}, pooling them within the released region.

    For a continuous predictor, estimation uses a neighborhood around
    its mean among the rule's out-of-bag members. A local regression of
    the target on that predictor supplies a slope. For a binary 0/1
    predictor, the slope is the difference in mean target values between
    levels 1 and 0. Each case receives the average gradient from rules
    in whose original region it is an out-of-bag member and for which
    that predictor is released.
  }

  \subsection{Reading the scores}{
    A positive score indicates that larger predictor values locally
    accompany larger target values; a negative score indicates the
    opposite direction. Larger magnitudes represent stronger local
    changes on the chosen scale. Setting \code{use.abs = TRUE}
    summarizes strength by averaging magnitudes before opposing rule
    slopes can cancel.

    Scores are reported for the processed predictors in
    \code{object$xvar.names}. A hot-encoded factor can therefore have
    several indicator columns. An unavailable score means that the case
    has no usable local estimate for that predictor, for example because
    it lacks applicable rules or sufficient local variation.
  }

  \subsection{Prediction targets}{
    By default, regression explains out-of-bag predicted responses, and
    classification explains out-of-bag class probabilities. Binary
    classification returns scores for the first probability column;
    \code{attr(x, "target")} identifies the target in the returned
    object. Multiclass classification returns a named list with one
    gradient table per class.

    The target follows the forest stored in the model. A survival forest
    supplies mortality predictions. When VarPro uses a multivariate
    regression forest for RMST targets, each predicted target has its
    own gradient table. More generally, multivariate VarPro regression
    returns one table per predicted response.

    Use \code{y.external} to explain another numeric target, such as
    predictions from a different model or a particular class probability.
    A supplied matrix retains all of its target columns. Target values
    must be aligned with the observations retained by the original
    analysis.
  }

  \subsection{Scaling and locality}{
    With \code{scale = "local"}, the slope is multiplied by the
    selected predictor values' spread around the rule-region center.
    With \code{scale = "global"}, the multiplier is the predictor's
    standard deviation across the training observations. Both express
    the gradient in target units. With \code{scale = "none"}, the
    slope is in target units per predictor unit; for a binary predictor,
    it is the change from level 0 to level 1.

    Neighborhood size controls how locally the slope is estimated.
    The defaults choose among candidate neighborhoods using local
    leave-one-out prediction error. Reducing \code{cut.max} restricts
    the available neighborhoods; increasing \code{nmin} requires more
    observations for each estimate. For binary predictors, both levels
    are represented in a sample of the released-region observations,
    subject to the sample-size controls.
  }

  \subsection{Displaying local importance}{
    \code{shap.ivarpro(x)} gives a beeswarm-style summary of the local
    gradients. Position shows the gradient and color shows the predictor
    value. \code{plot(x, var = "x1")} displays one predictor's gradients
    against its values. Use \code{col.var} to color by another variable
    and examine how the local relationship varies across groups.

    For a list of target-specific tables, supply \code{target} by name
    or index to either plotting function. Without it, the first target
    is used with a warning. Stored data are used automatically; with
    \code{save.data = FALSE}, supply the corresponding processed data
    through \code{dat} to \code{shap.ivarpro} or \code{data} to
    \code{plot}.
  }
  \subsection{New observations}{
    Use \code{predict(x, newdata = test)} to obtain gradients for new
    cases from the stored rule estimates. Supply predictors in their
    original form; the training hot-encoding is reused. Omitting
    \code{newdata} recovers out-of-bag training scores. See
    \code{\link{predict.ivarpro}} for an example.
  }
}

\value{
  An object of class \code{ivarpro}. A single target produces a numeric
  data frame with one row per processed training observation and one
  column per predictor in \code{object$xvar.names}. Row names retain
  the processed training-row identifiers. Multiple targets produce a
  named list of these data frames. \code{print(x)} summarizes a list;
  \code{print(x, full = TRUE)} displays all of its tables.

  The \code{target} attribute identifies the target or targets. When
  requested, the \code{data} attribute contains processed predictors and
  the target values, and the \code{model} attribute contains the supplied
  model. Target columns in the stored data are named \code{y} or
  \code{y.<target>}, with suffixes added to avoid name collisions; their
  names are recorded in \code{attr(attr(x, "data"), "response.names")}.

  The \code{ivarpro.path} attribute retains neighborhood settings and
  diagnostics for the selected rule estimates. Membership indices are
  included when \code{path.store.membership = TRUE}. Ordinary use and
  plotting require only the gradient tables and, for plots, their data.
}

\author{Min Lu and Hemant Ishwaran}

\references{
  Lu, M. and Ishwaran, H. (2025). Individual variable priority: a
  model-independent local gradient method for variable importance.
  \emph{Artificial Intelligence Review}, 58:407.
}

\seealso{
  \code{\link{varpro}}, \code{\link{plot.ivarpro}},
  \code{\link{shap.ivarpro}}, \code{\link{predict.ivarpro}}
}

\examples{
\donttest{
## Survival: local changes in predicted mortality.
library(survival)
data(peakVO2, package = "randomForestSRC")
peak <- na.omit(peakVO2)

## Keep all predictors available for the local plots.
set.seed(137)
vp <- varpro(Surv(ttodead, died) ~ ., peak,
             split.weight = FALSE, ntree = 100,
             parallel = FALSE)
ivp <- ivarpro(vp)
print(head(ivp))
shap.ivarpro(ivp)

## Exercise time colors the local relationship with peak oxygen uptake.
plot(ivp, var = "peak.vo2", col.var = "interval")

## Specify a fixed maximum neighborhood width.
ivp.local <- ivarpro(vp, adaptive = FALSE, cut.max = 0.5)
plot(ivp.local, var = "peak.vo2", col.var = "interval")

## Interaction example: Model 3 of Lu and Ishwaran (2025).
## The signal is 6*x1*x2.
## Its slopes are 6*x2 for x1 and 6*x1 for x2.
set.seed(137)
n <- 500
X <- matrix(runif(n * 10), nrow = n)
colnames(X) <- paste0("x", seq_len(ncol(X)))
d <- data.frame(y = 6 * X[, 1] * X[, 2] + rnorm(n), X)
vp <- varpro(y ~ ., d, split.weight = FALSE, ntree = 100,
             parallel = FALSE)

set.seed(137)
iv.local <- ivarpro(vp)
set.seed(137)
iv.global <- ivarpro(vp, scale = "global")
set.seed(137)
iv.slope <- ivarpro(vp, scale = "none")

## Compare local magnitudes under the three scaling choices.
scale.summary <- data.frame(
  variable = colnames(iv.local),
  local = colMeans(abs(iv.local), na.rm = TRUE),
  global = colMeans(abs(iv.global), na.rm = TRUE),
  slope = colMeans(abs(iv.slope), na.rm = TRUE))
print(scale.summary)

## Map both slopes over the (x1, x2) plane, as in the paper's
## gradient displays. Each point is a case; color gives its slope.
## The top row shows the true slopes and the bottom row iVarPro estimates.
## scale = "none" puts the estimates in the same units as the derivatives.
slopes <- cbind("True slope for x1" = 6 * d$x2,
                "True slope for x2" = 6 * d$x1,
                "iVarPro slope for x1" = iv.slope$x1,
                "iVarPro slope for x2" = iv.slope$x2)

slope.map <- function(x1, x2, slopes) {
  ## Use one color scale for all four panels. Positive slopes run
  ## from white through orange to red; negative slopes are blue.
  values <- slopes[is.finite(slopes)]
  limit <- max(1, ceiling(max(abs(values))))
  pal <- c(grDevices::colorRampPalette(c("navy", "white"))(101)[-101],
           grDevices::colorRampPalette(c("white", "orange", "red"))(101))
  slope.col <- function(z) pal[1L + round(100 * (z / limit + 1))]

  op <- par(no.readonly = TRUE)
  on.exit(par(op), add = TRUE)
  layout(matrix(c(1, 2, 3, 4, 5, 5), ncol = 2, byrow = TRUE),
         heights = c(1, 1, 0.3))
  par(mar = c(3.5, 3.5, 2, 1), mgp = c(2, 0.6, 0))
  for (j in seq_len(ncol(slopes))) {
    ok <- is.finite(slopes[, j])
    plot(x1, x2, type = "n", xlim = c(0, 1), ylim = c(0, 1),
         xlab = "x1", ylab = "x2", main = colnames(slopes)[j], asp = 1)
    points(x1[ok], x2[ok], pch = 16, cex = 0.7,
           col = slope.col(slopes[ok, j]))
    points(x1[!ok], x2[!ok], pch = 4, cex = 0.6, col = "grey60")
  }
  par(mar = c(0, 0, 0, 0))
  plot.new()
  at <- seq(-limit, limit, length.out = 7)
  legend("center", legend = format(signif(at, 2), trim = TRUE),
         fill = slope.col(at), ncol = length(at), bty = "n", cex = 0.8,
         title = "Slope (grey crosses: unavailable)")
}
slope.map(d$x1, d$x2, slopes)

## With slopes on the vertical axis, color by the other predictor.
plot(iv.slope, var = "x1", col.var = "x2")
plot(iv.slope, var = "x2", col.var = "x1")

## Explain two supplied targets using the same forest rules.
## Name the columns so targets can be selected explicitly in plots.
targets <- cbind(interaction = 6 * d$x1 * d$x2,
                 linear = 2 * d$x1 - d$x2)
iv.targets <- ivarpro(vp, y.external = targets, scale = "none")
print(iv.targets)
print(head(iv.targets[["linear"]]))
plot(iv.targets, var = "x1", target = "interaction", col.var = "x2")
shap.ivarpro(iv.targets, target = "linear")
}}

\keyword{individual importance}
