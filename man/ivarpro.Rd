\name{ivarpro}
\alias{ivarpro}
\title{Individual Variable Priority: Case-Specific Local Gradients}

\description{
  Calculates variable importance for individual training observations.
  The scores describe the direction and strength of local changes in a
  model's predictions as each predictor changes. Supports regression,
  classification, and survival analyses.
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
  \item{object}{A fitted \code{varpro} object, or a supervised
    \code{rfsrc} grow object with a single outcome and numeric
    predictors. Use a \code{varpro} object for categorical predictors
    and multivariate regression.}

  \item{adaptive}{Logical. If \code{TRUE}, automatically limits the
    largest candidate neighborhood according to the sample size.
    Used when \code{cut} is not supplied.}

  \item{cut}{Optional vector of nonnegative neighborhood widths for
    continuous predictors. Each width gives the maximum distance from
    the rule's center, in standard deviations of the predictor values
    among usable observations in the enlarged comparison group described
    in Details. Supplied values are sorted and duplicates removed;
    \code{adaptive},
    \code{cut.max}, and \code{ncut} are then ignored. Binary 0/1
    predictors use a comparison of the two levels.}

  \item{cut.max}{Upper bound on candidate neighborhood width when
    \code{cut} is not supplied. Smaller values restrict the search to
    narrower neighborhoods.}

  \item{ncut}{Number of candidate widths when \code{cut} is not
    supplied. Must be a positive integer.}

  \item{nmin}{Minimum number of usable observations for a local
    estimate. Must be an integer of at least two.}

  \item{nmax}{Maximum number of observations used for a local estimate.
    Must be an integer of at least two. The effective maximum is also
    limited according to the training sample size and is at least
    \code{nmin}.}

  \item{y.external}{Optional numeric vector or matrix containing the
    values to explain, such as predictions from another model. Rows
    must match the training observations used by \code{object}, in
    the same order. Each matrix column supplies a separate target;
    column names identify the targets. The default uses out-of-bag
    predictions from the forest in \code{object}.}

  \item{noise.na}{Logical. If \code{TRUE}, averages available rule
    estimates and uses \code{NA} where none are available. With
    \code{FALSE}, unavailable rule estimates contribute zero to the
    average, and observations with no applicable rules receive zero.}

  \item{max.rules.tree}{Maximum number of rules examined per tree.
    Defaults to the setting in a \code{varpro} object, or 150 for
    direct \code{rfsrc} input.}

  \item{max.tree}{Maximum number of trees used to obtain rules.
    Defaults to the setting in a \code{varpro} object, or 150 for
    direct \code{rfsrc} input.}

  \item{use.loo}{Logical. If \code{TRUE}, chooses the neighborhood for
    each continuous-predictor fit by leave-one-out prediction error.
    With \code{FALSE}, uses the valid candidate with the most
    observations, subject to \code{nmin} and \code{nmax}.}

  \item{use.abs}{Logical. If \code{TRUE}, averages the absolute values
    of the rule-level scores. The default \code{FALSE} preserves
    their signs.}

  \item{path.store.membership}{Logical. If \code{TRUE}, stores the
    observation indices for each rule and its additional comparison
    group. Setting \code{FALSE} reduces storage without changing the
    calculated scores or their plots.}

  \item{save.data}{Logical. If \code{TRUE}, stores the processed
    predictors and target values in the result for plotting.}

  \item{save.model}{Logical. If \code{TRUE}, stores the supplied model
    in the result.}

  \item{scale}{Scaling of the local slopes. \code{"local"} uses the
    predictor's spread in the selected neighborhood; \code{"global"}
    uses its standard deviation across the training observations;
    \code{"none"} gives slopes per unit of the predictor. See Details.}
}

\details{
  \code{ivarpro} shows how a predictor's relationship with a model's
  predictions varies from one observation to another. It returns a
  separate importance score for each training observation and predictor.
  The scores summarize local slopes, called gradients, which describe
  both the direction and strength of the relationship.

  Use \code{x <- ivarpro(object)} to calculate the scores from a fitted
  model. \code{print(head(x))} displays the first few observations for
  a single prediction target. The plotting functions below help explore
  how the scores vary across the data.

  \subsection{Interpreting the scores}{
    A positive score indicates that larger predictor values locally
    accompany larger predicted values; a negative score indicates the
    opposite direction. Larger magnitudes represent stronger local
    changes on the chosen scale. For example, a negative score for a
    mortality target indicates a local relationship with lower
    predicted mortality as the predictor increases.

    The default preserves signs when averaging rule-level scores, so
    positive and negative slopes can cancel. Use \code{use.abs = TRUE}
    to summarize strength alone by averaging their absolute values.

    Scores are calculated for the predictors in
    \code{object$xvar.names}. VarPro converts categorical predictors
    to numeric 0/1 indicators, with a separate score for each indicator.
    With the default \code{noise.na = TRUE}, \code{NA} marks an
    unavailable estimate, for example because an observation has no
    applicable rules or there is too little local predictor variation.
  }

  \subsection{Which values are explained}{
    The values being explained are called the prediction target.
    By default, \code{ivarpro} uses out-of-bag (OOB) predictions from
    the forest in \code{object}. Regression uses predicted responses,
    and classification uses predicted class probabilities. Binary
    classification uses the first probability column; use
    \code{attr(x, "target")} to identify it. Multiclass classification
    returns a named list with one gradient table per class.

    A survival forest supplies mortality predictions. When VarPro uses
    a multivariate regression forest for restricted mean survival time
    (RMST) targets, each predicted target has its own gradient table.
    More generally, multivariate VarPro regression returns one table
    per predicted response.

    Supply \code{y.external} to explain another numeric target, such
    as predictions from a different model or a particular class
    probability, using the same forest rules. A matrix supplies one
    target per column, including when it has two columns. Its rows
    must match the observations used in the fitted model, after any
    omissions during model fitting.
  }

  \subsection{How local scores are calculated}{
    Each forest rule describes a group of observations through
    restrictions on predictor values. To study one predictor,
    \code{ivarpro} removes that predictor's restrictions while keeping
    those on the other predictors. This step, called releasing the
    predictor, adds observations to the comparison group.

    For a continuous predictor, the method fits a straight line to
    the target values using observations from the enlarged group that
    are near the predictor's mean among the rule's OOB members. The
    fitted slope describes the local change in the target. For a 0/1
    predictor, the slope is the difference in mean target values
    between levels 1 and 0.

    After any requested scaling, each rule's score is assigned to its
    original OOB members: observations satisfying the rule that were
    not used to grow its tree. Each observation's final score averages
    the applicable rule scores for that predictor.
  }

  \subsection{Scaling and neighborhood size}{
    With the default \code{scale = "local"}, each rule's slope is
    multiplied by the predictor's spread in the selected neighborhood.
    For continuous predictors, this spread is measured around the
    rule's OOB mean; for 0/1 predictors, it is the sample standard
    deviation. With \code{scale = "global"}, the multiplier is the
    predictor's standard deviation across the training observations.
    Both choices express the score in target units. With
    \code{scale = "none"}, scores are in target units per predictor
    unit; for a 0/1 predictor, the slope is the change from level 0 to 1.

    Neighborhood size determines which nearby observations contribute
    to a continuous-predictor slope. By default, candidate neighborhoods
    are compared by leave-one-out prediction error: each observation
    is predicted from a line fitted without it, and the neighborhood
    with the smallest mean squared error is chosen. With
    \code{use.loo = FALSE}, the valid candidate containing the most
    observations is used instead.

    \code{cut.max} limits the candidate widths, and \code{adaptive}
    allows the maximum to be reduced according to sample size.
    Supply \code{cut} to specify the widths directly. Increasing
    \code{nmin} requires more observations for each local fit;
    \code{nmax} limits how many are used. Binary predictors use a
    comparison of both levels, subject to these sample-size controls,
    rather than a neighborhood search.
  }

  \subsection{Displaying local importance}{
    \code{shap.ivarpro(x)} compares the distribution of scores across
    predictors. \code{plot(x, var = "x1")} shows one predictor's
    scores against its values. Add \code{col.var} to color points by
    another variable and examine how the local relationship varies
    across observations. See \code{\link{plot.ivarpro}} for interpreting
    and adjusting these plots.

    For a list of target-specific tables, supply \code{target} by name
    or index to either plotting function. Without it, the first target
    is used with a warning. The data stored by \code{ivarpro} are used
    automatically. With \code{save.data = FALSE}, supply the matching
    processed data through \code{dat} to \code{shap.ivarpro} or
    \code{data} to \code{plot}.
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
  data frame with one row per training observation used by the model
  and one column per predictor in \code{object$xvar.names}. Row names
  match the processed training data. Multiple targets produce a named
  list of these data frames. \code{print(x)} summarizes a list;
  \code{print(x, full = TRUE)} displays all of its tables.

  The \code{target} attribute identifies the target or targets. When
  requested, the \code{data} attribute contains the processed predictors
  and target values, and the \code{model} attribute contains the supplied
  model. Target columns in the stored data are named \code{y} or
  \code{y.<target>}, with suffixes added when needed to give each column
  a unique name. The target-to-column mapping is available through
  \code{attr(attr(x, "data"), "response.names")}.

  The \code{ivarpro.path} attribute stores neighborhood settings and
  diagnostics for the selected rule estimates, such as their slopes
  and numbers of observations. For multiple targets, shared settings
  are stored on the list and target-specific diagnostics on each table.
  Membership indices are included when \code{path.store.membership = TRUE}.
  The gradient tables and matching data are sufficient for plotting.
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
