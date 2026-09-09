\name{plot.ivarpro}
\alias{plot.ivarpro}
\alias{shap.ivarpro}
\title{Plot Individual Variable Priority}
\description{
  Display case-specific iVarPro gradients for one predictor, or compare
  their distributions across predictors in a beeswarm-style summary.
}
\usage{
\method{plot}{ivarpro}(x, var, col.var = NULL, size.var = NULL,
  data = NULL, target = NULL, pch = 16, cex = 0.8,
  cex.range = c(0.5, 2), main = NULL, xlab = NULL,
  ylab = "iVarPro gradient", legend = TRUE, ...)

shap.ivarpro(ivar, dat = NULL, feature_names = NULL,
  max.points = 5000, max.points.per.feature = NULL,
  point.alpha = 1.0, point.size = 0.35, point.pch = 16,
  scale.value = TRUE, style = c("blobby", "jitter"),
  blobby.separation = 3, target = NULL)
}
\arguments{
  \item{x, ivar}{An iVarPro object, or a numeric data frame or matrix
    of case-specific gradients.}
  \item{var}{Predictor to display, given by its column name or index
    in the gradient table. Use the processed predictor name for a
    hot-encoded factor.}
  \item{col.var}{Optional column of the plotting data used to color
    points. A factor gives group colors; a continuous variable gives
    a color gradient.}
  \item{size.var}{Optional column of the plotting data used to scale
    point sizes.}
  \item{data, dat}{Plotting data with rows in the same order as the
    gradient table. By default, use the data stored by \code{ivarpro}
    or \code{predict.ivarpro}. The plotted predictor columns must be
    numeric; the stored data provide the appropriate hot-encoding.}
  \item{target}{For a multi-target object, the response or class name,
    or its integer position. If omitted, use the first target with
    a warning.}
  \item{pch, cex}{Point symbol and size for the single-predictor plot.
    Group-specific point styles can override \code{pch}.}
  \item{cex.range}{Range of point sizes when \code{size.var} is supplied.}
  \item{main, xlab, ylab}{Plot title and axis labels.}
  \item{legend}{Display the color legend in the single-predictor plot?}
  \item{...}{Graphical arguments and optional display controls for
    \code{plot}; see Details.}
  \item{feature_names}{Names for an unnamed gradient matrix in the
    summary plot. Existing column names take precedence.}
  \item{max.points}{Maximum total number of points in the summary
    plot. Larger collections are subsampled.}
  \item{max.points.per.feature}{Optional additional limit on the
    number of displayed points per predictor.}
  \item{point.alpha, point.size, point.pch}{Point opacity, size and
    symbol for the summary plot.}
  \item{scale.value}{Rescale each predictor's displayed values to
    the range zero to one for coloring? Default is \code{TRUE}.
    This changes the colors, not the gradient scores.}
  \item{style}{Summary-point arrangement: \code{"blobby"} separates
    overlapping points into a beeswarm-like display; \code{"jitter"}
    uses vertical jitter.}
  \item{blobby.separation}{Spacing between points for
    \code{style = "blobby"}. Larger values increase separation.}
}
\details{
  \subsection{Single-predictor plot}{
    \code{plot(x, var = "x1")} places predictor values on the
    horizontal axis and their local gradients on the vertical axis.
    A zero reference line helps distinguish positive and negative
    relationships. Coloring by another predictor can reveal how the
    relationship varies across cases.

    Smooth curves are drawn by default, with separate curves for color
    groups. Useful controls passed through \code{...} include
    \code{smooth = FALSE} to suppress curves, \code{smooth.span} to
    adjust smoothing, and \code{jitter = FALSE} to suppress horizontal
    jitter. \code{jitter.seed} makes jitter reproducible.

    Use \code{x.dist = "rug"}, \code{"hist"}, or \code{"density"}
    to show the predictor distribution along the horizontal axis.
    Combinations such as \code{x.dist = c("hist", "rug")} are allowed.
    \code{zero.line = FALSE} suppresses the reference line.
    \code{col.style} selects \code{"auto"}, \code{"solid"},
    \code{"outline"}, or \code{"binary"} point styling.
  }
  \subsection{Summary plot}{
    \code{shap.ivarpro(x)} displays the iVarPro gradients for all
    predictors. Horizontal position shows a gradient's sign and
    magnitude; color shows the predictor value. Predictors are ordered
    by mean absolute gradient. Columns containing only zero or
    nonfinite scores are omitted.

    The gradient scale is set by the original \code{ivarpro} call.
    In particular, \code{scale = "global"} expresses changes on a
    common training-standard-deviation scale within each predictor,
    while \code{scale = "none"} shows slopes in original predictor
    units. See \code{\link{ivarpro}} for the three scaling choices.
  }
}
\value{
  Called for its plotting side effect. \code{plot.ivarpro} returns
  \code{TRUE} invisibly; \code{shap.ivarpro} returns \code{NULL}
  invisibly.
}
\seealso{
  \code{\link{ivarpro}}, \code{\link{predict.ivarpro}}
}
\examples{
\donttest{
## Compare local mortality gradients and inspect peak oxygen uptake.
library(survival)
data(peakVO2, package = "randomForestSRC")
peak <- na.omit(peakVO2)
set.seed(137)
vp <- varpro(Surv(ttodead, died) ~ ., peak,
             split.weight = FALSE, ntree = 100,
             parallel = FALSE)
ivp <- ivarpro(vp)
print(head(ivp))
shap.ivarpro(ivp)
plot(ivp, var = "peak.vo2", col.var = "interval")
## scale points to "y" (here equal to mortality)
plot(ivp, var = "peak.vo2", col.var = "interval", size.var = "y",
     x.dist = c("hist", "rug"), jitter = FALSE)
}}
\keyword{hplot}
