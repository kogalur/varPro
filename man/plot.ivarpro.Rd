\name{plot.ivarpro}
\alias{plot.ivarpro}
\alias{shap.ivarpro}
\title{Plot Individual Variable Priority}
\description{
  Plot the local importance scores calculated by \code{ivarpro}.
  Examine how one predictor's scores vary across observations, or
  compare score distributions across predictors in a summary plot.
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
  \item{x, ivar}{An \code{ivarpro} object, or a numeric data frame or
    matrix with one row per observation and one column per predictor
    containing the local importance scores.}
  \item{var}{Predictor to display, given by its column name or index
    in the gradient table. For a categorical predictor converted to
    0/1 indicators, use an indicator column's name or index.}
  \item{col.var}{Name of a column in the plotting data used to color
    points. Factors and numeric variables with few distinct values
    give group colors; continuous variables give a color gradient.}
  \item{size.var}{Name of a column in the plotting data used to scale
    point sizes.}
  \item{data, dat}{Data frame or matrix with the same observations in
    the same order as the gradient table. By default, uses the data
    stored by \code{ivarpro} or \code{predict.ivarpro}. The plotted
    predictor columns must be numeric; the stored data include the 0/1
    indicators used for categorical predictors.}
  \item{target}{For an object with several targets, the response or
    class name, or its integer position. If omitted, the first target
    is used with a warning.}
  \item{pch, cex}{Point symbol and size for the single-predictor plot.
    Group-specific point styles can override \code{pch}.}
  \item{cex.range}{Range of point sizes when \code{size.var} is supplied.}
  \item{main, xlab, ylab}{Plot title and axis labels.}
  \item{legend}{Logical. If \code{TRUE}, displays the color legend in
    the single-predictor plot when \code{col.var} is supplied.}
  \item{...}{Graphical arguments and optional display controls for
    \code{plot}; see Details.}
  \item{feature_names}{Names for an unnamed gradient matrix in the
    summary plot. Existing column names take precedence.}
  \item{max.points}{Maximum total number of points displayed in the
    summary plot. A random subset is plotted when this limit is
    exceeded.}
  \item{max.points.per.feature}{Optional additional limit on the
    number of displayed points per predictor.}
  \item{point.alpha, point.size, point.pch}{Point opacity, size, and
    symbol for the summary plot.}
  \item{scale.value}{Logical. If \code{TRUE}, rescales each predictor's
    displayed values to the range zero to one for coloring. With
    \code{FALSE}, uses a common range across predictors. This affects
    the colors, not the importance scores.}
  \item{style}{Arrangement of points in the summary plot.
    \code{"blobby"} separates overlapping points to show where scores
    are concentrated; \code{"jitter"} adds random vertical displacement.}
  \item{blobby.separation}{Spacing between points for
    \code{style = "blobby"}. Larger values increase separation.}
}
\details{
  These plots show how the local relationships estimated by
  \code{ivarpro} vary across observations. Use \code{shap.ivarpro(x)}
  for an overview of the predictors, then \code{plot(x, var = "x1")}
  to examine one predictor more closely. Each point represents an
  observation's local importance score for a predictor.

  The scores summarize local slopes, called gradients. With signed
  scores, positive values indicate that larger predictor values locally
  accompany larger values of the prediction target; negative values
  indicate the opposite direction. Distance from zero shows the strength
  of the local change on the chosen scale. Results calculated with
  \code{use.abs = TRUE} show strength without direction.

  \subsection{Single-predictor plot}{
    \code{plot(x, var = "x1")} places values of \code{x1} on the
    horizontal axis and its local scores on the vertical axis. A line
    at zero separates positive and negative scores. Add
    \code{col.var = "x2"} to see whether the relationship with
    \code{x1} differs according to \code{x2}. \code{size.var} can
    represent a further variable through point size.

    Smooth curves are added by default when enough observations are
    available: one curve without a color variable, or separate curves
    for its groups. Continuous color variables are divided into ranges
    based on quantiles for these curves.
  }

  \subsection{Adjusting the single-predictor plot}{
    Supply these controls through \code{...}:
    \describe{
      \item{Smooth curves}{Use \code{smooth = FALSE} to omit them,
        or \code{smooth.span} to adjust smoothing.}
      \item{Overlapping points}{Small horizontal jitter is added by
        default. Use \code{jitter = FALSE} to disable it, or
        \code{jitter.seed} for reproducible jitter. \code{col.style}
        selects \code{"auto"}, \code{"solid"}, \code{"outline"}, or
        \code{"binary"} point styling. The automatic style distinguishes
        categorical color groups using outlined or filled and open
        symbols.}
      \item{Predictor distribution}{Use \code{x.dist = "rug"} for
        ticks showing observed predictor values, \code{x.dist = "hist"}
        for a histogram, or \code{x.dist = "density"} for a density
        curve. Combinations such as \code{x.dist = c("hist", "rug")}
        are allowed. These displays use observations with plotted scores.}
      \item{Zero reference line}{Use \code{zero.line = FALSE} to
        omit it.}
    }
  }

  \subsection{Summary plot}{
    \code{shap.ivarpro(x)} gives each predictor a separate row.
    Horizontal position shows the score; color shows the observation's
    predictor value. Predictors are ordered by their mean absolute
    scores. In the default \code{"blobby"} display, overlapping points
    are spread vertically to show where scores are concentrated. This
    SHAP-style layout displays the local gradients calculated by
    \code{ivarpro}.

    By default, colors show low to high values separately for each
    predictor. \code{scale.value} controls this color scaling. The
    gradient scale is set by the original \code{ivarpro} call: for
    example, \code{scale = "global"} uses the predictor's training-sample
    standard deviation, and \code{scale = "none"} gives slopes per
    predictor unit. See \code{\link{ivarpro}} for the scaling choices.

    Columns with only zero or nonfinite scores are omitted. Each point
    requires a finite score and predictor value. \code{max.points} and
    \code{max.points.per.feature} limit the display size without changing
    the stored scores.
  }

  \subsection{Choosing a target and supplying data}{
    For a list of responses or classes, supply the whole object and
    choose one with \code{target}, for example
    \code{plot(x, var = "x1", target = 2)}. Omitting \code{target}
    selects the first element with a warning.

    Both functions use stored data by default. When no data are stored,
    supply the matching predictor values through \code{data} to
    \code{plot} or \code{dat} to \code{shap.ivarpro}. Rows must match
    the gradient table, and column names should match the plotted
    predictors. For categorical predictors, use the same numeric 0/1
    indicator columns used to calculate their scores.
  }
}
\value{
  Draws the requested plot. \code{plot.ivarpro} returns \code{TRUE}
  invisibly; \code{shap.ivarpro} returns \code{NULL} invisibly.
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
