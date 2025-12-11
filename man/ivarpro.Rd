\name{ivarpro}
\alias{ivarpro}
\title{Individual Variable Priority (iVarPro): Case-Specific Variable Importance}

\description{
  Individual Variable Priority (iVarPro) computes case-specific
  (individual-level) variable importance scores. For each observation in the
  data and for each predictor identified by the VarPro analysis, iVarPro
  returns a local gradient-based priority measure that quantifies how
  sensitive that case's prediction is to changes in that variable.
}

\usage{
ivarpro(object,
        cut = NULL,
        cut.max = 1,
        ncut = 21,
        nmin = 20, nmax = 150,
        y.external = NULL,
        noise.na = TRUE,
        papply = mclapply,
        max.rules.tree = NULL,
        max.tree = NULL,
        use.loo = TRUE,
        adaptive = FALSE)
}

\arguments{

  \item{object}{\code{varpro} object from a previous call to
    \code{varpro}, or a \code{rfsrc} object.}
  

  \item{cut}{Optional user-supplied sequence of \eqn{\lambda} values
  used to relax the constraint region in the local linear regression
  model. For continuous release variables, each value in \code{cut} is
  calibrated so that \code{cut = 1} corresponds to one standard
  deviation of the release coordinate. If \code{cut} is supplied, it is
  used as-is and the arguments \code{cut.max}, \code{ncut}, and
  \code{adaptive} are ignored. For binary or one-hot encoded release
  variables, the full released region is used and \code{cut} does not
  control neighborhood size.}


  \item{cut.max}{Maximum value of the \eqn{\lambda} grid used to define
  the local neighborhood for continuous release variables when
  \code{cut} is not supplied. By default, \code{cut} is constructed as
  \code{seq(0, cut.max, length.out = ncut)} (or up to a data-adaptive
  value if \code{adaptive = TRUE}). Smaller values of \code{cut.max}
  yield more local, sharper case-specific gradients, while larger values
  yield smoother, more global behavior.}


  \item{ncut}{Length of the \code{cut} grid when \code{cut} is not
  supplied. The default is \code{21}. The grid is constructed as
  \code{seq(0, cut.max, length.out = ncut)} (or up to an adaptively
  chosen maximum if \code{adaptive = TRUE}).}


  \item{nmin}{Minimum number of observations required for fitting a
    local linear model.}
  

  \item{nmax}{Maximum number of observations allowed for fitting a local
  linear model. Internally, \code{nmax} is capped at 10\% of the sample
  size.}


  \item{y.external}{Optional user-supplied response vector or matrix to
  use as the dependent variable in the local linear regression. Must
  have the same number of rows as the feature matrix and match the
  dimension and type expected for the outcome family.}


  \item{noise.na}{Logical. If \code{TRUE} (default), gradients for noisy
  or non-signal variables are set to \code{NA}; if \code{FALSE}, they
  are set to zero.}

  \item{papply}{Apply function used for R parallelization.}   

  \item{max.rules.tree}{Maximum number of rules per tree.  If
    unspecified, the value from the \code{varpro} object is used, while
    for \code{rfsrc} objects, a default value is used.}

  \item{max.tree}{Maximum number of trees used to extract rules. If
    unspecified, the value from the \code{varpro} object is used, while
    for \code{rfsrc} objects, a default value is used.}

  \item{use.loo}{Logical. If \code{TRUE} (default), leave-one-out
  cross-validation is used to select the best neighborhood size (i.e.,
  the best value in \code{cut}) for each rule and release variable. If
  \code{FALSE}, the neighborhood is chosen to use the largest available
  sample that satisfies \code{nmin} and \code{nmax}.}


  \item{adaptive}{Logical. If \code{FALSE} (default) and \code{cut} is
  not supplied, the \code{cut} grid is constructed as \code{seq(0,
  cut.max, length.out = ncut)}. If \code{TRUE} and \code{cut} is not
  supplied, a data-adaptive upper bound for the neighborhood scale is
  computed from the sample size using a simple bandwidth-style
  rule-of-thumb, and \code{cut} is constructed as a sequence from 0 to
  this data-adaptive maximum (subject to \code{cut.max}). This provides
  a convenient way to automatically sharpen the local neighborhood for
  case-specific gradients when the sample size is moderate to large.}


}

\details{

Understanding individual-level (case-specific) variable importance is
critical in applications where personalized decisions are required.
Traditional variable importance methods focus on average
(population-level) effects and often fail to capture heterogeneity
across individuals. In many real-world problems, it is not sufficient to
determine whether a variable is important on average; we must also
understand how it affects each individual prediction.

The VarPro framework identifies feature-space regions through rule-based
splitting and computes importance using only observed data. This avoids
biases introduced by permutation or synthetic data, leading to robust,
population-level importance estimates. However, VarPro does not directly
capture individual-level effects.

To address this limitation, individual variable priority (iVarPro)
extends VarPro by estimating, for each case and each variable identified
by the VarPro analysis, a local gradient that quantifies how small
changes in that variable influence that case's predicted outcome.
Formally, for each observation \eqn{i = 1, \dots, n} and each predictor
\eqn{j} that appears as a release variable in the VarPro rule set,
iVarPro returns a case-specific importance score measuring the
sensitivity of the prediction for case \eqn{i} to perturbations in
variable \eqn{j}, holding the other variables locally fixed.

iVarPro leverages the release region concept from VarPro. A region
\eqn{R} is first defined using VarPro rules. Since using only data
within \eqn{R} often results in insufficient sample size for stable
gradient estimation, iVarPro releases \eqn{R} along a coordinate
\eqn{s}. This means the constraint on \eqn{s} is removed while all
others are held fixed, yielding additional variation specifically in the
\eqn{s}-direction, precisely what is needed to compute directional
derivatives.

Local gradients are then estimated via linear regression on the expanded
region. For continuous release variables, the parameter \code{cut}
controls the amount of constraint relaxation, where \code{cut = 1}
corresponds to one standard deviation of the release coordinate,
calibrated automatically from the data. When \code{cut} is not supplied,
it is constructed as a grid from 0 to \code{cut.max}. Smaller values of
\code{cut.max} force more local neighborhoods and can sharpen
case-specific gradients near discontinuities, while larger values induce
smoother, more global behavior.

For binary or one-hot encoded release variables, the gradient is
interpreted as a scaled finite difference in the predicted outcome
between the two levels (0 and 1), conditional on the rule constraints
for the other variables. In this case, the full released region is used,
and \code{cut} is not used to define the local neighborhood along the
binary coordinate.

When \code{use.loo = TRUE}, a leave-one-out cross-validation (LOO) score
is computed for each candidate neighborhood defined by \code{cut} for
continuous release variables. The value of \code{cut} that minimizes the
LOO score is selected, providing an adaptive, data-driven choice of
expansion region size for each rule and release coordinate. When
\code{use.loo = FALSE}, the expansion region is chosen based only on
sample size, favoring the largest neighborhood that satisfies
\code{nmin} and \code{nmax}.

When \code{adaptive = TRUE} and \code{cut} is not supplied, the maximum
neighborhood scale is chosen based on the sample size using a simple
bandwidth-style rule-of-thumb, and the \code{cut} grid is constructed up
to this data-adaptive maximum (subject to the upper bound \code{cut.max}).
This can be particularly useful in settings with discontinuous or highly
nonlinear responses, where a globally wide neighborhood may over-smooth
important local features and obscure case-specific effects.

The flexibility of this framework makes it suitable for quantifying
case-specific variable importance in regression, classification, and
survival settings.  Currently, multivariate forests are not handled.

}

\value{

For univariate outcomes (and two-class classification treated as a
single score), a numeric matrix of dimension \eqn{n \times p_*} containing
case-specific (individual-level) variable priority values, where
\eqn{p_*} is the number of predictor variables identified by the VarPro
analysis (typically those used as release coordinates in the rule set).

\itemize{
  \item Each row corresponds to a case (observation) in the original data.
  \item Each column corresponds to a predictor variable identified by the
        VarPro analysis as having an associated release rule.
}

The entry in row \eqn{i} and column \eqn{j} is the iVarPro importance
score for variable \eqn{j} for case \eqn{i}, measuring the local
sensitivity of that case's prediction to changes in that variable.
Predictors that are never used as release variables in the VarPro
analysis do not receive a case-specific importance score (or, depending
on implementation, may appear with constant \code{NA} or zero values).

}

\author{

  Min Lu and Hemant Ishwaran

}

\references{

  Lu, M. and Ishwaran, H. (2025). Individual variable priority: a
  model-independent local gradient method for variable importance.

}

\seealso{
  \code{\link{varpro}}
}

\examples{
\donttest{

## ------------------------------------------------------------
##
## survival example with shap-like plot
##
## ------------------------------------------------------------

data(peakVO2, package = "randomForestSRC")
o <- varpro(Surv(ttodead, died)~., peakVO2, ntree = 50)
imp <- ivarpro(o, adaptive = TRUE)
ivarpro.shap(imp, o$x)


## ------------------------------------------------------------
##
## synthetic regression example
##
## ------------------------------------------------------------

## true regression function
true.function <- function(which.simulation) {
  if (which.simulation == 1) {
    function(x1, x2) { 1 * (x2 <= .25) +
      15 * x2 * (x1 <= .5 & x2 > .25) +
      (7 * x1 + 7 * x2) * (x1 > .5 & x2 > .25) }
  }
  else if (which.simulation == 2) {
    function(x1, x2) { r <- x1^2 + x2^2; 5 * r * (r <= .5) }
  }
  else {
    function(x1, x2) { 6 * x1 * x2 }
  }
}

## simulation function
simfunction <- function(n = 1000, true.function, d = 20, sd = 1) {
  d <- max(2, d)
  X <- matrix(runif(n * d, 0, 1), ncol = d)
  dta <- data.frame(list(
    x = X,
    y = true.function(X[, 1], X[, 2]) + rnorm(n, sd = sd)
  ))
  colnames(dta)[1:d] <- paste("x", 1:d, sep = "")
  dta
}

## iVarPro importance plot
ivarpro.plot <- function(dta, release = 1, combined.range = TRUE,
                         cex = 1.0, cex.title = 1.0, sc = 5.0,
                         gscale = 30, title = NULL) {
  x1 <- dta[, "x1"]
  x2 <- dta[, "x2"]
  x1n <- expression(x^{(1)})
  x2n <- expression(x^{(2)})
  if (release == 1) {
    if (is.null(title))
      title <- bquote("iVarPro Case-Specific Gradient " ~ x^{(1)})
    cex.pt <- dta[, "Importance.x1"]
  }
  else {
    if (is.null(title))
      title <- bquote("iVarPro Case-Specific Gradient " ~ x^{(2)})
    cex.pt <- dta[, "Importance.x2"]
  }
  if (combined.range) {
    cex.pt <- cex.pt / max(dta[, c("Importance.x1", "Importance.x2")],
                           na.rm = TRUE)
  }
  rng <- range(c(x1, x2))
  oldpar <- par(mar = c(4, 5, 5, 1),
                mgp = c(2.25, 1.0, 0),
                bg = "white")
  gscalev <- gscale
  gscale <- paste0("gray", gscale)
  plot(x1, x2, xlab = x1n, ylab = x2n,
       ylim = rng, xlim = rng,
       col = "#FFA500", pch = 19,
       cex = (sc * cex.pt), cex.axis = cex, cex.lab = cex,
       panel.first = rect(par("usr")[1], par("usr")[3],
                          par("usr")[2], par("usr")[4],
                          col = gscale, border = NA))
  abline(a = 0, b = 1,
         lty = 2, col = if (gscalev < 50) "white" else "black")
  mtext(title, cex = cex.title, line = .5)
  par(oldpar)
}

## simulate the data
which.simulation <- 1
df <- simfunction(n = 500, true.function(which.simulation))

## varpro analysis
o <- varpro(y ~ ., df)

## canonical ivarpro analysis (default cut.max = 1)
imp1 <- ivarpro(o)

## sharper local analysis using a smaller cut.max
imp2 <- ivarpro(o, cut.max = 0.5)

## data-adaptive analysis
imp3 <- ivarpro(o, adaptive = TRUE)

## build data for plotting the results (using x1 and x2)
df.imp1 <- data.frame(Importance = imp1, df[, c("x1", "x2")])
df.imp2 <- data.frame(Importance = imp2, df[, c("x1", "x2")])
df.imp3 <- data.frame(Importance = imp3, df[, c("x1", "x2")])

## plot the case-specific importance surfaces
oldpar <- par(mfrow = c(3, 2))
ivarpro.plot(df.imp1, 1); ivarpro.plot(df.imp1, 2)
ivarpro.plot(df.imp2, 1); ivarpro.plot(df.imp2, 2)
ivarpro.plot(df.imp3, 1); ivarpro.plot(df.imp3, 2)
par(oldpar)
}}

\keyword{individual importance}
