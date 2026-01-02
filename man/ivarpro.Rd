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
        adaptive = TRUE,
        cut = NULL,
        cut.max = 1,
        ncut = 51,
        nmin = 20, nmax = 150,
        y.external = NULL,
        noise.na = TRUE,
        papply = mclapply,
        max.rules.tree = NULL,
        max.tree = NULL,
        use.loo = TRUE,
        use.abs = FALSE,
        path.store.membership = TRUE,
        keep.data = TRUE)
}

\arguments{

  \item{object}{\code{varpro} object from a previous call to
    \code{varpro}, or a \code{rfsrc} object.}
  

  \item{adaptive}{Logical. If \code{FALSE} and \code{cut} is not
  supplied, the \code{cut} grid is constructed as \code{seq(0, cut.max,
  length.out = ncut)}. If \code{TRUE} (default) and \code{cut} is not
  supplied, a data-adaptive upper bound for the neighborhood scale is
  computed from the sample size using a simple bandwidth-style
  rule-of-thumb, and \code{cut} is constructed as a sequence from 0 to
  this data-adaptive maximum (subject to \code{cut.max}). This provides
  a convenient way to automatically sharpen the local neighborhood for
  case-specific gradients when the sample size is moderate to large.}

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
  supplied. The grid is constructed as \code{seq(0, cut.max, length.out
  = ncut)} (or up to an adaptively chosen maximum if \code{adaptive =
  TRUE}).}


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

  \item{use.abs}{Use the absolute gradient for individual importance?
   Default is \code{FALSE} which uses the actual gradient.}

  \item{path.store.membership}{Store the rule membership indices (OOB
  case IDs) in the returned object for later ladder/band calculations?
  Setting \code{FALSE} can substantially reduce memory usage when the
  number of rules is large, but disables ladder-based bands in
  \code{ivarpro.partial()} and prevents \code{ivarpro_band()} from being
  used. Default is \code{TRUE}.}

\item{keep.data}{Save the x and y data (default is \code{TRUE})?  Used
  for downstream plots.}

}

\details{

Understanding individual-level (case-specific) variable importance is
important in applications where decisions are made at the level of a
single person, unit, or record. A predictor may have only a modest
average effect, yet be highly influential for certain cases, or the
direction of its effect may differ across individuals.

The VarPro framework summarizes population-level importance by defining
feature-space regions using rule-based splitting and computing importance
using only observed data. iVarPro extends this idea to the individual
level by quantifying how sensitive each case's prediction is to small
changes in a predictor identified by the VarPro rule set.

For each VarPro rule, iVarPro considers the corresponding rule-defined
region and then \emph{releases} the rule along the rule's release
coordinate. Intuitively, releasing a region means keeping the other rule
constraints in place while allowing additional variation in the released
variable, which provides the information needed to estimate a local
directional effect. A simple local linear regression is then fit on this
released region, and the resulting slope is used as a local,
gradient-based priority score. Case-specific scores are obtained by
aggregating the relevant rule-level gradients over the rules that apply
to each case.

\strong{Neighborhood size and \code{cut.max}.}
For continuous release variables, the size of the local neighborhood
used for slope estimation is controlled by \code{cut} (constructed from
\code{cut.max} and \code{ncut} when not supplied). Smaller neighborhoods
produce more local behavior and can better reflect sharp changes, while
larger neighborhoods produce smoother, more global behavior. When
\code{use.loo = TRUE}, the neighborhood size is chosen in a data-driven
way using a leave-one-out criterion; when \code{use.loo = FALSE}, the
choice is based on meeting the requested sample-size bounds
\code{nmin} and \code{nmax}. When \code{adaptive = TRUE} and \code{cut} is
not supplied, an additional sample-size based rule is used to limit the
maximum neighborhood scale (subject to \code{cut.max}).

For binary or one-hot encoded release variables, iVarPro interprets the
local effect as a scaled finite difference between the two levels (0 and
1), conditional on the other rule constraints; in this case \code{cut}
does not control the neighborhood along the binary coordinate.

\strong{Cut.max ladder (neighborhood sensitivity).}
Because the choice of neighborhood scale can affect the estimated local
gradients, iVarPro also records a \emph{ladder} of rule-level gradient
estimates across the candidate neighborhood sizes defined by the
\code{cut} grid. These ladder values can be summarized (e.g., ranges or
quantiles) and used to visualize how sensitive case-specific gradients
are to the neighborhood choice, without repeatedly refitting iVarPro for
many different \code{cut.max} values. Ladder-based case summaries require
rule membership information; set \code{path.store.membership = TRUE} to
enable ladder bands and related summaries, or leave it \code{FALSE} to
reduce memory usage when the number of rules is very large.  See
examples below.

\strong{Settings that are currently handled.}
The flexibility of this framework makes it suitable for quantifying
case-specific variable importance in regression, classification, and
survival settings. Currently, multivariate forests are not handled.

}

\value{

For univariate outcomes (and two-class classification treated as a single
score), a numeric \code{data.frame} of dimension \eqn{n \times p} containing
case-specific (individual-level) variable priority values, where \eqn{n} is
the number of observations and \eqn{p} is the number of predictors in
\code{object$xvar.names}.

\itemize{
  \item Each row corresponds to a case (observation) in the original data.
  \item Each column corresponds to a predictor variable in
        \code{object$xvar.names}.
}

The entry in row \eqn{i} and column \eqn{j} is the iVarPro importance score
for variable \eqn{j} for case \eqn{i}, measuring the local sensitivity of
that case's prediction to changes in that variable. Predictors that are never
used as release variables in the VarPro rule set may appear with constant
\code{NA} values (when \code{noise.na = TRUE}) or constant zero values (when
\code{noise.na = FALSE}).

\strong{Ladder/path information.}
The returned object carries an attribute \code{"ivarpro.path"} containing
additional information used for ladder-based summaries and plotting. In
particular:

\describe{
  \item{\code{cut}}{The full \code{cut} grid used to evaluate candidate local
    neighborhoods.}
  \item{\code{cut.ladder}}{The interior values of \code{cut} (excluding the
    endpoints) used for the cut.max ladder path.}
  \item{\code{rule.imp.ladder}}{A numeric matrix of dimension \eqn{R \times L}
    storing rule-level gradients selected under each ladder truncation, where
    \eqn{R} is the number of retained rules and \eqn{L = length(cut.ladder)}.}
  \item{\code{rule.variable}}{Integer vector of length \eqn{R} giving the
    release-variable index for each retained rule.}
  \item{\code{oobMembership}}{Optional list (length \eqn{R}) giving the OOB
    membership indices for each retained rule; included only when
    \code{path.store.membership = TRUE}.}
}

Additional tuning flags and rule metadata (e.g., \code{use.loo},
\code{adaptive}, and tree/branch identifiers) may also be included for
diagnostics.

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

## canonical standard analysis
imp1 <- ivarpro(o)
ivarpro.shap(imp1)

## non-adaptive analysis
imp2 <- ivarpro(o, adaptive = FALSE)
ivarpro.shap(imp2)

## non-adaptive using a small cut.max
imp3 <- ivarpro(o, cut.max = 0.5, adaptive = FALSE)
ivarpro.shap(imp3)


## ------------------------------------------------------------
##
## synthetic regression example with partial plot
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

## simulate the data
which.simulation <- 1
df <- simfunction(n = 500, true.function(which.simulation))

## varpro analysis
vp <- varpro(y ~ ., df)

## ivarpro analysis 
imp <- ivarpro(vp)

## partial plot of x2 
ivarpro.partial(imp, var="x2")

## partial plot of x2 without ladder band
ivarpro.partial(imp, var="x2", ladder=FALSE)

## optional: use only a subset of ladder cuts
ivarpro.partial(imp, var="x2", ladder=TRUE, ladder.cuts=1:8)

## partial plot with color/size using x1 (color) and y (size)
ivarpro.partial(imp, var="x2", col.var="x1", size.var="y")

## ------------------------------------------------------------
##
## survival example with partial plot
##
## ------------------------------------------------------------

data(peakVO2, package = "randomForestSRC")

## varpro/importance call
i.pv <- ivarpro(varpro(Surv(ttodead, died)~., peakVO2))

## partial plot of peak vo2
## color displays interval (a measure of exercise time)
## size displays "y" which is predicted mortality in survival
ivarpro.partial(i.pv, var="peak.vo2", col.var="interval", size.var="y")

## same but using beta blockers for color
ivarpro.partial(i.pv, var="peak.vo2", col.var="betablok", size.var="y")

}}

\keyword{individual importance}
