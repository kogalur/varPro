\name{partialpro}
\alias{partialpro}
\title{Case-local Partial Profiles for VarPro Variables}

\description{
  Estimate and display case-local partial effect profiles for selected
  variables from a fitted \code{varpro} object.  The method constructs
  virtual twins by varying one feature at a time, evaluates a prediction
  learner on those virtual records, and optionally removes virtual records
  that are far from the observed covariate support using Unlimited Virtual
  Twins (UVT).  UVT filtering can be based on \code{isopro} isolation
  forest support or on \code{outpro} OOD distances.
}

\usage{
partialpro(object, xvar.names, nvar,
           target, learner, newdata, method = c("unsupv", "rnd", "auto"),
           verbose = FALSE, vt.filter = c("isopro", "outpro", "none"),
           ...)
}

\arguments{

  \item{object}{A \code{varpro} object returned by \code{varpro}.}

  \item{xvar.names}{Optional character vector of variables for which partial
  profiles are requested.  If omitted, the variables returned by
  \code{get.topvars(object)} are used.  Names not found in
  \code{object$xvar.names} are silently dropped.}

  \item{nvar}{Optional integer limiting the number of variables used from
  \code{xvar.names}.  If supplied, only the first \code{nvar} requested
  variables are used.  If \code{xvar.names} is omitted, this limits the
  number of top VarPro variables.}

  \item{target}{For classification, the class for which the partial profile
  is computed.  This can be either an integer column index or a character
  class label.  The default is the last class level.  For regression and
  survival outcomes this argument is ignored and the first prediction column
  is used.}

  \item{learner}{Optional prediction function.  If omitted,
  \code{partialpro} uses the forest stored in \code{object$rf}.  A supplied
  learner must accept a data frame of feature values with the same columns
  as \code{object$x}.  It should return a numeric vector for regression or
  survival outcomes, or a matrix/data frame of class probabilities for
  classification outcomes.  For classification, the column selected by
  \code{target} is used.}

  \item{newdata}{Optional data frame of cases on which the partial profiles
  are conditioned.  If omitted, up to \code{nsmp} training cases are sampled
  from \code{object$x} separately for each requested variable.  If supplied,
  all rows of \code{newdata} are used.  The data must already be on the
  fitted VarPro x-scale, i.e. it must contain the columns of
  \code{object$x}.  This is automatic when using rows from \code{object$x},
  for example \code{newdata = object$x[1:6, ]}.  Extra columns are ignored
  after alignment to \code{colnames(object$x)}.}

  \item{method}{Isolation forest method used by \code{isopro} when
  \code{vt.filter = "isopro"} and \code{cut != 0}.  Options are
  \code{"unsupv"}, \code{"rnd"}, and \code{"auto"}.  The default is
  \code{"unsupv"}.  If there is only one top variable and
  \code{method = "unsupv"}, the code switches internally to \code{"rnd"}.
  This argument is ignored when \code{vt.filter = "outpro"} or
  \code{vt.filter = "none"}.}

  \item{verbose}{Logical.  If \code{TRUE}, print the variable currently being
  processed.}

  \item{vt.filter}{Virtual twin filtering engine.  The default
  \code{"isopro"} uses isolation forests through \code{isopro}.  The option
  \code{"outpro"} uses out-of-distribution distances from \code{outpro},
  calibrated through \code{outpro.null}.  The option \code{"none"} disables
  virtual twin filtering, equivalent to setting \code{cut = 0}.}

  \item{...}{Advanced options controlling the virtual grid, UVT filtering,
  and local smoothing.  These options are intentionally not part of the main
  argument list, but they are user-adjustable.  See \strong{Advanced options}
  in \code{Details}.}
}

\details{

\strong{Method.}
For a requested variable \code{xnm}, \code{partialpro} creates a sequence of
virtual values \code{xvirtual}.  For each conditioning case, it constructs
virtual twins by replacing \code{xnm} with each value in \code{xvirtual} while
holding all other features fixed at that case's observed values.  The
prediction learner is then evaluated on the resulting virtual data.

The virtual grid is constructed as follows.  Factors and variables with at
most 10 unique observed values use their observed levels/values.  Continuous
variables with more than 10 unique values are trimmed to the central
\code{alpha} to \code{1 - alpha} empirical range and then represented by up to
\code{nvirtual} quantile values.  The actual number of grid points can be
smaller than \code{nvirtual} if quantiles coincide.

If \code{cut != 0} and \code{vt.filter != "none"}, UVT filtering is
applied before estimating the local profile.  With
\code{vt.filter = "isopro"}, an isolation forest is fit by \code{isopro}
using the top VarPro variables.  Each virtual twin receives an \code{isopro}
support score, and only virtual twins with score at least \code{cut} are
treated as admissible.

With \code{vt.filter = "outpro"}, virtual twins are scored by \code{outpro}
on the fitted VarPro x-scale.  The public \code{outpro} function normally
hot-encodes raw \code{newdata}; here the virtual matrix is already aligned to
\code{object$x}, so \code{partialpro} calls \code{outpro} with
\code{newdata.xscale = TRUE}.  The raw \code{outpro} distance is calibrated
against the null/reference distance distribution from \code{outpro.null} and
converted to a support score \code{1 - F0(distance)}.  The same rule is then
used: a virtual twin is admissible when this support score is at least
\code{cut}.  Thus \code{cut = 0} disables UVT filtering, while larger values
of \code{cut} retain fewer virtual twins.  The default outpro distance for
\code{partialpro} is \code{"knn"}, which uses a standardized nearest-neighbor
support calculation and avoids the forest-neighborhood computations used by
several other \code{outpro} distances.

For regression and survival outcomes, learner predictions are used on their
native prediction scale.  For classification, the selected class probability
is converted to a clipped log-odds scale using probabilities truncated to the
interval \code{[0.001, 0.999]}.

For continuous variables, a local polynomial profile is fit separately for
each conditioning case using \code{lm.fit} and the design
\code{1, x, x^2, ..., x^df}.  A case is retained if it has at least
\code{min(nmin, nxorg / 2)} admissible virtual twins, where \code{nxorg} is
the number of unique observed values of the variable.  When UVT filtering is
active and enough admissible training virtual twins are available, the code
fits both a UVT-restricted polynomial and an unrestricted polynomial on a
case-specific train split of virtual values.  The unrestricted fit is used
only if its held-out standardized MSE is lower than the UVT-restricted MSE by
more than \code{mse.tolerance}; otherwise the UVT-restricted fit is used.

For binary variables, no polynomial extrapolation is used.  The profile is
computed directly from the mean predicted value at the two virtual values,
and both virtual values must be admissible for a non-missing case-specific
profile.

The component named \code{yhat.causal} is the case-specific profile centered
at the first virtual value, i.e. the estimated contrast relative to
\code{xvirtual[1]}.  The name is retained for compatibility with the VarPro
terminology, but causal interpretation requires the usual substantive and
design assumptions; the function itself estimates model-based partial
profiles and contrasts.

\strong{Advanced options supplied through \code{...}.}
\describe{
  \item{\code{cut}}{UVT threshold.  Virtual twins with support score less
  than \code{cut} are excluded.  For \code{vt.filter = "isopro"}, the support
  score is the \code{isopro} score.  For \code{vt.filter = "outpro"}, the
  support score is \code{1 - F0(distance)}, where \code{F0} is the empirical
  null/reference CDF from \code{outpro.null}.  Default is \code{0.1}.  Set
  \code{cut = 0}, or use \code{vt.filter = "none"}, to turn off UVT
  filtering.}

  \item{\code{nsmp}}{Number of training cases sampled when \code{newdata} is
  omitted.  The actual number is \code{min(nrow(object$x), nsmp)}.  Default
  is \code{250}.  This option is ignored when \code{newdata} is supplied.}

  \item{\code{nvirtual}}{Requested number of virtual grid values for a
  continuous variable with more than 10 unique values.  Default is
  \code{100}.  The actual grid can be shorter if quantile values are tied.
  Factors and variables with at most 10 unique values use all observed
  levels/values instead.}

  \item{\code{nmin}}{Minimum number of admissible virtual twins required for a
  continuous-variable case profile, after applying the cap
  \code{min(nmin, nxorg / 2)}.  Default is \code{15}.}

  \item{\code{alpha}}{Tail trimming used to define the continuous virtual
  grid.  The grid is formed from values between the empirical
  \code{alpha} and \code{1 - alpha} quantiles.  Default is \code{0.025}.}

  \item{\code{df}}{Degree of the local polynomial used for continuous
  variables.  The value is rounded and constrained to be at least 1.
  Default is \code{2}, giving a quadratic local profile.}

  \item{\code{sampsize}}{Sample-size function passed to \code{isopro} when
  \code{vt.filter = "isopro"} and UVT filtering is active.  The default is
  \code{function(x) min(2^8, 0.632 * x)}.}

  \item{\code{ntree}}{Number of trees used by the \code{isopro} isolation
  forest when \code{vt.filter = "isopro"} and UVT filtering is active.
  Default is \code{500}.}

  \item{\code{nodesize}}{Terminal node size used by \code{isopro} when
  \code{vt.filter = "isopro"} and UVT filtering is active.  Default is
  \code{1}.}

  \item{\code{mse.tolerance}}{Tolerance in the held-out comparison between
  the UVT-restricted and unrestricted polynomial fits.  The unrestricted fit
  is selected only when its standardized MSE is smaller by more than this
  amount.  Default is \code{0}.}

  \item{\code{out.distancef}}{Distance used by \code{outpro} when
  \code{vt.filter = "outpro"}.  The default is \code{"knn"}.  Other accepted
  values are those supported by \code{outpro}, including \code{"prod"},
  \code{"euclidean"}, \code{"mahalanobis"}, \code{"manhattan"},
  \code{"minkowski"}, and \code{"kernel"}.}

  \item{\code{out.neighbor}}{Neighbor count passed to \code{outpro} and
  \code{outpro.null}.  The default \code{NULL} lets \code{outpro} choose its
  own value.}

  \item{\code{out.reduce}}{Reduced subspace passed to \code{outpro} when
  \code{vt.filter = "outpro"}.  The default \code{NULL} uses the top VarPro
  variables together with the focal variable currently being profiled.  A
  character vector selects named variables; a named numeric vector selects
  variables and supplies their weights.  In these two cases, the focal
  variable is added if absent.  Other values, such as \code{TRUE} or
  \code{FALSE}, are passed through to \code{outpro}.}

  \item{\code{out.cutoff}}{Cutoff passed to \code{outpro} when
  \code{out.reduce = TRUE}.  The default \code{NULL} lets \code{outpro} use
  its dimension-dependent default.  This is not the virtual twin threshold;
  the virtual twin threshold is \code{cut}.}

  \item{\code{out.max.rules.tree}}{Maximum number of rules per tree passed to
  \code{outpro} for forest-neighborhood distances.  Default is \code{150}.
  This option has little effect for the default \code{out.distancef = "knn"}.}

  \item{\code{out.max.tree}}{Maximum number of trees passed to \code{outpro}
  for forest-neighborhood distances.  Default is \code{150}.  This option has
  little effect for the default \code{out.distancef = "knn"}.}

  \item{\code{out.knn.chunk.size}}{Chunk size used by the \code{outpro} KNN
  distance calculation.  Default is \code{100L}.  Increasing this value can be
  faster but uses more memory.}

  \item{\code{out.null}}{Optional precomputed \code{outpro.null} object, or a
  named list of such objects keyed by profiled variable name.  This is mainly
  useful when repeatedly calling \code{partialpro} with
  \code{vt.filter = "outpro"} and the same calibration should be reused.  If
  omitted, null calibration is computed internally and cached by reduced
  subspace.}
}

\strong{Custom learner helpers.}
The package also supplies convenience learner constructors used in the
examples.  \code{rf.learner(o, ...)} re-fits a random forest using the top
VarPro variables as a gentler variable-weighting scheme and passes recognized
random-forest arguments through \code{...}.  \code{gbm.learner(o, ...)} fits a
\code{gbm} model with defaults \code{n.trees = 500},
\code{shrinkage = 0.1}, \code{interaction.depth = 3},
\code{cv.folds = 5}, and \code{n.cores = get.mc.cores()}; it uses Gaussian
loss for regression/survival scores and Bernoulli loss for two-class
classification.  \code{bart.learner(o, ...)} fits a BART regression learner
and accepts \code{mc.cores}; it applies only to regression/survival-style
VarPro objects.

Because \code{partialpro} samples cases and creates case-specific train/test
splits of the virtual grid, use \code{set.seed} before calling the function
when reproducible profiles are needed.
}

\value{
  An invisible named list with class \code{"partialpro"}.  There is one
  named component for each requested variable that remains after matching to
  \code{object$xvar.names}.  A component can be \code{NULL} if no usable
  virtual profiles are available for that variable.  Otherwise each component
  is a list with the following elements:
  \describe{
    \item{\code{case}}{Indices of the sampled training cases, or row numbers
    of \code{newdata}, retained for the variable.}

    \item{\code{xorg}}{Original observed values of the variable in
    \code{object$x}.}

    \item{\code{xvirtual}}{Virtual grid values used for the variable.}

    \item{\code{goodvt}}{Matrix with one row per retained case and one column
    per virtual value.  Entries are \code{1} for admissible virtual twins and
    \code{NA} for virtual twins removed by UVT.}

    \item{\code{yhat.par}}{Case-specific fitted partial profile matrix on the
    prediction scale.}

    \item{\code{yhat.nonpar}}{Case-specific fitted partial profile matrix on
    the prediction scale.  For the current continuous-variable implementation,
    this is the local fitted profile with a common global intercept added
    back.}

    \item{\code{yhat.causal}}{Case-specific contrast matrix centered at the
    first virtual value.}
  }
}

\author{
  Min Lu and Hemant Ishwaran
}

\references{
  Ishwaran H. (2025).  Multivariate Statistics: Classical Foundations
  and Modern Machine Learning, CRC (Chapman and Hall), in press.
}

\seealso{
  \code{\link{varpro}},
  \code{\link{isopro}},
  \code{\link{outpro}}
}

\examples{

\donttest{

##------------------------------------------------------------------
##
## Boston housing
##
##------------------------------------------------------------------

library(mlbench)
data(BostonHousing)
oldpar <- par(mfrow=c(2,3))
plot((oo.boston<-partialpro(varpro(medv~.,BostonHousing),nvar=6)))
par(oldpar)

##------------------------------------------------------------------
##
## Boston housing using newdata option
##
##
##------------------------------------------------------------------

library(mlbench)
data(BostonHousing)
o <- varpro(medv~.,BostonHousing)
oldpar <- par(mfrow=c(2,3))
plot(partialpro(o,nvar=3))
## same analysis using outpro/KNN filtering
plot(partialpro(o,nvar=3,vt.filter="outpro"))
## same but using newdata (set to first 6 cases of the training data)
plot(partialpro(o,newdata=o$x[1:6,],nvar=3))
par(oldpar)

##------------------------------------------------------------------
##
## Boston housing with externally constructed rf learner
##
##------------------------------------------------------------------

## varpro analysis
library(mlbench)
data(BostonHousing)
o <- varpro(medv~.,BostonHousing)

## default partial pro call
pro <- partialpro(o, nvar=3)

## partial pro call using built in rf learner
mypro <- partialpro(o, nvar=3, learner=rf.learner(o))

## compare the two
oldpar <- par(mfrow=c(2,3))
plot(pro)
plot(mypro, ylab="external rf learner")
par(oldpar)

##------------------------------------------------------------------
##
## Boston housing:  tree gradient boosting learner, bart learner
##
##------------------------------------------------------------------

if (library("gbm", logical.return=TRUE) &&
    library("BART", logical.return=TRUE)) {

## varpro analysis
library(parallel)
library(mlbench)
data(BostonHousing)
o <- varpro(medv~.,BostonHousing)

## default partial pro call
pro <- partialpro(o, nvar=3)

## partial pro call using built in gradient boosting learner
mypro <- partialpro(o, nvar=3, learner=gbm.learner(o, n.trees=1000, n.cores=get.mc.cores()))

## partial pro call using built in bart learner
mypro2 <- partialpro(o, nvar=3, learner=bart.learner(o, mc.cores=get.mc.cores()))

## compare the learners
oldpar <- par(mfrow=c(3,3))
plot(pro)
plot(mypro, ylab="external boosting learner")
plot(mypro2, ylab="external bart learner")
par(oldpar)
}

##------------------------------------------------------------------
##
## peak vo2 with 5 year rmst
##
##------------------------------------------------------------------

data(peakVO2, package = "randomForestSRC")
oldpar <- par(mfrow=c(2,3))
plot((oo.peak<-partialpro(varpro(Surv(ttodead,died)~.,peakVO2,rmst=5),nvar=6)))
par(oldpar)

##------------------------------------------------------------------
##
## veteran data set with celltype as a factor
##
##------------------------------------------------------------------

data(veteran, package = "randomForestSRC")
dta <- veteran
dta$celltype <- factor(dta$celltype)
oldpar <- par(mfrow=c(2,3))
plot((oo.veteran<-partialpro(varpro(Surv(time, status)~., dta), nvar=6)))
par(oldpar)

##------------------------------------------------------------------
##
## iris: classification analysis showing partial effects for all classes
##
##------------------------------------------------------------------

o.iris <- varpro(Species~.,iris)
yl <- paste("log-odds", levels(iris$Species))
oldpar <- par(mfrow=c(3,2))
plot((oo.iris.1 <- partialpro(o.iris, target=1, nvar=2)),ylab=yl[1])
plot((oo.iris.2 <- partialpro(o.iris, target=2, nvar=2)),ylab=yl[2])
plot((oo.iris.3 <- partialpro(o.iris, target=3, nvar=2)),ylab=yl[3])
par(oldpar)

##------------------------------------------------------------------
##
## iowa housing data
##
##------------------------------------------------------------------

## quickly impute the data; log transform the outcome
data(housing, package = "randomForestSRC")
housing <- randomForestSRC::impute(SalePrice~., housing, splitrule="random", nimpute=1)
dta <- data.frame(data.matrix(housing))
dta$y <- log(housing$SalePrice)
dta$SalePrice <- NULL

## partial effects analysis
o.housing <- varpro(y~., dta, nvar=Inf)
oo.housing <- partialpro(o.housing,nvar=15)
oldpar <- par(mfrow=c(3,5))
plot(oo.housing)
par(oldpar)

}
}
\keyword{plot}
