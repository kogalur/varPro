\name{partialpro}
\alias{partialpro}
\title{Partial Prediction Profiles for Individual Observations}

\description{
  Shows how a model's predictions change as one predictor is varied,
  while the other predictors are held fixed for each observation.
  Produces individual prediction profiles that can be displayed with
  \code{plot}. Optional filtering identifies predictor combinations
  that are unusual relative to the training data.
}

\usage{
partialpro(object, xvar.names, nvar,
           target, learner, newdata, method = c("unsupv", "rnd", "auto"),
           verbose = FALSE, vt.filter = c("isopro", "outpro", "none"),
           ...)
}

\arguments{
  \item{object}{A fitted \code{varpro} object for regression,
    classification, or survival analysis. Multivariate regression
    is not supported.}

  \item{xvar.names}{Optional character vector of predictors to examine.
    The default uses the VarPro importance ranking returned by
    \code{get.topvars(object)}. Names must match
    \code{object$xvar.names}, including indicator-column names for
    factors. Unavailable names are skipped with a warning.}

  \item{nvar}{Optional positive integer limiting the number of predictors
    examined. Uses the first \code{nvar} entries of \code{xvar.names},
    or the first \code{nvar} predictors in the default ranking.
    The limit is applied before unavailable names are skipped.
    Use \code{Inf} for all requested predictors.}

  \item{target}{For classification, a class label or an integer
    probability-column index. The default is the last level of the
    original response factor. Class labels are matched to named
    probability columns; unnamed columns must follow the response's
    class-level order. For regression and survival, the first
    prediction column is used and \code{target} is ignored.}

  \item{learner}{Optional prediction function. The default uses the
    forest in \code{object$rf}. A supplied function must accept a
    data frame with the predictor columns in \code{object$x} and
    return numeric predictions in the same row order. Classification
    requires one probability column per class. See Details.}

  \item{newdata}{Optional data frame of observations for which to
    calculate profiles. All supplied rows are used. The data must
    contain the numeric predictor columns stored in \code{object$x},
    including any factor indicator columns. For example, use
    \code{newdata = object$x[1:6, , drop = FALSE]} to examine six
    training observations. Extra columns are ignored. When omitted,
    up to \code{nsmp} training observations are sampled separately
    for each predictor.}

  \item{method}{Isolation forest method passed to \code{isopro}.
    Choices are \code{"unsupv"}, \code{"rnd"}, and \code{"auto"}.
    Used only when filtering with \code{vt.filter = "isopro"}.}

  \item{verbose}{Print the name of each predictor as it is processed?}

  \item{vt.filter}{Method for identifying unusual predictor
    combinations: \code{"isopro"} uses isolation forests,
    \code{"outpro"} uses distances from the training data, and
    \code{"none"} turns filtering off. See Details.}

  \item{...}{Named options controlling the prediction grid, filtering,
    and curve fitting. See the options below. Unnamed, unrecognized,
    and duplicate options cause an error.}
}

\details{
  \code{partialpro} helps explain a model by showing how its predictions
  change when one predictor changes. For each observation, it varies
  that predictor over a grid of values while keeping the other
  predictors fixed at the observation's values. The resulting
  predictions are summarized by a curve, called a partial profile.
  Comparing these curves shows how the model's relationship with a
  predictor varies across observations.

  Use \code{p <- partialpro(object, nvar = 3)} to calculate profiles
  for the first three predictors in the VarPro importance ranking,
  then \code{plot(p)} to display them. The function uses the model's
  training observations by default; supply \code{newdata} to examine
  particular observations instead.

  \subsection{Interpreting the profiles}{
    A rising profile indicates that larger predictor values give
    larger predictions when the other predictors are held fixed.
    Different curve shapes show how this relationship depends on
    the values of the other predictors.

    The prediction scale is set by the learner. The default forest
    supplies predicted responses for regression and mortality
    predictions for survival. For classification, the selected
    class probability is converted to log-odds, so larger values
    indicate a greater predicted probability of that class.
    Probabilities are limited to \code{[0.001, 0.999]} before this
    conversion to keep log-odds finite.

    For continuous predictors, the fitted curves share a common
    average intercept so their shapes can be compared. The returned
    \code{yhat.causal} component instead expresses each curve as
    changes relative to its value at the first grid point. These
    are model-based contrasts; a causal interpretation requires
    appropriate study-design and substantive assumptions.
  }

  \subsection{Predictors and observations}{
    Profiles use the numeric predictor columns stored in
    \code{object$x}. A factor can therefore have several profiles,
    one for each indicator column available in the fitted forest.
    Each profile varies that column while keeping the others fixed.
    Supplied \code{newdata} must use these same columns; original
    factor columns are not converted automatically.

    The grid always comes from the training data, including when
    \code{newdata} is supplied. Predictors with few distinct values
    use those values. For other predictors, the grid uses quantiles
    within the central training range, controlled by \code{alpha}
    and \code{nvirtual}.

    Use \code{set.seed} before calling \code{partialpro} for
    reproducible results. Randomness includes sampling training
    observations and dividing grid values for the curve-fitting
    comparisons described below.
  }

  \subsection{Filtering unusual predictor combinations}{
    Changing one predictor can create a combination of values that
    is uncommon in the training data. The modified observations
    are called \emph{virtual twins}. Unlimited Virtual Twins (UVT)
    filtering assesses these combinations before fitting the
    profiles.

    The default \code{vt.filter = "isopro"} uses an isolation
    forest fitted to the predictors in the VarPro importance
    ranking. A virtual twin passes the filter when its
    \code{isopro} support score is at least \code{cut}.

    With \code{vt.filter = "outpro"}, distances are compared with
    reference distances from \code{outpro.null}. The support score
    is \code{1 - F0(distance)}, where \code{F0} is the reference
    empirical distribution function. For example, with
    \code{cut = 0.1}, a virtual twin passes when at least ten
    percent of the reference distances are greater than its
    distance. The default uses nearest-neighbor distances and
    training observations for the reference distribution.

    Larger values of \code{cut} allow fewer virtual twins to pass.
    Use \code{cut = 0} or \code{vt.filter = "none"} to turn off
    filtering. The two filtering methods use different support
    scores, so the same \code{cut} need not accept the same twins.
  }

  \subsection{Fitting the curves}{
    A polynomial curve is fitted separately for each observation
    and predictor; the default is quadratic. When filtering is
    active and enough values are available, the function compares
    curves fitted with and without filtering. It fits them on
    part of the grid and compares their prediction errors on the
    remaining grid values. The unrestricted curve is used only
    when both errors are available and its standardized mean
    squared error is smaller by more than \code{mse.tolerance}.
    Otherwise, fitting uses the values that pass the filter.

    A fitted curve is evaluated over the full grid. Values rejected
    by the filter can therefore involve extrapolation. For a
    predictor with two distinct values, predictions at those values
    are used directly, and both must pass the filter for a profile
    to be available. A polynomial that cannot be fitted has missing
    profile values.
  }

  \subsection{Grid and curve-fitting options}{
    Supply these options through \code{...}:
    \describe{
      \item{\code{nsmp}}{Maximum number of training observations
        sampled for each predictor. Default is \code{250}.
        Ignored when \code{newdata} is supplied.}
      \item{\code{nvirtual}}{Requested number of grid values for
        predictors with many distinct values. Default is \code{100}.
        Repeated quantiles are combined, so the grid may be shorter.}
      \item{\code{alpha}}{Fraction excluded from each tail when
        constructing a continuous predictor's grid. Default is
        \code{0.025}. Predictors with few distinct values use all
        their values.}
      \item{\code{df}}{Polynomial degree. Default is \code{2} for
        quadratic curves. The value is rounded and is at least one.}
      \item{\code{nmin}}{Required number of grid values passing the
        filter for a continuous-predictor profile. Default is
        \code{15}, reduced to half the number of distinct training
        values when that number is smaller.}
      \item{\code{mse.tolerance}}{Required improvement in standardized
        prediction error before using an unrestricted curve.
        Default is \code{0}.}
    }
  }

  \subsection{Filtering options}{
    Supply these options through \code{...}:
    \describe{
      \item{\code{cut}}{Minimum support score for a virtual twin
        to pass the filter. Default is \code{0.1}; \code{0} turns
        filtering off.}
      \item{\code{sampsize, ntree, nodesize}}{Isolation forest
        settings used with \code{vt.filter = "isopro"}. Defaults
        are \code{function(x) min(2^8, 0.632 * x)}, \code{500},
        and \code{1}, respectively. See \code{\link{isopro}}.}
      \item{\code{out.distancef}}{Distance method used with
        \code{vt.filter = "outpro"}. Default is \code{"knn"}.
        See \code{\link{outpro}} for other distance methods and
        their interpretation.}
      \item{\code{out.neighbor}}{Number of neighbors used by
        \code{outpro}. Default is \code{NULL}, which uses the
        \code{outpro} default.}
      \item{\code{out.reduce}}{Predictors and weights for
        \code{outpro}. Default is \code{NULL}, which uses the
        VarPro-ranked predictors together with the predictor being
        profiled. A character vector specifies predictor columns;
        a named numeric vector also supplies relative weights.
        With either vector, the profiled predictor is added if
        absent, with relative weight one. Logical values are passed
        directly to \code{outpro}.}
      \item{\code{out.cutoff}}{Variable-selection cutoff passed to
        \code{outpro} when \code{out.reduce = TRUE}. Default is
        \code{NULL}, which uses the \code{outpro} default. The
        virtual-twin support threshold is set by \code{cut}.}
      \item{\code{out.max.rules.tree, out.max.tree}}{Rule and tree
        limits for forest-based \code{outpro} distances. Both
        default to \code{150}. Ignored for \code{"knn"}.}
      \item{\code{out.knn.chunk.size}}{Number of virtual observations
        processed together for nearest-neighbor distances. Default
        is \code{100L}. Smaller values reduce temporary memory use.}
      \item{\code{out.null}}{Optional \code{outpro.null} result, or
        a named list of results with one entry per profiled predictor.
        Use reference distances calculated with the same model,
        predictors, weights, and distance settings as the virtual
        twins. When omitted, reference distances are calculated
        from the training observations and reused when settings
        match.}
    }
  }

  \subsection{Using another prediction model}{
    Supply \code{learner} to explain predictions from another model
    using the same observations and predictor grids. The function
    receives only predictor columns and must return one prediction
    row for every input row, in the same order. For regression and
    survival, a numeric vector is sufficient; a numeric matrix or
    data frame is also accepted, using its first column. For
    classification, supply a numeric probability matrix or data
    frame with one column per class. Class names allow
    \code{target} to select a class regardless of column order;
    a numeric \code{target} selects by column position.

    The package supplies three learner constructors:
    \describe{
      \item{\code{rf.learner(object, ...)}}{Fits another random
        forest, giving equal split weights to the VarPro-ranked
        predictors and zero weight to the others. Recognized forest
        arguments can be supplied through \code{...}; the formula,
        data, split weights, and performance setting are set by
        the helper.}
      \item{\code{gbm.learner(object, ...)}}{Fits a gradient-boosted
        model for a numeric target or a two-class response. Options
        are \code{n.trees = 500}, \code{shrinkage = 0.1},
        \code{interaction.depth = 3}, \code{cv.folds = 5}, and
        \code{n.cores = get.mc.cores()}.}
      \item{\code{bart.learner(object, ...)}}{Fits a Bayesian
        additive regression tree model for a numeric target.
        Accepts \code{mc.cores} for prediction, defaulting to
        \code{get.mc.cores()}.}
    }
    For survival analyses, these helpers fit the numeric target
    stored in \code{object$y}, such as mortality or a single
    restricted mean survival time. This can differ from the
    default learner, which predicts directly from
    \code{object$rf}. The helpers reject unnamed, duplicate,
    and unrecognized options.
  }
}

\value{
  Invisibly, a named list of class \code{"partialpro"}, with one
  component per available requested predictor. A component is
  \code{NULL} when no profiles can be constructed for that predictor.
  Otherwise it contains:
  \describe{
    \item{\code{case}}{Row numbers in \code{object$x}, or in
      \code{newdata} when supplied, for the observations included
      in the result.}
    \item{\code{xorg}}{The predictor's values in the training data.}
    \item{\code{xvirtual}}{Grid values at which profiles are evaluated.}
    \item{\code{goodvt}}{Matrix indicating which virtual twins pass
      the filter: \code{1} for a passing value and \code{NA}
      otherwise. All entries are one when filtering is off.}
    \item{\code{yhat.par}}{Matrix of fitted profiles. Continuous
      predictors use polynomial curves with a common average
      intercept; two-valued predictors use direct predictions.
      Classification profiles are on the log-odds scale.}
    \item{\code{yhat.nonpar}}{The same fitted profiles, calculated
      by centering each fitted curve and adding the common average
      intercept for continuous predictors. Despite the component
      name, these also use the polynomial fits.}
    \item{\code{yhat.causal}}{Profile changes relative to the
      first grid value, calculated separately for each observation.}
  }
  All matrices have one row per observation in \code{case} and one
  column per value in \code{xvirtual}. Unavailable fits have
  \code{NA} profile values. If none of the requested predictors
  are available, the function returns \code{NULL}.
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
## peak VO2: use five-year RMST for variable priority
## Profiles below explain the default survival forest's mortality predictions.
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
