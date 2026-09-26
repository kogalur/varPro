\name{plot.partialpro}
\alias{plot.partialpro}
\title{Plot Partial Profiles from a VarPro Analysis}

\description{
  Display partial profiles from a \code{partialpro} analysis. Examine
  how a model's predictions change with a predictor, compare groups of
  observations, or show changes relative to a baseline value.
}

\usage{\method{plot}{partialpro}(x, xvar.names, nvar,
  parametric = FALSE, se = TRUE,
  causal = FALSE, subset = NULL, plot.it = TRUE, ...)
}

\arguments{
  \item{x}{An object returned by \code{partialpro}.}

  \item{xvar.names}{Predictor names or positive integer positions in
    \code{x} to plot. Defaults to all predictors in \code{x}.
    Unavailable names are skipped with a warning.}

  \item{nvar}{Optional nonnegative integer limiting how many predictors
    to plot. Applies after choosing \code{xvar.names}. The default,
    or \code{Inf}, uses all selected predictors.}

  \item{parametric}{Average the stored polynomial profiles directly?
    The default \code{FALSE} smooths the average profile using loess.
    Ignored when \code{causal = TRUE} and for binary predictors.}

  \item{se}{Display approximate uncertainty guides? Continuous curves
    have bands at plus and minus two standard errors. Binary predictors
    use box-style displays based on the same limits. See Details.}

  \item{causal}{Display changes relative to each profile's value at
    the first grid point? This takes precedence over \code{parametric}.
    See Details for interpretation.}

  \item{subset}{Optional factor for comparing groups, or a logical
    or positive integer vector selecting observations. Applies to
    both values of \code{parametric} and to \code{causal = TRUE}.
    See Details for matching observations.}

  \item{plot.it}{Draw the plots? Set \code{FALSE} to return the curves
    and plotting summaries without drawing them.}

  \item{...}{Additional graphical arguments, such as \code{ylab},
    \code{xlab}, \code{ylim}, and \code{lwd}. Axis labels may be
    vectors, and \code{ylim} may be a list, with entries corresponding
    to the selected predictors.}
}

\details{
  \code{plot.partialpro} summarizes the partial profiles calculated by
  \code{partialpro}. Each profile describes how a model's predictions
  change as one predictor varies while the other predictors stay fixed
  at an observation's values. The plots show the average relationship
  with that predictor and can compare this relationship across groups
  of observations.

  After computing \code{pro <- partialpro(model)}, use \code{plot(pro)}
  to display the profiles. Supply \code{xvar.names} to choose predictors
  or \code{nvar} to limit how many are shown. The plotting function
  summarizes the stored results; predictor selection, profile fitting,
  and filtering of unsupported predictor combinations are controlled
  in the original \code{partialpro} call.

  \subsection{Interpreting the plots}{
    For a continuous predictor, the horizontal axis shows the grid of
    predictor values and the vertical axis shows the average partial
    profile. By default, this average is smoothed using loess, with
    greater weight given to grid values supported by more observations.
    In the ungrouped display, the solid black line shows this curve.
    Set \code{parametric = TRUE} to average the stored polynomial
    profiles directly, without this additional smoothing.

    Binary predictors have a box-style display at each of their two
    values. The center shows the average profile value. These boxes
    summarize the mean and its uncertainty guides, rather than the
    distribution of individual observations.

    For regression and survival, the stored profiles use the learner's
    prediction scale. For classification, they use the log-odds scale
    for the class selected in \code{partialpro}.
  }

  \subsection{Uncertainty guides}{
    With \code{se = TRUE}, continuous curves have dotted lines at plus
    and minus two standard errors. The lines are red in the ungrouped
    display and match the group colors in grouped displays. Binary
    boxes are constructed from the mean and these two limits; their
    whiskers mark the limits. Set \code{se = FALSE} to omit the guides.

    For continuous predictors, the standard error uses the variation
    across stored profiles and the number of observations whose
    virtual twins pass the filter at each grid value. For binary
    predictors, it uses the number of available profile values. These
    are informal summaries of variation across observations, not
    confidence intervals accounting for model fitting and variable
    selection. A standard error is unavailable when fewer than two
    profile values can be used; the mean can still be displayed.
  }

  \subsection{Comparing groups or selecting observations}{
    Supply a factor through \code{subset} to compare its groups, for
    example \code{plot(pro, subset = group)}. Its levels define the
    groups shown in the plots. A logical vector selects rows marked
    \code{TRUE}; a vector of positive integers selects those row
    positions. Grouping and row selection work with both smoothed
    and polynomial summaries.

    Match \code{subset} to the observations used to create the
    profiles. When \code{partialpro} sampled training observations,
    use the row order of the fitted VarPro object's \code{x} data.
    When \code{partialpro} used \code{newdata}, use the row order of
    that data instead. Supply the factor or logical vector for that
    complete set of observations, not just the sampled profile rows.
    Each predictor's stored \code{case} indices identify which rows
    contribute to its summary. Missing group labels and empty groups
    are omitted.
  }

  \subsection{Changes relative to a baseline}{
    Set \code{causal = TRUE} to summarize changes from a common predictor
    baseline. For each observation, \code{partialpro} subtracts the
    profile value at the first grid point, \code{xvirtual[1]}, from
    the values at all grid points. The plotting function averages
    these contrasts and applies the default smoothing for continuous
    predictors. Positive contrasts indicate an increase from the
    baseline; negative contrasts indicate a decrease.

    The baseline is the first grid value, which need not be zero or
    the observation's own predictor value. These are model-based
    contrasts; a causal interpretation requires additional assumptions
    about the study and the relationships between variables. For an
    ungrouped binary display, only the contrast at the second value
    is drawn; grouped binary displays also show the zero baseline.
  }

  \subsection{Obtaining the plotting summaries}{
    Use \code{plot(pro, plot.it = FALSE)} to obtain the plotted values
    without drawing. There is one entry per predictor, or per
    predictor and group when \code{subset} is a factor. Each entry
    contains the predictor values, mean profile, and standard errors.
    These summaries can be used for inspection or custom plots.
  }
}

\value{
  With \code{plot.it = TRUE}, produces the requested plots and returns
  \code{NULL}.

  With \code{plot.it = FALSE}, returns a named list of summaries.
  Ungrouped entries are named by predictor; grouped entries combine
  the predictor and group names. Each entry contains:
  \describe{
    \item{\code{x}}{Predictor grid values used for the summary.}
    \item{\code{y}}{Mean profile values, after smoothing when applicable.}
    \item{\code{y.se}}{Approximate standard errors at the corresponding
      grid values. Unavailable standard errors are \code{NA}.
      With \code{se = FALSE}, this component is zero.}
  }
  Predictors and groups with no usable summary are omitted. Returns
  \code{NULL} when no summaries are available. Binary contrast summaries
  include both grid values even when the ungrouped plot shows only
  the second.
}

\author{

  Min Lu and Hemant Ishwaran

}

\references{

  Ishwaran H. (2025). Multivariate Statistics: Classical Foundations
  and Modern Machine Learning, CRC (Chapman and Hall), in press.

}

\seealso{
  \command{\link{partialpro}}
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
o.boston <- varpro(medv~., BostonHousing)
oo.boston <- partialpro(o.boston, nvar = 4, learner = rf.learner(o.boston))

oldpar <- par(mfrow = c(2, 4))

## Polynomial summary of the partial profiles
plot(oo.boston, parametric = TRUE, ylab = "parametric est.")

## Smoothed partial-profile summary (default)
plot(oo.boston, parametric = FALSE, ylab = "non-parametric est.")

par(oldpar)

##------------------------------------------------------------------
##
## Boston housing with subsetting
##
##------------------------------------------------------------------

library(mlbench)
data(BostonHousing)
o.boston <- varpro(medv~., BostonHousing)
oo.boston <- partialpro(o.boston, nvar = 3, learner = rf.learner(o.boston))

## Compare profiles for lower- and higher-priced homes
price <- BostonHousing$medv
pricef <- factor(price > median(price), labels = c("low priced", "high priced"))
oldpar <- par(mfrow = c(1, 1))
plot(oo.boston, subset = pricef, nvar = 1)
par(oldpar)

##------------------------------------------------------------------
##
## veteran data with subsetting using celltype as a factor
##
##------------------------------------------------------------------

data(veteran, package = "randomForestSRC")
dta <- veteran
dta$celltype <- factor(dta$celltype)
o.vet <- varpro(Surv(time, status) ~ ., dta)
oo.vet <- partialpro(o.vet, nvar = 6, nsmp = Inf, learner = rf.learner(o.vet))

## Compare partial profiles across cell types
oldpar <- par(mfrow = c(2, 3))
plot(oo.vet, subset = dta$celltype)
par(oldpar)

## Compare changes from the first grid value across cell types
oldpar <- par(mfrow = c(2, 3))
plot(oo.vet, subset = dta$celltype, causal = TRUE)
par(oldpar)

## Inspect the plotting summaries without drawing
obj <- plot(oo.vet, subset = dta$celltype, plot.it = FALSE)
str(obj, max.level = 1)

}
}
\keyword{plot}
