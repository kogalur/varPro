\name{plot.partialpro.varpro}
\alias{plot.partialpro.varpro}
\alias{plot.partialpro}
\title{Partial Plots for VarPro}

\description{
 Plot partial effects of x-variable(s) from a VarPro analysis.
}

\usage{\method{plot.partialpro}{varpro}(x, xvar.names, nvar,
  parametric = FALSE, se = TRUE,
  causal = FALSE, subset = NULL, plot.it = TRUE, ...)
}

\arguments{
  
  \item{x}{partialpro object obtained from previous call to \code{partialpro}.}

  \item{xvar.names}{Names of the x-variables to be plotted (uses all by
    default). Can also be integer values.}

  \item{nvar}{Number of variables to be plotted. Default is all.}
  
  \item{parametric}{Set this to true only if you think the partial
    effect is a polynomial.}

  \item{se}{Display standard errors?}

  \item{causal}{Display causal estimator?}

  \item{subset}{Optional factor for conditioning the partial effects.
  Does not apply if \code{parametric}='TRUE'.}

  \item{plot.it}{If \code{FALSE} no plots are generated and instead
  partial effects values are returned.}
  
  \item{...}{Additional options passed to plot.}
  
}

\details{

  Generates a smooth partial plot for continuous variables. Smoothed
  black lines are the partial effect and dashed red lines indicate a
  smoothed error bar of +/- two standard errors.  Note these
  "standard errors" are meant only to be a guide and should be
  interpreted with caution.  For binary variables results are displayed
  as boxplots with whiskers used to indicate the standard error.

  Partial effects are estimated nonparametrically using locally fit
  polynomial models.  These are the values displayed by default and
  should be used in any setting where the partial effect is expected to
  be nonlinear.  Use option \code{parametric} if the true effect is
  believed to be a polynomial.

  A causal estimator can be requested in which case the value displayed
  is the baseline subtracted parametric local estimator.

  Plots can be conditioned using option \code{subset} which takes the
  form of a factor of length equal to the original data sample size.
  This produces partial effects conditioned on the levels of the
  provided factor.  \code{subset} can also be specified by a vector of
  logical values or a vector of integer values indicating cases of
  interest. In this scenario, the analysis is confined to the requested
  subset of the data.

}

\author{

  Min Lu and Hemant Ishwaran

}

\references{
  
  Ishwaran H. (2025).  Multivariate Statistics: Classical Foundations
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
o.boston <- varpro(medv~.,BostonHousing)
oo.boston <- partialpro(o.boston, nvar=4, learner=rf.learner(o.boston))

par(mfrow=c(2,4))

## parametric local estimation (default)
plot(oo.boston, ylab="parametric est.")

## non-parametric local estimation
plot(oo.boston, parametric=FALSE, ylab="non-parametric est.")


##------------------------------------------------------------------
##
## Boston housing with subsetting
##
##------------------------------------------------------------------

library(mlbench)
data(BostonHousing)
o.boston <- varpro(medv~.,BostonHousing)
oo.boston <- partialpro(o.boston, nvar=3, learner=rf.learner(o.boston))

## subset analysis
price <- BostonHousing$medv
pricef <- factor(price>median(price), labels=c("low priced","high priced"))
par(mfrow=c(1,1))
plot(oo.boston, subset=pricef, nvar=1)


##------------------------------------------------------------------
##
## veteran data with subsetting using celltype as a factor
##
##------------------------------------------------------------------

data(veteran, package = "randomForestSRC")
dta <- veteran
dta$celltype <- factor(dta$celltype)
o.vet <- varpro(Surv(time, status)~., dta)
oo.vet <- partialpro(o.vet, nvar=6, nsmp=Inf, learner=rf.learner(o.vet))

## partial effects, with subsetting
par(mfrow=c(2,3))
plot(oo.vet, subset=dta$celltype)

## causal effects, with subsetting
par(mfrow=c(2,3))
plot(oo.vet, subset=dta$celltype, causal=TRUE)

}
}
\keyword{plot}
