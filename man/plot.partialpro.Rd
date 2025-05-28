\name{plot.partialpro}
\alias{plot.partialpro}
\title{Partial Plots for VarPro}

\description{
 Plot partial effects of x-variable(s) from a VarPro analysis.
}

\usage{\method{plot}{partialpro}(x, xvar.names, nvar,
  parametric = FALSE, se = TRUE,
  causal = FALSE, subset = NULL, plot.it = TRUE, ...)
}

\arguments{

  \item{x}{\code{partialpro} object returned from a previous call to \code{partialpro}.}

  \item{xvar.names}{Names (or integer indices) of the x-variables to plot. Defaults to all variables.}

  \item{nvar}{Number of variables to plot. Defaults to all.}

  \item{parametric}{Logical. Set to \code{TRUE} only if the partial effect is believed to follow a polynomial form.}

  \item{se}{Display standard errors?}

  \item{causal}{Display causal estimator?}

  \item{subset}{Optional conditioning factor. Not applicable if \code{parametric = TRUE}. May also be a logical or integer vector to subset the analysis.}

  \item{plot.it}{If \code{FALSE}, no plot is produced; instead, partial effect values are returned.}

  \item{...}{Additional arguments passed to \code{plot}.}

}


\details{

Generates smoothed partial effect plots for continuous variables. The solid black line represents the estimated partial effect; dashed red lines show an approximate plus-minus standard error band. These standard errors are intended as heuristic guides and should be interpreted cautiously.

Partial effects are estimated nonparametrically using locally fitted polynomial models. This is the default behavior and is recommended when effects are expected to be nonlinear. Use \code{parametric = TRUE} if the underlying effect is believed to follow a global polynomial form.

For binary variables, partial effects are shown as boxplots, with whiskers reflecting variability analogous to standard error.

The causal estimator, when requested, displays the baseline-subtracted parametric local effect.

Conditioning is supported via the \code{subset} option. When supplied as a factor (with length equal to the original data), the plot is stratified by its levels. Alternatively, \code{subset} can be a logical or integer vector indicating the cases to include in the analysis.

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
