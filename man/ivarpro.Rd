\name{ivarpro}
\alias{ivarpro}
\title{Individual Variable Priority (iVarPro)}

\description{
 Individual Variable Priority: A Model-Independent Local
  Gradient Method for Variable Importance
}

\usage{
ivarpro(object,
        cut = seq(.05, 1, length=21),
        nmin = 20, nmax = 150,
        y.external = NULL,
        noise.na = TRUE,
        papply = mclapply,
        max.rules.tree = 150,
        max.tree = 150)
}

\arguments{

  \item{object}{\code{varpro} object from a previous call to \code{varpro}, or a \code{rfsrc} object.}

  \item{cut}{Sequence of \eqn{lambda} values used to relax the constraint region in the local linear regression model. Calibrated so that \code{cut = 1} corresponds to one standard deviation of the release coordinate.}

  \item{nmin}{Minimum number of observations required for fitting a local linear model.}

  \item{nmax}{Maximum number of observations allowed for fitting a local linear model.}

  \item{y.external}{Optional user-supplied response vector to use as the dependent variable in the local linear regression. Must match the dimension and type expected for the outcome family.}

  \item{noise.na}{Logical. If \code{TRUE} (default), gradients for noisy or non-signal variables are set to \code{NA}; if \code{FALSE}, they are set to zero.}

  \item{papply}{Apply method; either \code{mclapply} or \code{lapply}.}

  \item{max.rules.tree}{Optional. Maximum number of rules per tree. If unspecified, the value from the \code{varpro} object is used.}

  \item{max.tree}{Optional. Maximum number of trees used for rule extraction. If unspecified, the value from the \code{varpro} object is used.}

}

\details{

Understanding individual-level variable importance is critical in
applications where personalized decisions are required. Traditional
variable importance methods focus on average (population-level) effects
and often fail to capture heterogeneity across individuals. In many
real-world problems, it is not sufficient to determine whether a
variable is important on average, we must also understand how it affects
individual predictions.


The VarPro framework identifies feature-space regions through rule-based
splitting and computes importance using only observed data. This avoids
biases introduced by permutation or synthetic data, leading to robust,
population-level importance estimates. However, VarPro does not directly
capture individual-level effects.


To address this limitation, individual variable priority (iVarPro)
extends VarPro by estimating the local gradient of each
feature, quantifying how small changes in a variable influence an
individual's predicted outcome. These gradients serve as natural
measures of sensitivity and provide an interpretable notion of
individualized importance.

iVarPro leverages the release region concept from VarPro. A region
\eqn{R} is first defined using VarPro rules. Since using only data
within \eqn{R} often results in insufficient sample size for stable
gradient estimation, iVarPro releases \eqn{R} along a coordinate
\eqn{s}. This means the constraint on \eqn{s} is removed while all
others are held fixed, yielding additional variation specifically in the
\eqn{s}-direction, precisely what is needed to compute directional
derivatives.

Local gradients are then estimated via linear regression on the expanded
region. The parameter \code{cut} controls the amount of constraint
relaxation. A value of \code{cut = 1} corresponds to one standard
deviation of the release coordinate, calibrated automatically from the
data.


The flexibility of this framework makes it suitable for quantifying
individual-level importance in regression, classification, and survival
settings.


}

\author{

  Min Lu and Hemant Ishwaran

}

\references{
  
  Lu, M. and Ishwaran, H. (2025). Individual variable priority: a
  model-independent local gradient method for variable importance.


}

\seealso{
  \command{\link{varpro}}
}

\examples{
\donttest{
## ------------------------------------------------------------
##
## synthetic regression example 
##
## ------------------------------------------------------------

## true regression function
true.function <- function(which.simulation) {
  if (which.simulation == 1) {
    function(x1,x2) {1*(x2<=.25) +
      15*x2*(x1<=.5 & x2>.25) + (7*x1+7*x2)*(x1>.5 & x2>.25)}
  }
  else if (which.simulation == 2) {
    function(x1,x2) {r=x1^2+x2^2;5*r*(r<=.5)}
  }
  else {
    function(x1,x2) {6*x1*x2}
  }
}

## simulation function
simfunction = function(n = 1000, true.function, d = 20, sd = 1) {
  d <- max(2, d)
  X <- matrix(runif(n * d, 0, 1), ncol = d)
  dta <- data.frame(list(x = X, y = true.function(X[, 1], X[, 2]) + rnorm(n, sd = sd)))
  colnames(dta)[1:d] <- paste("x", 1:d, sep = "")
  dta
}

## iVarPro importance plot
ivarpro.plot <- function(dta, release=1, combined.range=TRUE,
                     cex=1.0, cex.title=1.0, sc=5.0, gscale=30, title=NULL) {
  x1 <- dta[,"x1"]
  x2 <- dta[,"x2"]
  x1n = expression(x^{(1)})
  x2n = expression(x^{(2)})
  if (release==1) {
    if (is.null(title)) title <- bquote("iVarPro Estimated Gradient " ~ x^{(1)})
    cex.pt <- dta[,"Importance.x1"]
  }
  else {
    if (is.null(title)) title <- bquote("iVarPro Estimated Gradient " ~ x^{(2)})
    cex.pt <- dta[,"Importance.x2"]
  }
  if (combined.range) {
    cex.pt <- cex.pt / max(dta[, c("Importance.x1", "Importance.x2")],na.rm=TRUE)
  }
  rng <- range(c(x1,x2))
  par(mar=c(4,5,5,1),mgp=c(2.25,1.0,0))
  par(bg="white")
  gscalev <- gscale
  gscale <- paste0("gray",gscale)
  plot(x1,x2,xlab=x1n,ylab=x2n,
       ylim=rng,xlim=rng,
       col = "#FFA500", pch = 19,
       cex=(sc*cex.pt),cex.axis=cex,cex.lab=cex,
       panel.first = rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], 
                          col = gscale, border = NA))
  abline(a=0,b=1,lty=2,col= if (gscalev<50) "white" else "black")
  mtext(title,cex=cex.title,line=.5)
}

## simulate the data
which.simulation <- 1
df <- simfunction(n = 500, true.function(which.simulation))

## varpro analysis
o <- varpro(y~., df)

## canonical ivarpro analysis
imp1 <- ivarpro(o)

## ivarpro analysis with custom lambda
imp2 <- ivarpro(o, cut = seq(.05, .75, length=21))

## build data for plotting the results
df.imp1 <- data.frame(Importance = imp1, df[,c("x1","x2")])
df.imp2 <- data.frame(Importance = imp2, df[,c("x1","x2")])

## plot the results
par(mfrow=c(2,2))
ivarpro.plot(df.imp1,1)
ivarpro.plot(df.imp1,2)
ivarpro.plot(df.imp2,1)
ivarpro.plot(df.imp2,2)

}}

\keyword{individual importance}
