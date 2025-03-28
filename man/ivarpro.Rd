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
  
  \item{object}{VarPro object obtained from previous call to
  \code{varpro} or a \code{rfsrc} object.}

  \item{cut}{Sequence of lambda values using for relaxing the constraint
  region in the local linear regression model.  Calibrated so that a
  value of 1 (one) corresponds to a one standard deviation of the
  release coordinate.}

  \item{nmin}{Smallest sample size allowed in a local linear regression
  model.}

  \item{nmax}{Largest sample size allowed in a local linear regression
  model.}

  \item{y.external}{User supplied external y value used as the dependent
  variable in the local linear regression.  Must match the dimension and
  be appropriate for the family.}

  \item{noise.na}{Logical indicating whether 'NA' (default) or 0 should
  be used for the gradient for a noisy or non-signal variable.}
  
  \item{papply}{Use mclapply or lapply.}

  \item{max.rules.tree}{Optional. Maximum number of rules per tree.
    If left unspecified, uses the value from the VarPro object.}

  \item{max.tree}{Maximum number of trees used for extracting rules.
    If left unspecified, uses the value from the VarPro object.}
}

\details{

  Understanding the individual importance of variables is a crucial yet
  underexplored aspect of statistical modeling and machine
  learning. Traditional methods quantify variable effects at the
  population level but often fail to capture heterogeneity across
  individuals. In many applications, particularly those requiring
  personalized decisions, it is insufficient to determine whether a
  variable is important on average; rather, we must understand how it
  influences specific predictions.

  VarPro defines localized feature space regions using data-driven
  splitting rules and computes importance scores from these regions. By
  relying solely on observed data, VarPro avoids biases introduced by
  permutations and artificial data, ensuring a more robust approach to
  variable selection. Although VarPro addresses bias in population-level
  importance, it does not account for individualized effects.

  To bridge this gap, we propose individual variable priority (iVarPro),
  which defines feature importance based on the local gradient of each
  feature, providing a more interpretable measure. The gradient
  quantifies how small perturbations in a variable influence an
  individual's predicted outcome, serving as a natural measure of
  sensitivity.

  Due to the consistency property of VarPro, the regions (R) defined by
  VarPro rules can be used for studying meaningful variations in the
  target function. iVarPro harnesses this property by employing a local
  linear regression model within a region (R) to estimate the
  gradient. However, using only observations within R is often
  insufficient, as sample sizes within these small regions are typically
  too limited for stable estimation.  Therefore, iVarPro leverages the
  release region concept from the VarPro framework. Releasing (R) along a
  coordinate (s) expands the subset of individuals considered by
  removing constraints on (s) while preserving all other constraints
  defining (R). This targeted expansion introduces additional variation
  specifically along (s), which is precisely what is needed to compute
  the directional derivative.

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
