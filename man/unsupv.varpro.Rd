\name{unsupv.varpro}
\alias{unsupv.varpro}
\title{Unsupervised Variable Selection using Variable Priority (VarPro)}
\description{
  Selects Variables in Unsupervised Problems using Variable Priority
  (VarPro).
}

\usage{
unsupv.varpro(data, nvar = 20, 
       mode = c("sh1", "sh2", "none"), 
       tolerance = 1e-10,
       ntree = 150, nodesize = NULL,
       max.rules.tree = 150, max.tree = 150,
       papply = mclapply, verbose = FALSE, seed = NULL,
       ...)
}

\arguments{

  \item{data}{Data frame containing the usupervised data.}

  \item{nvar}{Upper limit to number of variables returned.}

  \item{mode}{Method used for creating aritifical two-class data used
    for filtering variables.  Choices are "sh1" (mode 1) or 
    "sh2" (mode 2) of Shi-Horvath's (2006) artificial data generating
    procedure.  Selecting "none" by-passes the filtering step.}

  \item{tolerance}{Tolerance used for filtering variables.}

  \item{ntree}{Number of trees to grow.}

  \item{nodesize}{Nodesize of trees. If not specified, value is set
  using an internal function optimized for sample size and dimension.}

  \item{max.rules.tree}{Maximum number of rules per tree.}

  \item{max.tree}{Maximum number of trees used for extracting rules.}

  \item{papply}{Use mclapply or lapply.}

  \item{verbose}{Print verbose output?}

  \item{seed}{Seed for repeatability.}

  \item{...}{Further arguments to be passed to \command{varpro}.}

}

\details{

  Breiman (2003; see also Shi-Horvath, 2006) transform the unsupervised
  learning problem into a two class supervised problem.  The first class
  consists of the original observations, while the second class is
  artificially created.  Breiman's method is generally used for
  unsupervised learning, however here we use the artificially created
  data for the purpose of unsupervised variable selection. Specifically,
  VarPro is applied by running a two-class classifier on the
  artificially created two class data.  The result of applying VarPro is
  a ranked set of variables. The top \code{nvar} of these are then used
  in a random forest autoencoder run by fitting the selected variables
  against themselves - a special type of multivariate forest.  Then
  using rules acquired from the multivariate forest, VarPro importance is
  obtained where importance is measured by overall feature variance.

  Artificial data is created using "mode 1" or "mode 2" of Shi-Horvath
  (2006).  Mode 1 (default) randomly permutes each set of observed
  features.  Mode 2 draws a uniform value from the minimum and maximum
  values of a feature.

  Selecting \code{mode}="none" by-passes the filtering step in which
  case the random forests autoencoder is applied to all variables.  Only
  use this option for low-dimensional problems.

  
}
  
\value{

  A VarPro object.
 
}

\author{
  Min Lu and Hemant Ishwaran
}

\references{

  Breiman, L. (2003). \emph{Manual on setting up, using and
  understanding random forest, V4.0}.  University of California
  Berkeley, Statistics Department, Berkeley.

  Lu M. and  Ishwaran H. (2022).  Variable Priority for Variable
  Selection in Rule-Based Models.

   Shi, T. and Horvath, S. (2006). Unsupervised learning with random forest
  predictors. \emph{Journal of Computational and Graphical Statistics},
  15(1):118-138. 

}

\seealso{
  \command{\link{varpro}}
}

\examples{
## ------------------------------------------------------------
## boston housing
## ------------------------------------------------------------

data(BostonHousing, package = "mlbench")

## default call
o <- unsupv.varpro(BostonHousing)
print(importance(o))

\donttest{

## ------------------------------------------------------------
## latent variable simulation
## ------------------------------------------------------------

n <- 1000
w <- rnorm(n)
x <- rnorm(n)
y <- rnorm(n)
z <- rnorm(n)
ei <- matrix(rnorm(n * 20, sd = sqrt(.1)), ncol = 20)
e21 <- rnorm(n, sd = sqrt(.4))
e22 <- rnorm(n, sd = sqrt(.4))
wi <- w + ei[, 1:5]
xi <- x + ei[, 6:10]
yi <- y + ei[, 11:15]
zi <- z + ei[, 16:20]
h1 <- w + x + e21
h2 <- y + z + e22
dta <- data.frame(w=w,wi=wi,x=x,xi=xi,y=y,yi=yi,z=z,zi=zi,h1=h1,h2=h2)

## default call
print(importance(unsupv.varpro(dta)))

## same as above, but without filtering
print(importance(unsupv.varpro(dta, mode = "none")))


## ------------------------------------------------------------
## glass
## ------------------------------------------------------------

data(Glass, package = "mlbench")

## remove the outcome
Glass$Type <- NULL

## get importance - no filtering
o <- unsupv.varpro(Glass, mode = "none")
print(importance(o))

## compare to PCA
(biplot(prcomp(o$x, scale = TRUE)))


## ------------------------------------------------------------
## glioma data
## ------------------------------------------------------------

data(glioma, package = "varPro")

## remove the outcome
glioma$y <- NULL

## reduce number of lasso iterations
print(importance(unsupv.varpro(glioma, maxit = 1000)))

## ------------------------------------------------------------
## largish data set: illustrates various options to speed up calculations
## ------------------------------------------------------------

## first we roughly impute the data
data(housing, package = "randomForestSRC")
housing2 <- randomForestSRC:::get.na.roughfix(housing)

## use bigger nodesize
print(importance(unsupv.varpro(housing2, ntree = 50, nodesize = 150)))

## use smaller bootstrap
print(importance(unsupv.varpro(housing2, ntree = 50, nodesize = 150, sampsize = 250)))

## ------------------------------------------------------------
## survival data
## ------------------------------------------------------------

data(peakVO2, package = "randomForestSRC")

## remove the outcome
peakVO2$died <- peakVO2$ttodead <- NULL

## get importance
print(importance(unsupv.varpro(peakVO2)))

}}

\keyword{unsupv.varpro}
