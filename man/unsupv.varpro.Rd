\name{unsupv.varpro}
\alias{unsupv.varpro}
\alias{unsupv}
\alias{cv.matrix.performance}
\alias{cv.matrix}
\alias{cv.unsupv}
\alias{cv.unsupv.varpro}
\title{Unsupervised Variable Selection using Variable Priority (VarPro)}
\description{
  Selects Variables in Unsupervised Problems using Variable Priority
  (VarPro).
}

\usage{
unsupv.varpro(data,
       method = c("auto", "unsupv", "rnd"),
       ntree = 200, nodesize = NULL,
       max.rules.tree = 50, max.tree = 200,
       papply = mclapply, verbose = FALSE, seed = NULL,
       ...)
}

\arguments{

  \item{data}{Data frame containing the usupervised data.}

  \item{method}{Type of forest used.  Choices are "auto" (the default
   method called auto-encoder, a type of multivariate forest), "unsupv"
   (unsupervised analysis, the default), and "rnd" (pure random
   forests).}
 
  \item{ntree}{Number of trees to grow.}

  \item{nodesize}{Nodesize of trees. If not specified, value is set
  using an internal function optimized for sample size and dimension.}

  \item{max.rules.tree}{Maximum number of rules per tree.}

  \item{max.tree}{Maximum number of trees used for extracting rules.}

  \item{papply}{Use mclapply or lapply.}

  \item{verbose}{Print verbose output?}

  \item{seed}{Seed for repeatability.}

  \item{...}{Further arguments to be passed to \command{rfsrc}.}

}

\details{

  VarPro is applied to the branches of a random forest to obtain
  importance values.  Three types of forests can be used specified by
  the option \code{method}.  The default \code{method}="auto" runs a
  random forest autoencoder by fitting selected variables against
  themselves - a special type of multivariate forest.  The second is
  \code{method}="unsupv" which runs unsupervised forests.  The third is
  random forests using pure random splitting, \code{method}="rnd".  For
  very large data sets the auto-encoder might be slow.  The other two
  methods are much faster.

  VarPro importance is measured by entropy defined in terms of
  overall feature variance.
  
}
  
\value{

  A VarPro object.
 
}

\author{
  Min Lu and Hemant Ishwaran
}

\references{

  Lu M. and Ishwaran H. (2022).  Model-Independent Variable Selection
  via the Rule-based Variable Priority (VarPro).


}

\seealso{
  \command{\link{predict.unsupv}}
  \command{\link{varpro}}
}

\examples{
## ------------------------------------------------------------
## boston housing: default call
## ------------------------------------------------------------

data(BostonHousing, package = "mlbench")

## default call
o <- unsupv(BostonHousing)
print(importance(o))

## ------------------------------------------------------------
## boston housing: using method="unsupv"
## ------------------------------------------------------------

data(BostonHousing, package = "mlbench")

## unsupervised splitting 
o <- unsupv(BostonHousing, method = "unsupv")
print(importance(o))

\donttest{

## ------------------------------------------------------------
## boston housing: illustrates hot-encoding
## ------------------------------------------------------------

## load the data
data(BostonHousing, package = "mlbench")

## convert some of the features to factors
Boston <- BostonHousing
Boston$zn <- factor(Boston$zn)
Boston$chas <- factor(Boston$chas)
Boston$lstat <- factor(round(0.2 * Boston$lstat))
Boston$nox <- factor(round(20 * Boston$nox))
Boston$rm <- factor(round(Boston$rm))

## call unsupervised varpro and print importance
print(importance(o <- unsupv(Boston)))

## get top variables
get.topvars(o)

## map importance values back to original features
print(get.orgvimp(o))

## same as above ... but for all variables
print(get.orgvimp(o, pretty = FALSE))


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
print(importance(unsupv(dta)))


## ------------------------------------------------------------
## glass (remove outcome)
## ------------------------------------------------------------

data(Glass, package = "mlbench")

## remove the outcome
Glass$Type <- NULL

## get importance
o <- unsupv(Glass)
print(importance(o))

## compare to PCA
(biplot(prcomp(o$x, scale = TRUE)))

## ------------------------------------------------------------
## largish data set: illustrates various options to speed up calculations
## ------------------------------------------------------------

## first we roughly impute the data
data(housing, package = "randomForestSRC")

## to speed up analysis, convert all factors to real values
housing2 <- randomForestSRC:::get.na.roughfix(housing)
housing2 <- data.frame(data.matrix(housing2))

## use fewer trees and bigger nodesize
print(importance(unsupv(housing2, ntree = 50, nodesize = 150)))

}}

\keyword{unsupv.varpro}
