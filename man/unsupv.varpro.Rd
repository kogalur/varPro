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

  \item{data}{Data frame containing the unsupervised data.}

  \item{method}{Type of forest used. Options are \code{"auto"} (auto-encoder), \code{"unsupv"} (unsupervised analysis), and \code{"rnd"} (pure random forest).}

  \item{ntree}{Number of trees to grow.}

  \item{nodesize}{Minimum terminal node size. If not specified, an internal function selects an appropriate value based on sample size and dimension.}

  \item{max.rules.tree}{Maximum number of rules per tree.}

  \item{max.tree}{Maximum number of trees used to extract rules.}

  \item{papply}{Parallel apply method; typically \code{mclapply} or \code{lapply}.}

  \item{verbose}{Print verbose output?}

  \item{seed}{Seed for reproducibility.}

  \item{...}{Additional arguments passed to \code{rfsrc}.}

}

\details{

VarPro is applied to the branches of a random forest to compute variable importance values. The type of forest used is controlled by the \code{method} option.

The default, \code{method = "auto"}, fits a random forest autoencoder by
regressing selected variables against themselves, a specialized form of
multivariate forest. The alternative \code{"unsupv"} uses unsupervised
forests, and \code{"rnd"} employs pure random splitting. For large
datasets, the autoencoder may be slower, while the other two methods are
typically faster.

VarPro importance is quantified using an entropy-based measure defined in terms of overall feature variance.

}

\value{

  A VarPro object.
 
}

\author{
  Min Lu and Hemant Ishwaran
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
