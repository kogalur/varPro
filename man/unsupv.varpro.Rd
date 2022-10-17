\name{unsupv.varpro}
\alias{unsupv.varpro}
\title{Unsupervised Variable Selection using Variable Priority (VarPro)}
\description{
  Selects Variables in Unsupervised Problems using Variable Priority
  (VarPro).
}

\usage{
unsupv.varpro(data, nvar = 5, cutoff = NULL,
       method = c("sh1", "sh2"), cv = FALSE,
       ntree = 150, nodesize = NULL,
       max.rules.tree = 150, max.tree = min(150, ntree), 
       papply = mclapply, verbose = FALSE, seed = NULL,
       ...)
}

\arguments{

  \item{data}{Data frame containing the usupervised data.}

  \item{nvar}{This is the number of variables returned.}

  \item{cutoff}{Optional cut-off value for selecting variables.
  Over-rides \code{nvar} if specified.}

  \item{method}{Method used for creating aritifical two-class data.
    Choices are mode 1 ("sh1") or mode 2 ("sh2") of Shi-Horvath's (2006)
    artificial data generating procedure.  See details below.}

  \item{cv}{Instead of using a pre-set number of variables, use CV to
    determine the top variables (more accurate but slower)?}

  \item{ntree}{Number of trees to grow.}

  \item{nodesize}{Nodesize of trees.}

  \item{max.rules.tree}{Maximum number of rules per tree.}

  \item{max.tree}{Maximum number of trees used for extracting rules.}

  \item{papply}{Use mclapply or lapply.}

  \item{verbose}{Print verbose output?}

  \item{seed}{Seed for repeatability.}

  \item{...}{Further arguments to be passed to \command{varpro} or
    \command{cv.varpro}.}

}

\details{

  Breiman (2003; see also Shi-Horvath, 2006) transform the unsupervised
  learning problem into a two class supervised problem.  The first class
  consists of the original observations, while the second class is
  artificially created.  Breiman's method is generally used for
  unsupervised learning, however here we use the artificially created
  data for the purpose of unsupervised variable selection. Specifically,
  VarPro is applied by running a two-class classifier on the artificially
  created two class data.  The idea is that the classifier will identify
  variables that are informative for clusters.

  The result of applying VarPro is a ranked set of variables. The top
  \code{nvar} of these are returned (if \code{cutoff} is set then the
  top variables meeting this cutoff value is returned; see
  \command{importance} for details).  The option \code{cv} uses
  cross-validation to select variables (see \command{cv.varpro} for details).
  This is more accurate, but much slower.

  Artificial data is created using "mode 1" or "mode 2" of Shi-Horvath
  (2006).  Mode 1 (default) randomly draws from each set of observed
  features.  Mode 2 draws a uniform value from the minimum and maximum
  values of a feature.

}
  
\value{

  Vector containing names of variables found to be informative using VarPro.
 
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
  \command{\link{cv.varpro}}
  \command{\link{varpro}}
}

\examples{
## ------------------------------------------------------------
## boston housing
## ------------------------------------------------------------

data(BostonHousing, package = "mlbench")

## default call
print(unsupv.varpro(BostonHousing))

## use cv with fast option
print(unsupv.varpro(BostonHousing, cv = TRUE, fast = TRUE))

\donttest{

## ------------------------------------------------------------
## glioma data
## ------------------------------------------------------------

data(glioma, package = "varPro")

## remove the outcome
glioma$y <- NULL

## reduce number of lasso iterations
print(unsupv.varpro(glioma, nvar = 10, maxit = 1000))



## ------------------------------------------------------------
## largish data set: illustrates various options to speed up calculations
## ------------------------------------------------------------

## first we roughly impute the data
data(housing, package = "randomForestSRC")
housing2 <- randomForestSRC:::get.na.roughfix(housing)

## use bigger nodesize
print(unsupv.varpro(housing2, ntree = 50, nodesize = 150, nvar = 10))

## use smaller bootstrap
print(unsupv.varpro(housing2, ntree = 50, nodesize = 150, sampsize = 250, nvar = 10))

## ------------------------------------------------------------
## largish data set with CV option using fast=TRUE
## ------------------------------------------------------------

## roughly impute the data
data(housing, package = "randomForestSRC")
housing2 <- randomForestSRC:::get.na.roughfix(housing)

## cv with fast = TRUE
print(unsupv.varpro(housing2, ntree = 50, nodesize = 150, cv = TRUE, fast = TRUE))

## cv even faster - use sampsize option
print(unsupv.varpro(housing2, ntree = 50, nodesize = 150, cv = TRUE, fast = TRUE, sampsize = 250))


## ------------------------------------------------------------
## survival data
## ------------------------------------------------------------

data(peakVO2, package = "randomForestSRC")

print(unsupv.varpro(peakVO2, ntree = 100, nvar = 10))

}}

\keyword{unsupv.varpro}
