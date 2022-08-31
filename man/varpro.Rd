\name{varpro}
\alias{varpro}
\title{Variable Priority (VarPro) for Variable Selection in Rule-Based Models}
\description{
  Variable Priority (VarPro) for Variable Selection in Rule-Based Models
  for Regression, Classification and Survival.
}

\usage{
varpro(f, data, ntree = 500, split.weight = TRUE,
       nodesize = if (split.weight) 5 else 10,
       max.rules.tree = 150, max.tree = min(150, ntree), 
       papply = mclapply, verbose = FALSE, seed = NULL, ...)
}

\arguments{

  \item{f}{Formula describing the model to be fit.}

  \item{data}{Training data set (a data frame).} 

  \item{ntree}{Number of trees to grow.}
  
  \item{split.weight}{Use guided feature selection?  Features are
  selected with probability according to split weight values, the latter
  being acquired in a preliminary lasso+tree step.}

  \item{nodesize}{Nodesize of trees.}


  \item{max.rules.tree}{Maximum number of rules per tree.}

  \item{max.tree}{Maximum number of trees used for extracting rules.}

  \item{papply}{Use mclapply or lapply.}

  \item{verbose}{Print verbose output?}

  \item{seed}{Seed for repeatability.}
  
  \item{...}{Further arguments (for experts only).}

}

\details{

  Rule-based models, such as simple decision rules, rule learning,
  trees, boosted trees, Bayesian additive regression trees, Bayesian
  forests and random forests, are widely used for variable
  selection. These nonparametric models do not require model
  specification and accommodate various outcomes including regression,
  classification, survival and longitudinal analyses. Although
  permutation variable importance (VIMP) and knockoffs have been
  extensively studied, their empirical results can be compromised in
  certain scenarios because both VIMP and knockoffs depend on the
  quality of the generated artificial covari- ates for their success. To
  address this practical problem, we propose a new framework of variable
  priority (VarPro), which instead of generating artificial covariates,
  creates release rules to examine the affect on the response for each
  covariate. Instead of new data being created, neighborhoods of the
  existing data are used for estimating the importance of a
  variable. Similar to VIMP and knockoffs, VarPro allows the conditional
  distribution of the response variable to be arbitrary and unknown.

  The VarPro method implemented here is described as follows.  A
  collection of \code{ntree} trees are grown with guided feature
  selection where variables are selected according to a split-weight
  obtained in a pre-processing step using lasso+trees.  A random subset
  of \code{max.tree} trees are selected from the \code{ntree} trees and
  a random subset of branches, \code{max.rules.tree}, are chosen from
  each tree.  These rules are used for forming the VarPro estimator of
  importance.  Applies to regression, multiclass and survival data.  

  Regarding the guided feature selection: this is generally a very
  useful techinque as it supervises the rules obtained from the trees.
  For this reason it is recommended to keep the \code{split.weight}
  option on and this is especially true for high-dimensional, or even
  moderately high-dimensional, problems.  If this option is turned off,
  then \code{nodesize} should be increased to improve precision of the
  estimators.  See the example below.
  
}

\value{

  Output containing VarPro estimators used to calculate importance. See
  \code{importance.varpro}.
  
}

\author{
  Min Lu and Hemant Ishwaran
}

\references{
  Lu M. and  Ishwaran H. (2022).  Variable Priority for Variable
  Selection in Rule-Based Models.
}

\seealso{
  \command{\link{glioma}}
  \command{\link{importance.varpro}}
}

\examples{

## ------------------------------------------------------------
## classification example: iris 
## ------------------------------------------------------------

## apply varpro to the iris data
o <- varpro(Species ~ ., iris)

## call the importance function and print the results
print(importance(o))

## ------------------------------------------------------------
## regression example: boston housing
## ------------------------------------------------------------

## load the data
data(BostonHousing, package = "mlbench")

## call varpro
o <- varpro(medv~., BostonHousing)

## extract importance values
imp <- importance(o)

## print importance values
print(imp)

## plot importance values
importance(o, plot.it = TRUE)

## ------------------------------------------------------------
## regression example: friedman 1
## ------------------------------------------------------------

o <- varpro(y~., data.frame(mlbench::mlbench.friedman1(1000)))
print(importance(o))


## ------------------------------------------------------------
## regression example: all noise
## ------------------------------------------------------------

x <- matrix(rnorm(100 * 50), 100, 50)
y <- rnorm(100)
o <- varpro(y~., data.frame(y = y, x = x))
print(importance(o))


## ------------------------------------------------------------
## example without guided feature selection
## (i.e. split.weight=FALSE and rules are not supervised)
## ------------------------------------------------------------

o <- varpro(y~., data.frame(mlbench::mlbench.friedman2(1000)),
            nodesize = 10, split.weight = FALSE)
print(importance(o))

\donttest{

## ------------------------------------------------------------
## survival example: pbc 
## ------------------------------------------------------------
data(pbc, package = "randomForestSRC")
o <- varpro(Surv(days, status)~., pbc)
print(importance(o))

## ------------------------------------------------------------
## pbc survival with rmst (restrcited mean survival time)
## functional of interest is RMST at 500 days
## ------------------------------------------------------------
data(pbc, package = "randomForestSRC")
o <- varpro(Surv(days, status)~., pbc, rmst = 500)
print(importance(o))

## ------------------------------------------------------------
## pbc survival with rmst vector
## variable importance is a list for each rmst value
## ------------------------------------------------------------
data(pbc, package = "randomForestSRC")
o <- varpro(Surv(days, status)~., pbc, rmst = c(500, 1000))
print(importance(o))


## ------------------------------------------------------------
## survival example with more variables
## ------------------------------------------------------------
data(peakVO2, package = "randomForestSRC")
o <- varpro(Surv(ttodead, died)~., peakVO2)
imp <- importance(o, plot.it = TRUE)
print(imp)

## ------------------------------------------------------------
## largish data: iowa housing data
## use bigger nodesize to speed calculations
## ------------------------------------------------------------

## use rapid, fast impute
data(housing, package = "randomForestSRC")
housing2 <- randomForestSRC:::get.na.roughfix(housing)

## varpro call
o <- varpro(SalePrice~., housing2, ntree = 100, nodesize = 50)
print(importance(o))

}
}
\keyword{varpro}
