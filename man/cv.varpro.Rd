\name{cv.varpro}
\alias{cv.varpro}
\title{Cross-Validated Cutoff Value for Variable Priority (VarPro)}
\description{
  Selects Cutoff Value for Variable Priority (VarPro).
}

\usage{
cv.varpro(f, data, nvar = 30, ntree = 150,
       local.std = TRUE, zcut = seq(0.1, 2, length = 50), nblocks = 10,
       split.weight = TRUE, split.weight.method = NULL, sparse = TRUE,
       nodesize = NULL, max.rules.tree = 150, max.tree = min(150, ntree), 
       papply = mclapply, verbose = FALSE, seed = NULL,
       fast = FALSE, crps = FALSE, ...)
}

\arguments{

  \item{f}{Model formula specifying the outcome and predictors.}

  \item{data}{Training data set (data frame).}

  \item{nvar}{Maximum number of variables to return.}

  \item{ntree}{Number of trees to grow.}

  \item{local.std}{Use locally standardized importance values?}

  \item{zcut}{Grid of positive cutoff values used for selecting top variables.}

  \item{nblocks}{Number of blocks (folds) for cross-validation.}

  \item{split.weight}{Use guided tree-splitting? Variables are selected for splitting with probability proportional to split-weights, obtained by default from a preliminary lasso+tree step.}

  \item{split.weight.method}{Character string or vector specifying how split-weights are generated. Defaults to lasso+tree.}

  \item{sparse}{Use sparse split-weights?}

  \item{nodesize}{Minimum terminal node size. If not specified, an internal function sets the value based on sample size and data dimension.}

  \item{max.rules.tree}{Maximum number of rules per tree.}

  \item{max.tree}{Maximum number of trees used for rule extraction.}

  \item{papply}{Apply method; either \code{mclapply} or \code{lapply}.}

  \item{verbose}{Print verbose output?}

  \item{seed}{Seed for reproducibility.}

  \item{fast}{Use \code{rfsrc.fast} in place of \code{rfsrc}? May improve speed at the cost of accuracy.}

  \item{crps}{Use CRPS (continuous ranked probability score) instead of Harrell's C-index for evaluating survival performance? Applies only to survival families.}

  \item{...}{Additional arguments passed to \code{varpro}.}

}


\details{

  Applies VarPro and then selects from a grid of cutoff values the
  cutoff value for identifying variables that minimizes out-of-sample
  performance (error rate) of a random forest where the forest is fit to
  the top variables identified by the given cutoff value.

  Additionally, a "conservative" and "liberal" list of variables are
  returned using a one standard deviation rule.  The conservative list
  comprises variables using the largest cutoff with error rate within
  one standard deviation from the optimal cutoff error rate, whereas the
  liberal list uses the smallest cutoff value with error rate within one
  standard deviation of the optimal cutoff error rate.

  For class imbalanced settings (two class problems where relative
  frequency of labels is skewed towards one class) the code
  automatically switches to random forest quantile classification (RFQ;
  see O'Brien and Ishwaran, 2019) under the gmean (geometric mean)
  performance metric.


}

\value{

  Output containing importance values for the optimized cutoff value.
  A conservative and liberal list of variables is also returned.

  Note that importance values are returned in terms of the original features
  and not their hot-encodings.  For importance in terms of
  hot-encodings, use the built-in wrapper \command{get.vimp} (see
  example below).

}

\author{
  Min Lu and Hemant Ishwaran
}

\references{

  Lu, M. and Ishwaran, H. (2024). Model-independent variable selection
  via the rule-based variable priority. arXiv e-prints, pp.arXiv-2409.


  O'Brien R. and Ishwaran H. (2019).  A random forests quantile
  classifier for class imbalanced data. \emph{Pattern Recognition},
  90, 232-249.
}

\seealso{
  \command{\link{importance.varpro}}
  \command{\link{uvarpro}}
  \command{\link{varpro}}
}

\examples{
\donttest{
## ------------------------------------------------------------
## van de Vijver microarray breast cancer survival data
## high dimensional example
## ------------------------------------------------------------
     
data(vdv, package = "randomForestSRC")
o <- cv.varpro(Surv(Time, Censoring) ~ ., vdv)
print(o)

## ------------------------------------------------------------
## boston housing
## ------------------------------------------------------------

data(BostonHousing, package = "mlbench")
print(cv.varpro(medv~., BostonHousing))

## ------------------------------------------------------------
## boston housing - original/hot-encoded vimp
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

## make cv call
o <-cv.varpro(medv~., Boston)
print(o)

## importance original variables (default)
print(get.orgvimp(o, pretty = FALSE))

## importance for hot-encoded variables
print(get.vimp(o, pretty = FALSE))

## ------------------------------------------------------------
## multivariate regression example: boston housing
## vimp is collapsed across the outcomes
## ------------------------------------------------------------

data(BostonHousing, package = "mlbench")
print(cv.varpro(cbind(lstat, nox) ~., BostonHousing))

## ------------------------------------------------------------
## iris
## ------------------------------------------------------------

print(cv.varpro(Species~., iris))

## ------------------------------------------------------------
## friedman 1
## ------------------------------------------------------------

print(cv.varpro(y~., data.frame(mlbench::mlbench.friedman1(1000))))

##----------------------------------------------------------------
##  class imbalanced problem 
## 
## - simulation example using the caret R-package
## - creates imbalanced data by randomly sampling the class 1 values
## 
##----------------------------------------------------------------

if (library("caret", logical.return = TRUE)) {

  ## experimental settings
  n <- 5000
  q <- 20
  ir <- 6
  f <- as.formula(Class ~ .)
 
  ## simulate the data, create minority class data
  d <- twoClassSim(n, linearVars = 15, noiseVars = q)
  d$Class <- factor(as.numeric(d$Class) - 1)
  idx.0 <- which(d$Class == 0)
  idx.1 <- sample(which(d$Class == 1), sum(d$Class == 1) / ir , replace = FALSE)
  d <- d[c(idx.0,idx.1),, drop = FALSE]
  d <- d[sample(1:nrow(d)), ]

  ## cv.varpro call
  print(cv.varpro(f, d))

}


## ------------------------------------------------------------
## pbc survival with rmst vector
## note that vimp is collapsed across the rmst values
## similar to mv-regression
## ------------------------------------------------------------

data(pbc, package = "randomForestSRC")
print(cv.varpro(Surv(days, status)~., pbc, rmst = c(500, 1000)))


## ------------------------------------------------------------
## peak VO2 with cutoff selected using fast option
## (a) C-index (default) (b) CRPS performance metric
## ------------------------------------------------------------

data(peakVO2, package = "randomForestSRC")
f <- as.formula(Surv(ttodead, died)~.)

## Harrel's C-index (default)
print(cv.varpro(f, peakVO2, ntree = 100, fast = TRUE))

## Harrel's C-index with smaller bootstrap
print(cv.varpro(f, peakVO2, ntree = 100, fast = TRUE, sampsize = 100))

## CRPS with smaller bootstrap
print(cv.varpro(f, peakVO2, crps = TRUE, ntree = 100, fast = TRUE, sampsize = 100))

## ------------------------------------------------------------
## largish data set: illustrates various options to speed up calculations
## ------------------------------------------------------------

## roughly impute the data
data(housing, package = "randomForestSRC")
housing2 <- randomForestSRC:::get.na.roughfix(housing)

## use bigger nodesize
print(cv.varpro(SalePrice~., housing2, fast = TRUE, ntree = 50, nodesize = 150))

## use smaller bootstrap
print(cv.varpro(SalePrice~., housing2, fast = TRUE, ntree = 50, nodesize = 150, sampsize = 250))

}
}
\keyword{cv.varpro}
