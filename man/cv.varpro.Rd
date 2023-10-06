\name{cv.varpro}
\alias{cv.varpro}
\title{Cross-Validated Cutoff Value for Variable Priority (VarPro)}
\description{
  Selects Cutoff Value for Variable Priority (VarPro).
}

\usage{
cv.varpro(f, data, nvar = 30,
       ntree = 150,
       zcut = seq(0.1, 2, length = 50),
       nblocks = 10,
       split.weight = TRUE, sparse = TRUE, nodesize = NULL, 
       max.rules.tree = 150, max.tree = min(150, ntree), 
       papply = mclapply, verbose = FALSE, seed = NULL,
       fast = FALSE, crps = FALSE, ...)
}

\arguments{

  \item{f}{Formula describing the model to be fit.}

  \item{data}{Training data set (a data frame).}

  \item{nvar}{Upper limit to number of variables returned.}  

  \item{ntree}{Number of trees to grow.}

  \item{zcut}{Grid of positive pre-specified cutoff values.}

  \item{nblocks}{This is like the number of folds used in M-fold
  validation.}
  
  \item{split.weight}{Use guided feature selection?  Features are
  selected with probability according to split weight values, the latter
  being acquired in a preliminary lasso+tree step.}

  \item{sparse}{Use sparse guided feature selection?}

  \item{nodesize}{Nodesize of trees. If not specified, value is set
  using an internal function optimized for sample size and dimension.}

  \item{max.rules.tree}{Maximum number of rules per tree.}

  \item{max.tree}{Maximum number of trees used for extracting rules.}

  \item{papply}{Use mclapply or lapply.}

  \item{verbose}{Print verbose output?}

  \item{seed}{Seed for repeatability.}

  \item{fast}{Use fast random forests, \command{rfsrc.fast}, in place of \command{rfsrc}?
          Improves speed but may be less accurate.}
	  
  \item{crps}{Use CRPS (continuous rank probability score) for
     estimating survival performance (default is to use Harrel's C-index)?
     Only applies to survival families.}

  \item{...}{Further arguments to be passed to \command{varpro}.}

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
  Lu M. and Ishwaran H. (2022).  Model-Independent Variable Selection
  via the Rule-based Variable Priority (VarPro).


  O'Brien R. and Ishwaran H. (2019).  A random forests quantile
  classifier for class imbalanced data. \emph{Pattern Recognition},
  90, 232-249.
}

\seealso{
  \command{\link{importance.varpro}}
  \command{\link{unsupv.varpro}}
  \command{\link{varpro}}
}

\examples{
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
## iris
## ------------------------------------------------------------

print(cv.varpro(Species~., iris))

\donttest{
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
