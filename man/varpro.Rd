\name{varpro}
\alias{varpro}
\alias{make.vt}
\title{Model-Independent Variable Selection via the Rule-based Variable Priority (VarPro)}
\description{
Model-Independent Variable Selection via the Rule-based Variable Priority (VarPro)
for Regression, Classification and Survival.
}

\usage{
varpro(f, data, nvar = 30, ntree = 500, y = NULL,
       split.weight = TRUE, split.weight.method = NULL, sparse = TRUE,
       nodesize = NULL, max.rules.tree = 150, max.tree = min(150, ntree),
       parallel = TRUE, cores = get.number.cores(),
       papply = mclapply, verbose = FALSE, seed = NULL, ...)
}

\arguments{

  \item{f}{Formula describing the model to be fit.}

  \item{data}{Training data set (a data frame).}

  \item{nvar}{Upper limit to number of variables returned.}  

  \item{ntree}{Number of trees to grow.}

  \item{y}{Users can replace the original response value with their own
  custom designed response: for example this can be the predicted value
  from a machine learning method.  Used to study which variables are
  important in the user's predictor.  The supplied \code{y} must match
  the dimension of the data.} 
  
  \item{split.weight}{Use guided tree-splitting?  Features selected to
  split a tree node are selected with probability according to a
  split-weight value, the latter by default being acquired in a
  preliminary lasso+tree step.}

  \item{split.weight.method}{Optional character string (or vector)
  specifying the method used to generate the split-weights.  If not
  specified, defaults to a lasso+tree step.  See details below.}

  \item{sparse}{Use sparse split-weights?}

  \item{nodesize}{Nodesize of trees. If not specified, value is set
  using an internal function optimized for sample size and dimension.}

  \item{max.rules.tree}{Maximum number of rules per tree.}

  \item{max.tree}{Maximum number of trees used for extracting rules.}

  \item{parallel}{If \code{TRUE}, use parallel foreach to fit each lasso
  fold.  This registers parallel before hand using \command{doMC}.}

   \item{cores}{Sets the number of cores for parallel processing when
     using the lasso.  By default uses the parallel command
     \command{detectCores}.}

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
  quality of the generated artificial covariates for their success. To
  address this practical problem, we propose a new framework of variable
  priority (VarPro), which instead of generating artificial covariates,
  creates release rules to examine the affect on the response for each
  covariate. Instead of new data being created, neighborhoods of the
  existing data are used for estimating the importance of a
  variable. Similar to VIMP and knockoffs, VarPro allows the conditional
  distribution of the response variable to be arbitrary and unknown.

  The VarPro algorithm is described as follows.  A collection of
  \code{ntree} trees are grown with guided tree-splitting where
  variables selected to split a node are chosen with probability
  according to a split-weight obtained in a pre-processing step.  A
  random subset of \code{max.tree} trees are selected from the
  \code{ntree} trees and a random subset of branches,
  \code{max.rules.tree}, are chosen from each tree.  These rules are
  used for forming the VarPro estimator of importance.  Applies to
  regression, multiclass and survival data.

  Guided tree-splitting encourages the rules harvested from a tree to
  favor potentially strong features.  Therefore option
  \code{split.weight} should generally be left on (this is especially
  true for high-dimensional problems).  If the option is turned off,
  consider increasing \code{nodesize} to improve precision of the
  estimators.

  By default, split-weights are obtained by using a combination of lasso
  and trees.  The split-weight of a variable is defined as the
  absolute value of the standardized beta coefficient from fitting a
  lasso combined with the split frequency of the variable from a forest
  of shallow trees.  If the sample size is moderate and dimension is
  moderate then this latter step is replaced with the absolute value of
  the permutation importance of the variable.  Note that in order to use
  lasso for guided feature selection, factors are converted using
  one-hot-encoding.  Therefore all variables are converted to real
  values.

  For greater customization of how split-weights are determined, use
  option \command{split.weight.method} which is set by choosing any
  values from the strings "lasso", "tree", "vimp".  Thus "lasso" uses
  lasso only for determining the split-weight, whereas "lasso tree" or
  "lasso vimp" uses lasso in combination with shallow trees or lasso in
  combination with permutation importance.  See the examples below.
  
  Selection of variables is based on a preset cutoff value set using the
  \code{importance} function.  There is a cross-validation (CV) method
  to automate this.  See \command{cv.varpro}.
  
  Run times can be improved by using smaller values of \code{ntree} and
  larger values of \code{nodesize}.  Other options are also available.
  See the examples below.

  For class imbalanced settings (two class problems where relative
  frequency of labels is skewed towards one class) the code
  automatically switches to random forest quantile classification (RFQ;
  see O'Brien and Ishwaran, 2019) under the gmean (geometric mean)
  performance metric.  This can be over-ridden by the hidden option
  \code{use.rfq}.
  
  
  
}

\value{

  Output containing VarPro estimators used to calculate importance. See
  \command{importance.varpro}.  Also see \command{cv.varpro} for
  automated variable selection.
  
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
  \command{\link{glioma}}
  \command{\link{cv.varpro}}  
  \command{\link{importance.varpro}}
  \command{\link{predict.varpro}}  
  \command{\link{isopro.varpro}}
  \command{\link{unsupv.varpro}}
  
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

## extract and print importance values
imp <- importance(o)
print(imp)

## another way to extract and print importance values
print(get.vimp(o))
print(get.vimp(o, pretty = FALSE))

## plot importance values
importance(o, plot.it = TRUE)


## ------------------------------------------------------------
## regression example: boston housing illustrating hot-encoding
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

## call varpro and print the importance
print(importance(o <- varpro(medv~., Boston)))

## get top variables
get.topvars(o)

## map importance values back to original features
print(get.orgvimp(o))

## same as above ... but for all variables
print(get.orgvimp(o, pretty = FALSE))

## ------------------------------------------------------------
## regression example: friedman 1
## ------------------------------------------------------------

o <- varpro(y~., data.frame(mlbench::mlbench.friedman1(1000)))
print(importance(o))

## ------------------------------------------------------------
## example without guided tree-splitting
## ------------------------------------------------------------

o <- varpro(y~., data.frame(mlbench::mlbench.friedman2(1000)),
            nodesize = 10, split.weight = FALSE)
print(importance(o))

## ------------------------------------------------------------
## regression example: all noise
## ------------------------------------------------------------

x <- matrix(rnorm(100 * 50), 100, 50)
y <- rnorm(100)
o <- varpro(y~., data.frame(y = y, x = x))
print(importance(o))

\donttest{
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

  ## varpro call
  print(importance(varpro(f, d)))

}

## ------------------------------------------------------------
## survival example: pbc 
## ------------------------------------------------------------
data(pbc, package = "randomForestSRC")
o <- varpro(Surv(days, status)~., pbc)
print(importance(o))

## ------------------------------------------------------------
## pbc survival with rmst (restricted mean survival time)
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
## high dimensional survival example
## ------------------------------------------------------------
data(vdv, package = "randomForestSRC")
o <- varpro(Surv(Time, Censoring)~., vdv)
print(importance(o))

## ------------------------------------------------------------
## high dimensional survival example without sparse option
## ------------------------------------------------------------
data(vdv, package = "randomForestSRC")
o <- varpro(Surv(Time, Censoring)~., vdv, sparse = FALSE)
print(importance(o))

## ----------------------------------------------------------------------
## high dimensional survival example using different split-weight methods
## ----------------------------------------------------------------------
data(vdv, package = "randomForestSRC")
f <- as.formula(Surv(Time, Censoring)~.)

## lasso only
print(importance(varpro(f, vdv, split.weight.method = "lasso")))

## lasso and vimp
print(importance(varpro(f, vdv, split.weight.method = "lasso vimp")))

## lasso, vimp and shallow trees
print(importance(varpro(f, vdv, split.weight.method = "lasso vimp tree")))

## ------------------------------------------------------------
## largish data (iowa housing data)
## to speed up calculations convert data to all real
## ------------------------------------------------------------

## first we roughly impute the data
data(housing, package = "randomForestSRC")
dta <- randomForestSRC:::get.na.roughfix(housing)
dta <- data.frame(data.matrix(dta))

## varpro call
o <- varpro(SalePrice~., dta)
print(importance(o))

## ------------------------------------------------------------
## large data: illustrates different ways to improve speed
## ------------------------------------------------------------

n <- 25000
p <- 50
d <- data.frame(y = rnorm(n), x = matrix(rnorm(n * p), n))

## use large nodesize
print(system.time(o <- varpro(y~., d, ntree = 100, nodesize = 200)))
print(importance(o))

## use large nodesize, smaller bootstrap 
print(system.time(o <- varpro(y~., d, ntree = 100, nodesize = 200,
           sampsize = 100)))
print(importance(o))


## ------------------------------------------------------------
## custom split-weights (hidden option)
## ------------------------------------------------------------

## load the data
data(BostonHousing, package = "mlbench")

## make some features into factors
Boston <- BostonHousing
Boston$zn <- factor(Boston$zn)
Boston$chas <- factor(Boston$chas)
Boston$lstat <- factor(round(0.2 * Boston$lstat))
Boston$nox <- factor(round(20 * Boston$nox))
Boston$rm <- factor(round(Boston$rm))

## get default custom split-weights: a named real vector
swt <- varPro:::get.splitweight.custom(medv~.,Boston)

## define custom splits weight
swt <- swt[grepl("crim", names(swt)) |
           grepl("zn", names(swt)) |
           grepl("nox", names(swt)) |
           grepl("rm", names(swt)) |
           grepl("lstat", names(swt))]
           
swt[grepl("nox", names(swt))] <- 4
swt[grepl("lstat", names(swt))] <- 4

swt <- c(swt, strange=99999)

cat("custom split-weight\n")
print(swt)
  
## call varpro with the custom split-weights
o <- varpro(medv~.,Boston,split.weight.custom=swt,verbose=T,sparse=F)
cat("varpro result\n")
print(importance(o))
print(get.vimp(o, pretty=FALSE))
print(get.orgvimp(o, pretty=FALSE))


## ------------------------------------------------------------
## user supplied y 
## ------------------------------------------------------------

## load the data
data(BostonHousing, package = "mlbench")

## train your ML method here - we use misspecified linear model
y <- lm(medv~lstat+b, BostonHousing)$fitted

## varpro with user specified y
o <- varpro(medv~., BostonHousing, y=y)
print(importance(o))



}
}
\keyword{varpro}
