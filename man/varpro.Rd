\name{varpro}
\alias{varpro}
\title{Model-Independent Variable Selection via the Rule-based Variable Priority (VarPro)}

\description{
Model-Independent Variable Selection via the Rule-based Variable Priority (VarPro)
for Regression, Multivariate Regression, Classification and Survival.
}

\usage{
varpro(f, data, nvar = 30, ntree = 500, 
       split.weight = TRUE, split.weight.method = NULL, sparse = TRUE,
       nodesize = NULL, max.rules.tree = 150, max.tree = min(150, ntree),
       parallel = TRUE, cores = get.number.cores(),
       papply = mclapply, verbose = FALSE, seed = NULL, ...)
}

\arguments{

  \item{f}{Formula specifying the model to be fit.}

  \item{data}{Data frame containing the training data.}

  \item{nvar}{Maximum number of variables to return.}

  \item{ntree}{Number of trees to grow.}

  \item{split.weight}{Use guided tree-splitting? Candidate variables for splitting are selected with probability proportional to a split-weight, obtained by default from a preliminary lasso+tree step.}

  \item{split.weight.method}{Character string (or vector) specifying method used to generate split-weights. Defaults to lasso+tree. See \code{Details}.}

  \item{sparse}{Use sparse split-weights?}

  \item{nodesize}{Minimum terminal node size. If not specified, value is set internally based on sample size and dimension.}

  \item{max.rules.tree}{Maximum number of rules per tree.}

  \item{max.tree}{Maximum number of trees used to extract rules.}

  \item{parallel}{Use parallel execution for lasso folds using \pkg{doMC}.}

  \item{cores}{Number of cores for parallel processing. Defaults to \code{parallel::detectCores()}.}

  \item{papply}{Apply method for parallel execution; typically \code{mclapply} or \code{lapply}.}

  \item{verbose}{Print verbose output?}

  \item{seed}{Seed for reproducibility.}

  \item{...}{Additional arguments for advanced use.}

}

\details{

Rule-based models, such as decision rules, rule learning, trees, boosted
trees, Bayesian additive regression trees, Bayesian forests, and random
forests, are widely used for variable selection. These nonparametric
methods require no model specification and accommodate various outcomes
including regression, classification, survival, and longitudinal data.

Although permutation variable importance (VIMP) and knockoff methods have been extensively studied, their effectiveness can be limited in practice. Both approaches rely on the quality of artificially generated covariates, which may not perform well in complex or high-dimensional settings.

To address these limitations, we introduce a new framework called variable priority (VarPro). Instead of generating synthetic covariates, VarPro constructs *release rules* to assess the impact of each covariate on the response. Neighborhoods of existing data are used for estimation, avoiding the need for artificial data generation. Like VIMP and knockoffs, VarPro imposes no assumptions on the conditional distribution of the response.

The VarPro algorithm proceeds as follows: 
A forest of \code{ntree} trees is grown using guided tree-splitting, where candidate variables for node splitting are selected with probability proportional to their split-weights. These split-weights are computed in a preprocessing step. A subset of \code{max.tree} trees is randomly selected from the forest, and \code{max.rules.tree} branches are sampled from each selected tree. The resulting rules form the basis of the VarPro importance estimator. The method supports regression, multivariate regression, multiclass classification, and survival outcomes.

Guided tree-splitting encourages rule construction to favor influential features. Thus, \code{split.weight} should generally remain \code{TRUE}, especially for high-dimensional problems. If disabled, it is recommended to increase \code{nodesize} to improve estimator precision.

By default, split-weights are computed via a lasso-plus-tree
strategy. Specifically, the split-weight of a variable is defined as the
product of the absolute standardized lasso coefficient and the
variable's split frequency from a forest of shallow trees. If sample
size and dimension are both moderate, this may be replaced by the
variable's absolute permutation importance. Note: variables are one-hot
encoded for use in lasso, and all inputs are converted to numeric
values.

To customize split-weight construction, use the \code{split.weight.method} argument with one or more of the following strings: \code{"lasso"}, \code{"tree"}, or \code{"vimp"}. For example, \code{"lasso"} uses only lasso coefficients; \code{"lasso tree"} combines lasso and shallow trees; \code{"lasso vimp"} combines lasso with permutation importance. See examples below.

Variables are ranked by importance, with higher values indicating greater influence. Cross-validation can be used to determine a cutoff threshold. See \code{cv.varpro} for details.

Run time can be reduced by using smaller values of \code{ntree} or larger values of \code{nodesize}. Additional runtime tuning options are discussed in the examples.

In class-imbalanced two-class settings, the algorithm automatically switches to random forest quantile classification (RFQ; see O'Brien and Ishwaran, 2019) using the geometric mean (gmean) metric. This behavior can be overridden via the hidden option \code{use.rfq}.

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
  Lu, M. and Ishwaran, H. (2024). Model-independent variable selection
  via the rule-based variable priority. arXiv e-prints, pp.arXiv-2409.

  O'Brien R. and Ishwaran H. (2019).  A random forests quantile
  classifier for class imbalanced data. \emph{Pattern Recognition},
  90, 232-249.
}

\seealso{
  \command{\link{alzheimers}}
  \command{\link{cv.varpro}}
  \command{\link{glioma}}
  \command{\link{importance.varpro}}
  \command{\link{predict.varpro}}  
  \command{\link{isopro}}
  \command{\link{uvarpro}}
  
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


\donttest{
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

## ------------------------------------------------------------
## multivariate regression example: boston housing
## ------------------------------------------------------------

data(BostonHousing, package = "mlbench")

## using rfsrc multivariate formula call
importance(varpro(Multivar(lstat, nox) ~., BostonHousing))

## using cbind multivariate formula call
importance(varpro(cbind(lstat, nox) ~., BostonHousing))

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
o <- varpro(medv~.,Boston,split.weight.custom=swt,verbose=TRUE,sparse=FALSE)
cat("varpro result\n")
print(importance(o))
print(get.vimp(o, pretty=FALSE))
print(get.orgvimp(o, pretty=FALSE))



}
}
\keyword{varpro}
