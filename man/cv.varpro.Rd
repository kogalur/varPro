\name{cv.varpro}
\alias{cv.varpro}
\title{Cross-Validated Cutoff Value for Variable Priority (VarPro)}
\description{
  Selects Cutoff Value for Variable Priority (VarPro).
}

\usage{
cv.varpro(f, data, ntree = 150,
       zcut = seq(0.1, 2, length = 50),
       nblocks = 10,
       split.weight = TRUE, nodesize = NULL, 
       max.rules.tree = 150, max.tree = min(150, ntree), 
       papply = mclapply, verbose = FALSE, seed = NULL,
       fast = FALSE, crps = FALSE, ...)
}

\arguments{

  \item{f}{Formula describing the model to be fit.}

  \item{data}{Training data set (a data frame).} 

  \item{ntree}{Number of trees to grow.}

  \item{zcut}{Grid of positive pre-specified cutoff values.}

  \item{nblocks}{This is like the number of folds used in M-fold
  validation.}
  
  \item{split.weight}{Use guided feature selection?  Features are
  selected with probability according to split weight values, the latter
  being acquired in a preliminary lasso+tree step.}

  \item{nodesize}{Nodesize of trees.}

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
  standard deviation of the optimal cutoff error rate.}

\value{

  Output containing importance values for the optimized cutoff value.
  A conservative and liberal list of variables is also returned.
  
}

\author{
  Min Lu and Hemant Ishwaran
}

\references{
  Lu M. and  Ishwaran H. (2022).  Variable Priority for Variable
  Selection in Rule-Based Models.
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
## iris
## ------------------------------------------------------------

print(cv.varpro(Species~., iris))

\donttest{

## ------------------------------------------------------------
## friedman 1
## ------------------------------------------------------------

print(cv.varpro(y~., data.frame(mlbench::mlbench.friedman1(1000))))

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
