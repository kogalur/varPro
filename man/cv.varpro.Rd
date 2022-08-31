\name{cv.varpro}
\alias{cv.varpro}
\title{Cross-Validated Cutoff Value for Variable Priority (VarPro)}
\description{
  Selects Cutoff Value for Variable Priority (VarPro).
}

\usage{
cv.varpro(f, data, ntree = 300,
       zcut = seq(0.1, 2, length = 50),
       split.weight = TRUE,
       nodesize = 10, max.rules.tree = 150, max.tree = min(150, ntree), 
       papply = mclapply, verbose = FALSE, seed = NULL, ...)
}

\arguments{

  \item{f}{Formula describing the model to be fit.}

  \item{data}{Training data set (a data frame).} 

  \item{ntree}{Number of trees to grow.}

  \item{zcut}{Grid of positive pre-specified cutoff values.}
  
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

  Applies VarPro and then selects the cutoff value for identifying
  variable importance by chosing from a grid of cutoff values the cutoff
  value that minimizes out-of-sample performance of a random forest
  where the forest is fit to the top variables identified by a given
  cutoff value.
  
}

\value{

  Output containing importance values for the optimized cutoff value.
  
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
  \command{\link{varpro}}
}

\examples{
## ------------------------------------------------------------
## van de Vijver microarray breast cancer survival data
## high dimensional example, so we set nodesize to small value
## ------------------------------------------------------------
     
data(vdv, package = "randomForestSRC")
o <- cv.varpro(Surv(Time, Censoring) ~ ., vdv, nodesize.reduce=2, nodesize=2)
print(o)

## ------------------------------------------------------------
## boston housing
## ------------------------------------------------------------

data(BostonHousing, package = "mlbench")
o <- cv.varpro(medv~., BostonHousing)
print(o)

}
\keyword{cv.varpro}
