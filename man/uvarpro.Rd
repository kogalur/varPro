\name{uvarpro}
\alias{uvarpro}
\title{Unsupervised Variable Selection using Variable Priority (UVarPro)}
\description{
  Variable Selection in Unsupervised Problems using UVarPro
}

\usage{
uvarpro(data,
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

  UVarPro performs unsupervised variable selection by applying the
  VarPro framework to random forests trained on unlabeled data. The
  forest construction is governed by the \code{method} argument. By
  default, \code{method = "auto"} fits a random forest autoencoder,
  which regresses each selected variable on itself, a specialized form
  of multivariate forest modeling. Alternatives include \code{"unsupv"},
  which uses pseudo-responses and multivariate splits to build an
  unsupervised forest (Tang and Ishwaran, 2017), and \code{"rnd"}, which
  uses completely random splits. For large datasets, the autoencoder may
  be slower, while the \code{"unsupv"} and \code{"rnd"} options are
  typically more computationally efficient.
  

  Variable importance is measured using an entropy-based criterion that
  reflects the overall variance explained by each feature. Users may
  also supply custom entropy functions to define alternative importance
  metrics. See the examples for details.

}

\value{

  A uvarpro object.
 
}

\author{
  Min Lu and Hemant Ishwaran
}

\references{

  Tang F. and Ishwaran H. (2017).  Random forest missing data
  algorithms.  \emph{Statistical Analysis and Data Mining}, 10:363-377.

}

\seealso{
  \command{\link{varpro}}
}

\examples{
## ------------------------------------------------------------
## boston housing: default call
## ------------------------------------------------------------

data(BostonHousing, package = "mlbench")

## default call
o <- uvarpro(BostonHousing)
print(importance(o))

## ------------------------------------------------------------
## boston housing: using method="unsupv"
## ------------------------------------------------------------

data(BostonHousing, package = "mlbench")

## unsupervised splitting 
o <- uvarpro(BostonHousing, method = "unsupv")
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
print(importance(o <- uvarpro(Boston)))

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
print(importance(uvarpro(dta)))


## ------------------------------------------------------------
## glass (remove outcome)
## ------------------------------------------------------------

data(Glass, package = "mlbench")

## remove the outcome
Glass$Type <- NULL

## get importance
o <- uvarpro(Glass)
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
print(importance(uvarpro(housing2, ntree = 50, nodesize = 150)))

## ------------------------------------------------------------
##  custom importance
##  OPTION 1: use hidden entropy option
## ------------------------------------------------------------

my.entropy <- function(xC, xO, ...) {

  ## xC     x feature data from complementary region
  ## xO     x feature data from original region
  ## ...    used to pass aditional options (required)
 
  ## custom importance value
  wss <- mean(apply(rbind(xO, xC), 2, sd, na.rm = TRUE))
  bss <- (mean(apply(xC, 2, sd, na.rm = TRUE)) +
              mean(apply(xO, 2, sd, na.rm = TRUE)))
  imp <- 0.5 * bss / wss
  
  ## entropy value must contain complementary and original membership
  entropy <- list(comp = list(...)$compMembership,
                  oob = list(...)$oobMembership)

  ## return importance and in the second slot the entropy list 
  list(imp = imp, entropy)

}

o <- uvarpro(BostonHousing, entropy=my.entropy)
print(importance(o))


## ------------------------------------------------------------
##  custom importance
##  OPTION 2: direct importance without hidden entropy option
## ------------------------------------------------------------

o <- uvarpro(BostonHousing, ntree=3, max.rules.tree=10)

## convert original/release region into two-class problem
## define importance as the lasso beta values 

## For faster performance on Unix systems, consider using:
## library(parallel)
## imp <- do.call(rbind, mclapply(seq_along(o$entropy), function(j) { ... }))

imp <- do.call(rbind, lapply(seq_along(o$entropy), function(j) {
  rO <- do.call(rbind, lapply(o$entropy[[j]], function(r) {
    xC <- o$x[r[[1]],names(o$entropy),drop=FALSE]
    xO <- o$x[r[[2]],names(o$entropy),drop=FALSE]
    y <- factor(c(rep(0, nrow(xC)), rep(1, nrow(xO))))
    x <- rbind(xC, xO)
    x <- x[, colnames(x) != names(o$entropy)[j]]
    fit <- tryCatch(
      suppressWarnings(glmnet::cv.glmnet(as.matrix(x), y, family = "binomial")),
      error = function(e) NULL
    )
    if (!is.null(fit)) {
      beta <- setNames(rep(0, length(o$entropy)), names(o$entropy))
      bhat <- abs(coef(fit)[-1, 1])
      beta[names(bhat)] <- bhat
      beta
    } else {
      NULL
    }
  }))
  if (!is.null(rO)) {
    val <- colMeans(rO, na.rm = TRUE)
    names(val) <- colnames(rO)
    return(val)
  } else {
    return(NULL)
  }
}) |> setNames(names(o$entropy)))

print(imp)


## ------------------------------------------------------------
##  custom importance
##  OPTION 3: direct importance using built in lasso beta function
## ------------------------------------------------------------

o <- uvarpro(BostonHousing, ntree=3, max.rules.tree=10)
print((beta <- get.beta.entropy(o)))

## bonus: display s-dependent graph
sdependent(beta)

}}


\keyword{uvarpro}
