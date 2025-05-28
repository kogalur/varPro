\name{clusterpro}
\alias{clusterpro}
\title{ClusterPro for Unsupervised Data Visualization}

\description{
  ClusterPro for unsupervised data visualization using Varpro
  rules.
}

\usage{
clusterpro(data,
           method = c("auto", "unsupv", "rnd"),
           ntree = 100, nodesize = NULL,
           max.rules.tree = 40, max.tree = 40,
           papply = mclapply, verbose = FALSE, seed = NULL, ...)
}

\arguments{

  \item{data}{Data frame containing the unsupervised data.}

  \item{method}{Type of forest used. Options are \code{"auto"} (auto-encoder), \code{"unsupv"} (unsupervised analysis), and \code{"rnd"} (pure random forest).}

  \item{ntree}{Number of trees to grow.}

  \item{nodesize}{Minimum terminal node size. If not specified, the value is chosen by an internal function based on sample size and data dimension.}

  \item{max.rules.tree}{Maximum number of rules per tree.}

  \item{max.tree}{Maximum number of trees used to extract rules.}

  \item{papply}{Parallel apply method; typically \code{mclapply} or \code{lapply}.}

  \item{verbose}{Print verbose output?}

  \item{seed}{Seed for reproducibility.}

  \item{...}{Additional arguments passed to \code{uvarpro}.}

}

\details{

Unsupervised data visualization tool based on the VarPro framework. For
each VarPro rule and its complementary region, a two-class analysis is
performed to estimate regression coefficients that quantify variable
importance relative to the release variable. These coefficients are then
used to scale the centroids of the two regions.

The resulting scaled centroids from all rule pairs form an enhanced
learning data set for the release variable. This transformed data can be
passed to standard visualization tools (e.g., UMAP or t-SNE) to explore
structure and relationships in the original high-dimensional space.

}

\author{

  Hemant Ishwaran

}

\seealso{
  \command{\link{plot.clusterpro}}
  \command{\link{uvarpro}}
}

\examples{
\donttest{

##------------------------------------------------------------------
##
## V-cluster simulation
##
##------------------------------------------------------------------


vcsim <- function(m=500, p=9, std=.2) {
  p <- max(p, 2)
  n <- 2 * m
  x <- runif(n, 0, 1)
  y <- rep(NA, n)
  y[1:m] <- x[1:m] + rnorm(m, sd = std)
  y[(m+1):n] <- -x[(m+1):n] + rnorm(m, sd = std)
  data.frame(x = x,
             y = y,
             z = matrix(runif(n * p, 0, 1), n))
}

dvc <- vcsim()
ovc <- clusterpro(dvc)
par(mfrow=c(3,3));plot(ovc,1:9)
par(mfrow=c(3,3));plot(ovc,1:9,col.names="x")

##------------------------------------------------------------------
##
## 4-cluster simulation
##
##------------------------------------------------------------------


if (library("MASS", logical.return=TRUE)) {


fourcsim <- function(n=500, sigma=2) {
  
  cl1 <- mvrnorm(n,c(0,4),cbind(c(1,0),c(0,sigma)))
  cl2 <- mvrnorm(n,c(4,0),cbind(c(1,0),c(0,sigma)))
  cl3 <- mvrnorm(n,c(0,-4),cbind(c(1,0),c(0,sigma)))
  cl4 <- mvrnorm(n,c(-4,0),cbind(c(1,0),c(0,sigma)))
  dta <- data.frame(rbind(cl1,cl2,cl3,cl4))
  colnames(dta) <- c("x","y")
  data.frame(dta, noise=matrix(rnorm((n*4)*20),ncol=20))

}

d4c <- fourcsim()
o4c <- clusterpro(d4c)
par(mfrow=c(2,2));plot(o4c,1:4)

}

##------------------------------------------------------------------
##
## latent variable simulation
##
##------------------------------------------------------------------

lvsim <- function(n=1000, q=2, qnoise=15, noise=FALSE)  {
  w <- rnorm(n)
  x <- rnorm(n)
  y <- rnorm(n)
  z <- rnorm(n)
  ei <- matrix(rnorm(n * q * 4, sd = sqrt(.1)), ncol = q * 4)
  e1 <- rnorm(n, sd = sqrt(.4))
  e2 <- rnorm(n, sd = sqrt(.4))
  wi <- w + ei[, 1:q]
  xi <- x + ei[, (q+1):(2*q)]
  yi <- y + ei[, (2*q+1):(3*q)]
  zi <- z + ei[, (3*q+1):(4*q)]
  h1 <- w + x + e1
  h2 <- y + z + e2
  dta <- data.frame(w=w,wi=wi,x=x,xi=xi,y=y,yi=yi,z=z,zi=zi,h1=h1,h2=h2)
  if (noise) {
    dta <- data.frame(dta, noise = matrix(rnorm(n * qnoise), ncol = qnoise))
  }
  dta
}

dlc <- lvsim()
olc <- clusterpro(dlc)
par(mfrow=c(4,4));plot(olc,col.names="w")


##------------------------------------------------------------------
##
## Glass mlbench data 
##
##------------------------------------------------------------------

data(Glass, package = "mlbench")
dg <- Glass

## with class label
og <- clusterpro(dg)
par(mfrow=c(4,4));plot(og,1:16)

## without class label
dgU <- Glass; dgU$Type <- NULL
ogU <- clusterpro(dgU)
par(mfrow=c(3,3));plot(ogU,1:9)

}
}
\keyword{plot}
