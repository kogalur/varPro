\name{isopro.varpro}
\alias{isopro.varpro}
\alias{isopro}
\alias{get.iso.performance}
\title{Identify Anomalous Data}

\description{
 Use isolation forests to identify rare/anomalous data.
}

\usage{
isopro.varpro(object,
         method = c("unsupv", "rnd", "auto"),
         sampsize = function(x){min(2^6, .632 * x)},
         ntree = 500, nodesize = 1,
         formula = NULL, data = NULL, ...)
}

\arguments{
  
  \item{object}{VarPro object obtained from previous call.}

  \item{method}{Isolation forest method to be used.  Choices are
    "unsupv" (unsupervised analysis, the default), "rnd" (pure random
    splitting) and "auto" (auto-encoder, a type of multivariate
    forest).}

  \item{sampsize}{Function specifying the sample size used for
    constructing a tree where sampling is without replacement.  Can also
    be specified using a number.}

  \item{ntree}{Number of trees.}

  \item{nodesize}{Minumum terminal node size.}
  
  \item{formula}{Formula for when supervised isolation forest is to be used.
    Ignored if \code{object} is provided.}

  \item{data}{Data frame used for fitting isolation forest.  Ignored if
  \code{object} is provided.}
    
  \item{...}{Additional options to be passed to \command{rfsrc}.}
  
}

\details{

  Isolation Forest (Liu et al., 2008) is a random forest procedure for
  detecting anomalous data.  In the original method, random trees are
  constructed using pure random splitting with each tree constructed
  from a subsample of the data.  Typically the subsample size is much
  smaller than 0.632, the rate used with random forests.  The idea
  is that under random splitting, and because of extreme subsampling,
  rare or anomalous data will tend to split early and become isolated
  very quickly.  Because of this, the depth of an observation, defined
  as the number of splits needed to be become a terminal node, can be
  used to find anomalous observations:  anomalous data points are
  those with relatively small depth values.

  There are several ways to run the code.  The default is to provide a
  formula and a data set via options \code{formula} and \code{data}.  In
  the case of unsupervised analysis (which is the usual application of
  isolation forest), the user only needs to pass in a data set using
  \code{data}.  In this setting, the option \code{method} is applicable
  (see above).  In the case where both a formula and data set is
  provided, then a supervised analysis is run.  The option \code{method}
  is no longer applicable in this case.  This is the less conventional
  approach.

  Another approach (also unconventional) is to pass a VarPro object
  obtained from a previous call (typically this being an unsupervised
  analysis using \code{unsupv}, but this is not the only choice).
  Unsupervised isolation forests is then applied to the dimension
  reduced x matrix from the VarPro call.  Therefore this results in an
  analysis similar to using only the option \code{data}, however the
  difference and advantage is that dimension reduction is achieved via
  VarPro.

  Users should experiment with the type of isolation forest method used.
  This is because while the original isolation forest performs well,
  there are many examples where it can be improved upon.  See the
  examples below which illustrate that the pure random splitting "rnd"
  method may not always be the best.

  Computationally "rnd" is the fastest method followed by "unsupv".  The
  slowest is "auto" which should be used in low-dimensional problems.

}

\author{

  Min Lu and Hemant Ishwaran

}

\references{

  Liu, Fei Tony, Kai Ming Ting, and Zhi-Hua Zhou. (2008). Isolation
  forest.  2008 Eighth IEEE International Conference on Data
  Mining. IEEE.

  Lu M. and Ishwaran H. (2022).  Model-Independent Variable Selection
  via the Rule-based Variable Priority (VarPro).


}

\seealso{
  \command{\link{predict.isopro}}
  \command{\link{unsupv.varpro}}
  \command{\link{varpro}}
}

\examples{

## ------------------------------------------------------------
##
## satellite data: convert some of the classes to "outliers"
## unsupervised isopro analysis
##
## ------------------------------------------------------------

## load data, make three of the classes into outliers
data(Satellite, package = "mlbench")
is.outlier <- is.element(Satellite$classes,
          c("damp grey soil", "cotton crop", "vegetation stubble"))

## remove class labels, make unsupervised data
x <- Satellite[, names(Satellite)[names(Satellite) != "classes"]]

## isopro calls
i.rnd <- isopro(data=x, method = "rnd", sampsize=32)
i.uns <- isopro(data=x, method = "unsupv", sampsize=32)
i.aut <- isopro(data=x, method = "auto", sampsize=32)

## AUC and precision recall (computed using true class label information)
perf <- cbind(get.iso.performance(is.outlier,i.rnd$howbad),
              get.iso.performance(is.outlier,i.uns$howbad),
              get.iso.performance(is.outlier,i.aut$howbad))
colnames(perf) <- c("rnd", "unsupv", "auto")
print(perf)

\donttest{
## ------------------------------------------------------------
##
## boston housing analysis
## isopro analysis using a previous VarPro (supervised) object 
##
## ------------------------------------------------------------

data(BostonHousing, package = "mlbench")

## call varpro first and then isopro
o <- varpro(medv~., BostonHousing)
o.iso <- isopro(o)

## identify data with extreme percentiles
print(BostonHousing[o.iso$howbad <= quantile(o.iso$howbad, .01),])

## ------------------------------------------------------------
##
## boston housing analysis
## supervised isopro analysis - direct call using formula/data
##
## ------------------------------------------------------------

data(BostonHousing, package = "mlbench")

## direct approach uses formula and data options
o.iso <- isopro(formula=medv~., data=BostonHousing)

## identify data with extreme percentiles
print(BostonHousing[o.iso$howbad <= quantile(o.iso$howbad, .01),])


## ------------------------------------------------------------
##
## monte carlo experiment to study different methods
## unsupervised isopro analysis
##
## ------------------------------------------------------------

## monte carlo parameters
nrep <- 25
n <- 1000

## simulation function
twodimsim <- function(n=1000) {
  cluster1 <- data.frame(
    x = rnorm(n, -1, .4),
    y = rnorm(n, -1, .2)
  )
  cluster2 <- data.frame(
    x = rnorm(n, +1, .2),
    y = rnorm(n, +1, .4)
  )
  outlier <- data.frame(
    x = -1,
    y =  1
  )
  x <- data.frame(rbind(cluster1, cluster2, outlier))
  is.outlier <- c(rep(FALSE, 2 * n), TRUE)
  list(x=x, is.outlier=is.outlier)
}

## monte carlo loop
hbad <- do.call(rbind, lapply(1:nrep, function(b) {
  cat("iteration:", b, "\n")
  ## draw the data
  simO <- twodimsim(n)
  x <- simO$x
  is.outlier <- simO$is.outlier
  ## iso pro calls
  i.rnd <- isopro(data=x, method = "rnd")
  i.uns <- isopro(data=x, method = "unsupv")
  i.aut <- isopro(data=x, method = "auto")
  ## save results
  c(tail(i.rnd$howbad,1),
    tail(i.uns$howbad,1),
    tail(i.aut$howbad,1))
}))


## compare performance
colnames(hbad) <- c("rnd", "unsupv", "auto")
print(summary(hbad))
boxplot(hbad,col="blue",ylab="outlier percentile value")
}

}
\keyword{outlier}
