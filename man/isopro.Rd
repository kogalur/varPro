\name{isopro}
\alias{isopro}
\title{Identify Anomalous Data}

\description{
 Use isolation forests to identify rare/anomalous data.
}

\usage{
isopro(object,
       method = c("unsupv", "rnd", "auto"),
       sampsize = function(x){min(2^6, .632 * x)},
       ntree = 500, nodesize = 1,
       formula = NULL, data = NULL, ...)
}

\arguments{

  \item{object}{\code{varpro} object returned from a previous call.}

  \item{method}{Isolation forest method. Options are \code{"unsupv"}
  (unsupervised analysis, default), \code{"rnd"} (pure random
  splitting), and \code{"auto"} (auto-encoder, a type of multivariate
  forest).}


  \item{sampsize}{Function or numeric value specifying the sample size
    used for constructing each tree. Sampling is without replacement.}
  

  \item{ntree}{Number of trees to grow.}

  \item{nodesize}{Minimum terminal node size.}

  \item{formula}{Formula used for supervised isolation forest. Ignored
    if \code{object} is provided.}
  

  \item{data}{Data frame used to fit the isolation forest. Ignored if
    \code{object} is provided.}
  

  \item{...}{Additional arguments passed to \code{rfsrc}.}

}

\details{

Isolation Forest (Liu et al., 2008) is a random forest-based method for
detecting anomalous observations. In its original form, trees are
constructed using pure random splits, with each tree built from a small
subsample of the data, typically much smaller than the standard 0.632
fraction used in random forests. The idea is that anomalous or rare
observations are more likely to be isolated early, requiring fewer
splits to reach terminal nodes. Thus, observations with relatively small
depth values (i.e., shallow nodes) are considered anomalies.


There are several ways to apply the method:

\itemize{
  
  \item The default approach is to supply a \code{formula} and
  \code{data} to build a supervised isolation forest. If only
  \code{data} is provided (i.e., no response), an unsupervised analysis
  is performed. In this case, the \code{method} option is used to
  specify the type of isolation forest (e.g., \code{"unsupv"},
  \code{"rnd"}, or \code{"auto"}).
  

  
  \item If both a formula and data are provided, a supervised model is
  fit. In this case, \code{method} is ignored. While less conventional,
  this approach may be useful in certain applications.
  
  
  \item Alternatively, a \code{varpro} object may be supplied, but other
  configurations are also supported. In this setting, isolation forest
  is applied to the reduced feature matrix extracted from
  theobject. This is similar to using the \code{data} option alone but
  with the advantage of prior dimension reduction.
  

}

Users are encouraged to experiment with the choice of \code{method}, as
the original isolation forest (\code{"rnd"}) performs well in many
scenarios but can be improved upon in others. For example, in some
cases, \code{"unsupv"} or \code{"auto"} may yield better detection
performance.


In terms of computational cost, \code{"rnd"} is the fastest, followed by
\code{"unsupv"}. The slowest is \code{"auto"}, which is best suited for
low-dimensional settings.


}


\author{

  Min Lu and Hemant Ishwaran

}

\references{

  Liu, Fei Tony, Kai Ming Ting, and Zhi-Hua Zhou. (2008). Isolation
  forest.  2008 Eighth IEEE International Conference on Data
  Mining. IEEE.

  Ishwaran H. (2025).  Multivariate Statistics: Classical Foundations
  and Modern Machine Learning, CRC (Chapman and Hall), in press.


}

\seealso{
  \command{\link{predict.isopro}}
  \command{\link{uvarpro}}
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
