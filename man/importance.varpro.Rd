\name{importance.varpro}
\alias{importance.varpro}
\alias{importance}
\alias{get.vimp}
\alias{get.orgvimp}
\alias{get.topvars}
\title{Calculate VarPro Importance}

\description{
 Calculates variable importance using results from previous varpro call.
}

\usage{\method{importance}{varpro}(o, cutoff = 2, trim = 0.1,
  plot.it = FALSE, conf = TRUE, sort = TRUE,
  ylab = if (conf) "Importance" else "Standardized Importance",
  max.rules.tree, max.tree,
  papply = mclapply,
  ...)
}

\arguments{
  
  \item{o}{varpro object obtained from previous call to \code{varpro}.}

  \item{cutoff}{Cut-off value for selecting variables.  Default uses
  two standard deviations.}

  \item{trim}{Used for setting windsorized trim value to robustify mean
  and standard deviation values.}

  \item{plot.it}{Plot the importance values?}

  \item{conf}{Show importance values with standard errors as a boxplot
    (thus showing an informal confidence region)?  If \code{conf='FALSE'},
    plots standardized importance.}

  \item{sort}{Sort the results in decreasing order?}
  
  \item{ylab}{Label used for vertical axis.}
  
  \item{max.rules.tree}{Optional. Maximum number of rules per tree.
    If left unspecified, uses the value from the VarPro object.}

  \item{max.tree}{Maximum number of trees used for extracting rules.
    If left unspecified, uses the value from the VarPro object.}

  \item{papply}{Use mclapply or lapply.}
  
  \item{...}{Additional options.}
  
}

\details{

  Calculates split-sample mean and standard deviation importance values
  using the results from a previous varpro call and converts these to
  standardized importance values from which variables are selected.

}

\value{

  Invisibly, table summarizing the results.  Contains mean importance
  'mean', the standard deviation 'std' and standardized importance 'z'.
  Value 'zcenter' is the latter centered by the cutoff.  Using the
  provided cutoff value, 'selected' identifies selected variables.

  For classification, three additional tables are provided: conditional
  'z', conditional 'zcenter' and conditional 'selected' where the values
  are broken down by class label (i.e. values given are conditional
  on the class label).
  
  See \command{cv.varpro} for a data driven cross-validation method for
  selecting the cutoff value, \code{cutoff}.
  
}

\author{

  Min Lu and Hemant Ishwaran

}

\references{
  
  Lu M. and Ishwaran H. (2022).  Model-Independent Variable Selection
  via the Rule-based Variable Priority (VarPro).
   
}

\seealso{
  \command{\link{cv.varpro}}
  \command{\link{varpro}}
}

\examples{
\donttest{

## ------------------------------------------------------------
## regression example
## ------------------------------------------------------------

data(BostonHousing, package = "mlbench")

## call varpro
o <- varpro(medv~., BostonHousing)

## extract importance values
imp <- importance(o)
print(imp)

## plot the results
imp <- importance(o, plot.it = TRUE)
print(imp)


}
}
\keyword{varpro}
