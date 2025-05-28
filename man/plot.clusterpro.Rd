\name{plot.clusterpro}
\alias{plot.clusterpro}
\title{Plots for Unsupervised Data Visualization}

\description{
 Plots for unsupervised data visualization
}

\usage{\method{plot}{clusterpro}(x, xvar.names, shrink=TRUE,
   col=TRUE, col.names=NULL, sort=TRUE, cex=FALSE, breaks=10, ... )
}

\arguments{

  \item{x}{\code{clusterpro} object returned from a previous call to \code{clusterpro}.}

  \item{xvar.names}{Names (or integer indices) of the x-variables to plot. Defaults to all variables.}

  \item{shrink}{Logical. If \code{TRUE}, shrinks the release variable to zero.}

  \item{col}{Logical. If \code{TRUE}, colors the points in the plot.}

  \item{col.names}{Variable used to color the plots. Defaults to the release variable. Can also be an integer index.}

  \item{sort}{Logical. If \code{TRUE}, sorts plots by variable importance.}

  \item{cex}{Numeric value to scale point size.}

  \item{breaks}{Number of breaks used when mapping colors to points.}

  \item{...}{Additional arguments passed to \code{plot}.}

}

\details{

Generates a two-dimensional visualization using UMAP applied to the enhanced data corresponding to a release variable. This provides a low-dimensional representation of the clustered structure derived from the rule-based transformation of the original data.

}


\author{

  Hemant Ishwaran

}

\references{

  McInnes L., Healy J. and Melville J. (2018).  UMAP: Uniform Manifold
  Approximation and Projection for Dimension Reduction.  ArXiv e-prints.
  
}

\seealso{
  \command{\link{clusterpro}}
}


\keyword{plot}
