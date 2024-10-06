\name{plot.clusterpro.varpro}
\alias{plot.clusterpro.varpro}
\alias{plot.clusterpro}
\title{Plots for Unsupervised Data Visualization}

\description{
 Plots for unsupervised data visualization
}

\usage{\method{plot.clusterpro}{varpro}(x, xvar.names, shrink=TRUE,
   col=TRUE, col.names=NULL, sort=TRUE, cex=FALSE, breaks=10, ... )
}

\arguments{
  
  \item{x}{clusterpro object obtained from previous call to \code{clusterpro}.}

  \item{xvar.names}{Names of the x-variables to be plotted (uses all by
   default).  Can also be integer values.}

  \item{shrink}{Shrink the release variable to zero?}
  
  \item{col}{Color the plots?}
  
  \item{col.names}{The default is to color each plot by the release
   variable.  This option allows plots to be colored by the same
   variable specified by \code{col.names}.  Can also be an integer
   value.}

  \item{sort}{Sort the plot by the importance of a variable?}

  \item{cex}{Scale size of the points?}

  \item{breaks}{Number of breaks used when coloring points.}
    
  \item{...}{Additional options passed to plot.}
  
}

\details{

  Generates a two-dimensional visualization plot using UMAP applied to
  the enhanced data of a release variable.

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
