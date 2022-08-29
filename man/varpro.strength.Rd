\name{varpro.strength}
\alias{varpro.strength}
\title{xxxxxxxxxxx}
\description{
  xxxxxxxxxxx
}

\usage{
varpro.strength(object,
                m.target = NULL,
                max.rules.tree = 150,
                max.tree = 150,
                stat = c("importance", "complement", "oob"),
                seed = NULL,
                do.trace = FALSE)
}

\arguments{

  \item{object}{rfsrc object}

  \item{m.target}{Character value for multivariate families
    specifying the target outcome to be used.  If left unspecified, the
    algorithm will choose a default target.}

  \item{max.rules.tree}{Maximum number of rules per tree.}

  \item{max.tree}{Maximum number of trees used for extracting rules.}

  \item{stat}{Determines which statistic is output. Either importabce,
    complement mean, or oob mean.}

  \item{seed}{Seed for repeatability.}

  \item{do.trace}{xxxxxxxx}
}

\details{
  xxxxxxxxxxx
}

\value{
 xxxxxxxxxxx
}

\author{
 xxxxxxxxxxx 
}

\references{
 xxxxxxxxxxx
}

\seealso{
 xxxxxxxxxx
}


\examples{
  
\donttest{
## ------------------------------------------------------------
## regression example: boston housing
## ------------------------------------------------------------

if (library("randomForestSRC", logical.return = TRUE)) {

## load the data
data(BostonHousing, package = "mlbench")

o <- rfsrc(medv~., BostonHousing, ntree=100)

## call varpro.strength
varpro.strength(object = o, max.rules.tree = 10, max.tree = 15)

}
}
}
\keyword{varpro.strength}



  


