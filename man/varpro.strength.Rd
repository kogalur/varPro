\name{varpro.strength}
\alias{varpro.strength}
\title{Obtain Strength Array and Other Values from a VarPro Object}
\description{
  Used to parse values from a VarPro object.
}

\usage{
varpro.strength(object,
                m.target = NULL,
                max.rules.tree = 150,
                max.tree = 150,
                stat = c("importance", "complement", "oob"),
                membership = FALSE,
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

  \item{stat}{Determines which statistic is output. Either importance,
    complement mean, or oob mean.}

  \item{membership}{Return OOB and complementary OOB membership indices
    for each rule?}

  \item{seed}{Seed for repeatability.}

  \item{do.trace}{Used to obtain detailed trace.}
}

\details{
  Not intended for end-user use and is primarily meant for internal
  use by the package.
  
}

\examples{
  
\donttest{
## ------------------------------------------------------------
## regression example: boston housing
## ------------------------------------------------------------

## load the data
data(BostonHousing, package = "mlbench")

o <- rfsrc(medv~., BostonHousing, ntree=100)

## call varpro.strength
varpro.strength(object = o, max.rules.tree = 10, max.tree = 15)

}
}
\keyword{varpro.strength}



  


