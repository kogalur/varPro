\name{varpro.strength}
\alias{varpro.strength}
\title{Obtain Strength Array and Other Values from a VarPro Object}

\description{
  Used to parse values from a VarPro object.
}

\usage{
varpro.strength(object,
                newdata,
                m.target = NULL,
                max.rules.tree = 150,
                max.tree = 150,
                stat = c("importance", "complement", "oob", "none"),
                membership = FALSE,
                neighbor = 5,
                seed = NULL,
                do.trace = FALSE, ...)
}

\arguments{

  \item{object}{\code{rfsrc} object.}

  \item{newdata}{Optional test data. If provided, returns branch and complementary branch membership of the training data corresponding to the test cases.}

  \item{m.target}{Character string specifying the target outcome for multivariate families. If unspecified, a default is selected automatically.}

  \item{max.rules.tree}{Maximum number of rules extracted per tree.}

  \item{max.tree}{Maximum number of trees used for rule extraction.}

  \item{stat}{Statistic to output. Options include \code{"importance"}, \code{"complement mean"}, and \code{"oob mean"}.}

  \item{membership}{Return out-of-bag and complementary membership indices for each rule?}

  \item{neighbor}{Nearest neighbor parameter, used only when \code{newdata} is specified.}

  \item{seed}{Seed for reproducibility.}

  \item{do.trace}{Enable detailed trace output.}

  \item{...}{Additional arguments.}

}

\details{
  Not intended for direct end-user use; primarily designed for internal package operations.
}


\examples{
  
\donttest{
## ------------------------------------------------------------
## regression example: boston housing
## ------------------------------------------------------------

## load the data
data(BostonHousing, package = "mlbench")

o <- randomForestSRC::rfsrc(medv~., BostonHousing, ntree=100)

## call varpro.strength
varpro.strength(object = o, max.rules.tree = 10, max.tree = 15)

## call varpro.strength with test data
varpro.strength(object = o, newdata = BostonHousing[1:3,], max.rules.tree = 10, max.tree = 15)

}
}
\keyword{varpro.strength}



  


