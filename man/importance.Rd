\name{importance}
\alias{importance}
\alias{importance.varpro}
\alias{importance.uvarpro}
\alias{importance.rhf}

\title{Calculate and Plot Variable Importance}

\description{
Calculates and optionally plots variable importance for fitted
\code{varpro}, \code{uvarpro}, and \code{rhf} objects. Larger scores
indicate greater importance.
}

\usage{
importance(x, ...)

\method{importance}{varpro}(x, local.std = TRUE, y.external = NULL,
  cutoff = 0.79, trim = 0.1, plot.it = FALSE, conf = TRUE, sort = TRUE,
  ylab = if (conf) "Importance" else "Standardized Importance",
  max.rules.tree, max.tree,
  ...)

\method{importance}{uvarpro}(x, local.std = FALSE, y.external = NULL,
  cutoff = 0.79, trim = 0.1, plot.it = FALSE, conf = TRUE, sort = TRUE,
  ylab = if (conf) "Importance" else "Standardized Importance",
  max.rules.tree, max.tree,
  ...)

\method{importance}{rhf}(x, local.std = TRUE, y.external = NULL,
  cutoff = 0.79, trim = 0.1, plot.it = FALSE, conf = TRUE, sort = TRUE,
  ylab = if (conf) "Importance" else "Standardized Importance",
  max.rules.tree, max.tree,
  ...)


}

\arguments{

  \item{x}{A \code{varpro}, \code{uvarpro}, or \code{rhf} object.}

  \item{local.std}{Logical. If \code{TRUE}, standardizes each rule
  comparison before combining the results. If \code{FALSE}, divides
  mean importance by its standard deviation across trees. UVarPro
  always uses \code{FALSE}; RHF always uses \code{TRUE}. See Details.}

  \item{y.external}{Optional replacement response, used when
  \code{local.std = TRUE}. Supply values for the same observations,
  in the same order, as those used in the fitted object. See Details
  for the required form. Ignored for \code{uvarpro} objects.}

  \item{cutoff}{Importance-score threshold used in the plot.
  Variables with \code{z >= cutoff} are highlighted. Also sets the
  box limits when \code{conf = TRUE}. The returned scores are
  unchanged.}

  \item{trim}{Fraction of values capped at each end of the
  across-tree importance distribution before calculating summaries
  (winsorization). Must satisfy \code{0 <= trim < 0.5}. Use
  \code{trim = 0} for no capping.}

  \item{plot.it}{Logical. If \code{TRUE}, plots importance when more
  than one tree contributes. Classification plots use overall
  importance. Multivariate regression returns tables only.}

  \item{conf}{Logical. With \code{plot.it = TRUE}, uses a box-style
  display when \code{TRUE}, or a bar plot of \code{z} when
  \code{FALSE}. See Details for the box limits.}

  \item{sort}{Logical. If \code{TRUE}, orders results by decreasing
  importance. Otherwise, uses the predictor order in the fitted
  object.}

  \item{ylab}{Label for the plot's vertical axis.}

  \item{max.rules.tree}{Optional maximum number of rules per tree
  used to calculate importance. Defaults to the fitted object's
  setting for \code{varpro} and \code{uvarpro}, or 150 for
  \code{rhf}.}

  \item{max.tree}{Optional maximum number of trees used to
  calculate importance. Defaults to the fitted object's setting for
  \code{varpro} and \code{uvarpro}, or \code{min(150, x$ntree)} for
  \code{rhf}.}

  \item{...}{Additional graphical arguments passed to \code{bxp}
  or \code{barplot} when \code{plot.it = TRUE}.}

}

\details{

  After fitting a model with \code{\link{varpro}}, use
  \code{importance} to see which predictors have the strongest
  relationship with the response. The function combines importance
  measurements from the forest's rules into a score for each
  evaluated predictor. Larger scores indicate greater importance, and results
  are sorted from largest to smallest by default.

  Use \code{importance(x)} to obtain the scores or
  \code{importance(x, plot.it = TRUE)} to add a plot. Classification
  also provides scores for each class, and multivariate regression
  provides a separate summary for each response.

  \subsection{How the scores are calculated}{
    The forest's rules define groups of observations for comparing
    response values. For regression, the comparisons measure
    differences in response means; for classification, they compare
    class proportions. The default, \code{local.std = TRUE},
    standardizes each comparison using the variability in the
    observations being compared. For survival, the comparisons use
    the numeric survival summaries calculated by \code{varpro}.

    Rule scores are averaged within each tree, weighted by the
    number of out-of-bag observations in the rule. These are
    observations not used to grow that tree. The tree scores are
    then averaged to obtain \code{z}.

    With \code{local.std = FALSE}, the function instead summarizes
    the stored rule scores within each tree, then divides the mean
    tree score by the standard deviation across trees. This ratio
    is returned as \code{z}, together with \code{mean} and
    \code{std}.

    In both cases, \code{trim} limits the influence of unusually
    small or large tree scores. The default, \code{trim = 0.1},
    replaces values below the 10th percentile or above the 90th
    percentile by those percentiles before calculating the mean
    and standard deviation. Set \code{trim = 0} to use the values
    unchanged.
  }

  \subsection{Plots and variable selection}{
    Set \code{plot.it = TRUE} to plot the scores. Variables with
    \code{z >= cutoff} are shown in red. Changing \code{cutoff}
    changes the plot, while the returned importance table continues
    to include the same predictors and scores.

    For a bar plot of \code{z}, set \code{conf = FALSE}. With
    \code{conf = TRUE}, each box is centered on mean importance
    and extends by \code{cutoff * std} in each direction. Under
    local standardization, the center is \code{z} and \code{std}
    is set to 1. Otherwise, \code{std} is the standard deviation
    across trees after winsorization. The boxes provide an informal
    visual guide to the cutoff.

    To select predictors for a prediction model, use
    \code{\link{cv.varpro}}, which compares prediction errors for
    variable sets formed using different importance cutoffs.
  }

  \subsection{Using another response}{
    Supply \code{y.external} with \code{local.std = TRUE} to
    calculate importance for another response using the same forest
    rules. For example, supplying predictions from another model
    shows which predictors are important for those predictions.

    For a single numeric response, including a survival summary,
    supply a numeric vector. For multiple numeric responses, supply
    a matrix with the same dimensions and response-column order as
    \code{x$y}. For classification, supply a factor with the same
    levels and coding as \code{x$y}. In every case, values must
    follow the observation order in the fitted object, including
    any omissions made during fitting.
  }

  \subsection{UVarPro and RHF objects}{
    For \code{uvarpro} objects, the function summarizes the rule
    scores from the unsupervised analysis using
    \code{local.std = FALSE}. See \code{\link{uvarpro}} for the
    interpretation of these scores.

    For \code{rhf} objects, importance uses log-transformed
    integrated hazard estimates and always applies local
    standardization. Out-of-bag estimates are used when available,
    and inbag estimates otherwise.
  }

}

\value{

  For regression, survival with a single response summary, and RHF,
  a data frame with predictor names as row names. With
  \code{local.std = TRUE}, the table contains only \code{z}:
  the mean locally standardized importance across trees after
  winsorization. With \code{local.std = FALSE}, including UVarPro
  analyses, it contains:
  \describe{
    \item{\code{mean}}{Mean importance across trees after
      winsorization.}
    \item{\code{std}}{Standard deviation of importance across trees
      after winsorization.}
    \item{\code{z}}{Standardized importance, \code{mean / std}.}
  }

  For classification, a list with two components:
  \describe{
    \item{\code{unconditional}}{Overall importance table, with the
      same columns described above.}
    \item{\code{conditional.z}}{Matrix of class-specific importance
      scores, with predictors in rows and the fitted object's class
      labels in columns.}
  }

  For multivariate regression, including survival analyses with
  multiple response summaries, a list containing one importance
  table per response, in response-column order.

  The summaries cover predictors represented in the evaluated rules.
  Categorical predictors can appear as several indicator columns.
  For VarPro or UVarPro objects, \code{get.orgvimp(x)} gives scores
  under the original predictor names, using the largest available
  score for each predictor.

}

\author{

  Min Lu and Hemant Ishwaran

}

\references{

  Lu, M. and Ishwaran, H., (2024). Model-independent variable selection
  via the rule-based variable priority. arXiv e-prints, pp.arXiv-2409.


}

\seealso{
  \command{\link{cv.varpro}}
  \command{\link{varpro}}
  \command{\link{uvarpro}}
}

\examples{


## ------------------------------------------------------------
## toy example - needed to pass CRAN test
## ------------------------------------------------------------

## mtcars regression
o <- varpro(mpg ~ ., mtcars, ntree = 1)
imp <- importance(o, local.std = FALSE)
print(imp)

\donttest{

## ------------------------------------------------------------
## iris example
## ------------------------------------------------------------

## apply varpro to the iris data
o <- varpro(Species ~ ., iris, max.tree = 5)

## print/plot the results
imp <- importance(o, plot.it = TRUE)
print(imp)

## ------------------------------------------------------------
## boston housing: regression
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



## ------------------------------------------------------------
## illustrates y-external: regression example
## ------------------------------------------------------------

## friedman1 - standard application of varpro
d <- data.frame(mlbench::mlbench.friedman1(250),noise=matrix(runif(250*10,-1,1),250))
o <- varpro(y~.,d)
print(importance(o))

## importance using external rf predictor
print(importance(o,y.external=randomForestSRC::rfsrc(y~.,d)$predicted.oob))

## importance using external lm predictor
print(importance(o,y.external=lm(y~.,d)$fitted))

## importance using external randomized predictor
print(importance(o,y.external=sample(o$y)))

## ------------------------------------------------------------
## illustrates y-external: classification example
## ------------------------------------------------------------

## iris - standard application of varpro
o <- varpro(Species~.,iris)
print(importance(o))

## importance using  external rf predictor
print(importance(o,y.external=randomForestSRC::rfsrc(Species~.,iris)$class.oob))

## importance using  external randomized predictor
print(importance(o,y.external=sample(o$y)))

## ------------------------------------------------------------
## illustrates y-external: survival
## ------------------------------------------------------------
data(pbc, package = "randomForestSRC")
o <- varpro(Surv(days, status)~., pbc)
print(importance(o))

## importance using  external rsf predictor
print(importance(o,y.external=randomForestSRC::rfsrc(Surv(days, status)~., pbc)$predicted.oob))

## importance using  external randomized predictor
print(importance(o,y.external=sample(o$y)))


}
}
\keyword{varpro}
