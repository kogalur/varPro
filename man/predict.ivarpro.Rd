\name{predict.ivarpro}
\alias{predict.ivarpro}
\title{Predict Individual Variable Priority for New Cases}
\description{
  Apply an iVarPro analysis to new observations, or recover its
  out-of-bag training scores. The result gives a local gradient for
  each case and processed predictor.
}
\usage{
\method{predict}{ivarpro}(object, newdata = NULL, model = NULL,
  noise.na = NULL, path.store.membership = FALSE,
  save.data = TRUE, ...)
}
\arguments{
  \item{object}{An object returned by \code{ivarpro}.}
  \item{newdata}{A data frame containing the predictors for new cases.
    Outcome columns are optional. For a VarPro model, supply the
    original predictor columns; the training hot-encoding is applied
    automatically. If \code{NULL}, recover the original training
    scores using out-of-bag rule membership.}
  \item{model}{The VarPro object or random forest used to obtain
    \code{object}. By default, use the model stored by
    \code{ivarpro(save.model = TRUE)}. Supply it here if it was not
    stored.}
  \item{noise.na}{How to average unavailable rule scores: \code{TRUE}
    omits them and returns \code{NA} where no usable score is available;
    \code{FALSE} includes them as zeros. By default, inherit the
    setting from \code{object}.}
  \item{path.store.membership}{Retain the case-to-rule membership
    indices with the result? These are optional diagnostic information.}
  \item{save.data}{Store the processed predictor data for subsequent
    plots? Default is \code{TRUE}.}
  \item{...}{Additional arguments passed to the forest prediction
    method when memberships are obtained.}
}
\details{
  New cases are routed through the original forest. For each predictor,
  their scores average the stored gradients of applicable rules that
  release that predictor. The local slopes and neighborhood choices
  remain those learned in the original iVarPro analysis.

  Scores retain the target, scaling and signed or absolute convention
  chosen in \code{ivarpro}. Multiple targets produce a named list of
  score tables. Use \code{plot} or \code{shap.ivarpro} to inspect the
  result, with \code{target} selecting a response or class when needed.

  Omitting \code{newdata} recovers training scores using out-of-bag
  membership. Passing the training predictors explicitly treats those
  observations as new cases and uses all applicable forest rules, so
  the two calls can give different scores.
}
\value{
  An object of class \code{ivarpro}, with one row per prediction case
  and one column per processed predictor. A single target produces a
  data frame; multiple targets produce a named list of data frames.
  Row names identify the prediction cases.

  The result retains the target labels, original model and rule
  information needed for further prediction. With \code{save.data = TRUE},
  its \code{data} attribute contains aligned plotting data. For new
  cases this contains processed predictors; the original explanation
  targets are not recalculated. See \code{\link{ivarpro}} for score
  interpretation.
}
\seealso{
  \code{\link{ivarpro}}, \code{\link{plot.ivarpro}},
  \code{\link{shap.ivarpro}}
}
\examples{
\donttest{
## Train on one sample and explain new observations.
set.seed(137)
n <- 400
sim <- data.frame(x1 = runif(n, -1, 1), x2 = runif(n, -1, 1))
sim$y <- 6 * sim$x1 * sim$x2 + rnorm(n, sd = 0.3)
trn <- sample.int(n, 300)
vp <- varpro(y ~ ., sim[trn, , drop = FALSE],
             split.weight = FALSE, ntree = 100,
             parallel = FALSE)
iv <- ivarpro(vp, scale = "none", nmin = 10)

## Outcome values are not needed for new-case gradients.
test <- sim[-trn, c("x1", "x2"), drop = FALSE]
iv.test <- predict(iv, newdata = test)
print(head(iv.test))
plot(iv.test, var = "x1", col.var = "x2")

## Recover the original out-of-bag training scores.
iv.oob <- predict(iv)
print(all.equal(as.matrix(iv.oob), as.matrix(iv),
                check.attributes = FALSE))
}}
\keyword{models}
