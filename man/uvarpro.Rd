\name{uvarpro}
\alias{uvarpro}
\alias{get.beta.entropy}
\alias{sdependent}

\title{Unsupervised Variable Selection and Dependence Graphs using UVarPro}

\description{
  Identifies variables that help explain dependence in data without a
  specified outcome. \code{uvarpro} forms local comparisons using forest
  rules. \code{get.beta.entropy} summarizes which variables help
  distinguish the groups in these comparisons. \code{sdependent}
  displays the resulting relationships as a graph and highlights
  candidate signal variables.
}

\usage{
uvarpro(data,
        method = c("auto", "unsupv", "rnd"),
        ntree = 200, nodesize = NULL,
        max.rules.tree = 20, max.tree = 200,
        verbose = FALSE, seed = NULL,
        ...)

get.beta.entropy(o,
                 second.stage = FALSE,
                 pre.filter = TRUE,
                 papply = mclapply,
                 vimp.min = 0,
                 nfolds = 10,
                 maxit = 2500,
                 thresh = 1e-3,
                 parallel = FALSE,
                 use.cv = TRUE,
                 lambda.sel = c("lambda.1se", "lambda.min"),
                 nlambda = NULL,
                 lambda.min.ratio = NULL,
                 nlambda.lasso = nlambda,
                 lambda.min.ratio.lasso = lambda.min.ratio)

sdependent(I,
           threshold = .25,
           layout = "grid",
           q.signal = .75,
           directed = TRUE,
           min.degree = NULL,
           title = "s-Dependent Variable Detection",
           plot = TRUE)
}

\arguments{
  \item{data}{Data frame containing the features to be analyzed, without
    an outcome or class-label column. Rows with missing values are
    omitted, unused factor levels are dropped, and categorical features
    are hot-encoded. See Details.}

  \item{method}{Method for growing the forest: \code{"auto"} learns to
    reproduce the supplied variables, \code{"unsupv"} uses unsupervised
    splitting, and \code{"rnd"} uses random splitting. See Details.}

  \item{ntree}{Number of trees to grow.}

  \item{nodesize}{Minimum terminal node size. Larger values give broader
    groups of observations. The default is chosen from the number
    of complete observations.}

  \item{max.rules.tree}{Maximum number of rules sampled per tree for
    the local comparisons.}

  \item{max.tree}{Maximum number of trees used to extract rules.}

  \item{verbose}{Currently unused.}

  \item{seed}{Optional seed passed to the forest calculation. Use
    \code{set.seed} before \code{uvarpro} to also control R-level
    sampling, and seed the lasso analysis separately; see Note.}

  \item{...}{Additional named forest arguments recognized by
    \code{rfsrc}, such as \code{mtry} or \code{ytry}, and a custom
    scoring function supplied as \code{entropy}; see Details.
    Supply lasso controls to \code{get.beta.entropy}.}

  \item{o}{An object returned by \code{uvarpro}, containing the
    processed data in \code{x} and the group membership lists
    in \code{entropy}.}

  \item{second.stage}{If \code{FALSE}, return the mean absolute
    logistic-lasso coefficient matrix. If \code{TRUE}, perform an
    additional local linear lasso using the predictors selected by the
    logistic stage, and return its mean absolute coefficients divided
    by the standard deviation of the released variable. See Details.}

  \item{pre.filter}{Should preliminary importance from \code{uvarpro}
    restrict the lasso analysis to variables with
    \code{get.vimp(o, pretty = FALSE) > vimp.min}? The filter applies
    to both predictors and release variables. It is skipped when
    preliminary importance is unavailable.}

  \item{papply}{An \code{lapply}-like function for processing released
    variables. Use \code{lapply} for serial execution. The default
    \code{mclapply} uses separate R worker sessions (a PSOCK cluster)
    when \code{getOption("mc.cores", 1L)} is greater than one, and
    serial execution otherwise. The \code{parallel}
    argument controls parallelism across cross-validation folds.}

  \item{vimp.min}{Strict lower cutoff for preliminary importance when
    \code{pre.filter = TRUE}. The default retains values greater than
    zero. Ignored when \code{pre.filter = FALSE}.}

  \item{nfolds}{Number of cross-validation folds for each local
    logistic-lasso analysis. At least three folds are required.
    Ignored when \code{use.cv = FALSE}.}

  \item{maxit}{Maximum number of iterations passed to \code{glmnet}
    in both lasso stages.}

  \item{thresh}{Convergence tolerance passed to \code{glmnet}.}

  \item{parallel}{Should \code{cv.glmnet} process its folds in parallel?
    A suitable \code{foreach} backend must be registered. Ignored when
    \code{use.cv = FALSE}. Account for available resources when
    enabling both levels of parallelism.}

  \item{use.cv}{Should the logistic-lasso penalty be chosen by
    cross-validation? If \code{FALSE}, coefficients are taken at the
    last, smallest penalty on the computed \code{glmnet} path.}

  \item{lambda.sel}{Cross-validation penalty choice for the logistic
    stage. \code{"lambda.1se"} selects the largest penalty whose
    cross-validation error is within one standard error of the minimum;
    \code{"lambda.min"} selects the penalty minimizing that error.
    Ignored when \code{use.cv = FALSE}.}

  \item{nlambda}{Number of penalty values requested for the logistic
    stage. \code{NULL} leaves the \code{glmnet} default unchanged. The
    computed path can end before all requested values are reached.}

  \item{lambda.min.ratio}{Smallest requested logistic-stage penalty
    relative to the largest penalty on the path. \code{NULL} uses the
    \code{glmnet} default.}

  \item{nlambda.lasso, lambda.min.ratio.lasso}{Corresponding path
    controls for the optional second-stage linear lasso. They default
    to the logistic-stage settings and are used only when
    \code{second.stage = TRUE}. The second stage uses the last
    computed penalty.}

  \item{I}{A finite, nonnegative numeric importance matrix, usually
    obtained from \code{get.beta.entropy}, with released variables as
    rows and predictors as columns. Supply unique, nonempty row and
    column names. Row names must belong to the column names and may
    occur in any order. Rows are aligned by name; missing release rows
    are filled with zeros.}

  \item{threshold}{Positive edge cutoff on the scale of \code{I}.
    An off-diagonal entry greater than or equal to this value creates
    an edge. Increasing the cutoff removes edges; decreasing it can
    recover weaker connections.}

  \item{layout}{Graph layout. Supported names are \code{"fr"},
    \code{"dh"}, \code{"gem"}, \code{"kk"}, \code{"lgl"},
    \code{"mds"}, \code{"sugiyama"}, \code{"graphopt"},
    \code{"nicely"}, \code{"random"}, \code{"sphere"},
    \code{"grid"}, \code{"circle"}, \code{"star"}, \code{"tree"},
    and \code{"bipartite"}; unambiguous abbreviations are accepted.
    Layout-specific requirements of \pkg{igraph} still apply.
    Alternatively, supply a numeric coordinate matrix with one row
    per nonisolated vertex, in graph vertex order.}

  \item{q.signal}{Quantile of the global importance scores among
    nonisolated vertices used for signal designation. Values range
    from zero to one; the default is the upper quartile. A signal
    must meet both this cutoff and the degree condition.}

  \item{directed}{Should the graph be directed? In the directed graph,
    an edge goes from the released variable to the predictor used to
    distinguish its local comparison groups. For an undirected
    display, supply a symmetric importance matrix; see Note.}

  \item{min.degree}{Minimum degree for candidate signal designation.
    With directed graphs this is the out-degree. The default is 1
    for directed graphs and 2 for undirected graphs.}

  \item{title}{Title for the graph.}

  \item{plot}{Should the graph be plotted? If \code{FALSE}, graph
    construction and signal selection are still performed. The
    \pkg{igraph} package is required in either case.}
}

\details{
  UVarPro helps identify variables that explain dependence in a data
  set without requiring a response. Every supplied column is treated
  as a variable to be studied. The results can be inspected as an
  importance matrix, summarized as a ranking, or displayed as a graph.

  The main workflow uses three functions:
  \enumerate{
    \item \code{o <- uvarpro(data)} uses a forest to define groups
      of observations for local comparisons.
    \item \code{I <- get.beta.entropy(o, papply = lapply)} estimates
      which variables distinguish those groups and returns an
      importance matrix.
    \item \code{sdependent(I)} displays the relationships as a graph
      and identifies candidate signal variables.
  }
  Inspect the matrix before plotting; \code{get.beta.entropy} can
  return \code{NULL} when no usable coefficient estimates are found.
  \code{importance(o)} gives a separate, preliminary summary used for
  optional screening. It is not the lasso importance matrix used to
  construct the graph.

  \subsection{Understanding a local comparison}{
    A forest rule defines a group through conditions on variable values.
    For example, a rule might require \eqn{x_1 \leq 0.5}{x1 <= 0.5}
    and \eqn{x_2 > 0.3}{x2 > 0.3}. Removing the condition on
    \eqn{x_1}{x1}, while keeping the other condition, is called
    \emph{releasing} \eqn{x_1}{x1}. The newly admitted observations
    form the \emph{near-miss}, or complementary, group.

    For a rule region \eqn{R} and released variable \eqn{s}, write
    the enlarged region as \eqn{R^{(s)}}{R^(s)}. The near-miss set is
    \deqn{C_s = R^{(s)} \setminus R.}{C_s = R^(s) minus R.}
    Original-region observations are out-of-bag for the corresponding
    tree. Both groups must be nonempty for a comparison to be used.

    \code{get.beta.entropy} assigns class 1 to the original group and
    class 0 to the near-miss group. It then uses the other variables to
    predict group membership with logistic lasso regression. The
    released variable is excluded because its rule condition already
    separates the groups. Variables that help distinguish the groups
    provide evidence of local dependence. Repeating this calculation
    across rules allows the relationships to differ across the data.
  }

  \subsection{Interpreting the importance matrix}{
    In the matrix returned by \code{get.beta.entropy}, rows identify
    released variables and columns identify predictors. For example,
    \code{I["x1", "x2"]} measures the average coefficient magnitude
    for \code{x2} when distinguishing groups formed by releasing
    \code{x1}. The reverse entry answers a different
    question, so the matrix is generally asymmetric.

    Each entry is the mean absolute logistic-lasso coefficient across
    the usable comparisons for that row. The diagonal is zero because
    a variable is excluded from its own release comparison. Predictors
    not selected in a usable comparison contribute zero. Thus
    \deqn{
      I_{sj} = \frac{1}{|\mathcal K_s|}
        \sum_{r \in \mathcal K_s} |\widehat\beta_{srj}|,
      \qquad j \ne s, \qquad I_{ss} = 0,
    }{
      I[s,j] = mean(abs(beta_hat[s,r,j]), r in K_s), j != s;
      I[s,s] = 0.
    }
    where \eqn{\mathcal K_s}{K_s} contains the usable comparisons
    for released variable \eqn{s}.

    A comparison is usable when fitting succeeds and at least one
    finite, nonzero predictor coefficient is obtained. Failed fits and
    intercept-only fits are excluded. Comparisons receive equal weight.
    A row is omitted when none of its comparisons is usable, so the
    matrix can have fewer rows than columns.

    \code{colSums(I)} summarizes each predictor's contributions across
    all release variables. Larger sums indicate larger total
    contributions; column means give the same ranking.
  }

  \subsection{Displaying relationships and selecting candidate signals}{
    \code{sdependent(I)} draws an arrow from variable \code{s} to
    variable \code{j} when \code{I[s, j] >= threshold}. The arrow
    means that \code{j} helps distinguish the groups formed by
    releasing \code{s}. It describes a predictive relationship, not
    a causal direction. Arrows in both directions and cycles are
    allowed.

    Increase \code{threshold} to show fewer connections or decrease
    it to include weaker connections. The cutoff applies to the
    coefficient magnitudes in \code{I}. The graph is based on the
    adjacency matrix
    \deqn{
      A_{sj} = \mathbf{1}\{I_{sj} \ge t\}, \quad s \ne j,
      \qquad A_{ss} = 0,
    }{
      A[s,j] = 1 if I[s,j] >= threshold and s != j; otherwise 0.
    }
    where \eqn{t} is \code{threshold}. Rows are matched to columns
    by variable name, with zero rows added for missing release variables.
    Variables with no connections are removed from the graph.

    Candidate signals are selected using both connectivity and overall
    importance. With the default directed graph, a variable must have
    at least \code{min.degree} outgoing arrows and a column-sum score
    at or above the \code{q.signal} quantile among variables remaining
    in the graph. The defaults require one outgoing arrow and a score
    in the upper quartile, including ties.

    These two summaries describe different roles. Out-degree counts
    predictors selected when a variable is released; column-sum
    importance measures that variable's contributions when other
    variables are released. In-degree counts the incoming arrows.
    Column sums use all importance values, including those below the
    edge cutoff, and are calculated before isolated variables are
    removed. Consequently, changing \code{threshold} can also change
    the set of variables used to determine the importance quantile.

    Candidate signal vertices are blue and other vertices are gray.
    Directed arrows are blue when their source is a candidate signal.
    Vertex sizes increase with column-sum importance, and arrow widths
    increase with their importance entries. Specifically, the size is
    \code{6 + 2 * log1p(imp.score)} and an edge with value \eqn{w}
    has width \eqn{1 + 3w/w_{\max}}{1 + 3*w/w_max}; widths are
    rescaled within each plot. Use \code{plot = FALSE} to obtain the
    graph summaries without drawing it.

    The population goal is a signal set that accounts for dependence
    among the remaining variables (Zhou et al., 2026). Redundant or
    interchangeable variables can give more than one such set.
    The graph's signal designations are exploratory candidates.
  }

  \subsection{Choosing a forest and preparing the data}{
    The \code{method} argument chooses how the forest forms its rules:
    \describe{
      \item{\code{"auto"}}{The default uses copies of the variables
        as multiple regression responses, with the variables themselves
        as predictors. This forest autoencoder forms groups by learning
        to reproduce the observed variable values.}
      \item{\code{"unsupv"}}{Uses unsupervised splitting, with some
        variables temporarily serving as responses. \code{ytry}
        controls how many are used.}
      \item{\code{"rnd"}}{Uses random splitting with an independently
        generated Gaussian response.}
    }
    All three methods use the same subsequent rule comparisons and
    lasso analysis. \code{ntree} controls the number of trees grown;
    \code{max.tree} and \code{max.rules.tree} limit rule extraction.
    Larger \code{nodesize} values form broader groups of observations.

    Rows with missing values are omitted. Categorical variables are
    converted to numeric indicator columns, called hot-encoding.
    The processed data are stored in \code{o$x}. All membership
    indices refer to these rows, and each processed column is studied
    separately. A factor can therefore contribute several rows or
    columns to the importance matrix and several graph vertices.
  }

  \subsection{Preliminary importance and screening}{
    \code{importance(o)} summarizes the preliminary scores calculated
    by \code{uvarpro}. The default rule score compares how much the
    variables vary within the original and near-miss groups with how
    much they vary when the groups are pooled. If \eqn{d(M)} is the
    average column standard deviation of a matrix \eqn{M}, the score is
    \deqn{
      H = \frac{d(X_C) + d(X_O)}{2 d(X_C \cup X_O)}.
    }{
      H = (d(X_C) + d(X_O)) / (2 * d(rbind(X_C, X_O))).
    }
    The union denotes row concatenation. Before scoring, each variable
    is divided by its full-data standard deviation without centering;
    near-zero standard deviations are replaced by one. All variables,
    including the released variable, enter this calculation.

    By default, \code{get.beta.entropy} limits both release variables
    and lasso predictors to those with preliminary importance above
    \code{vimp.min}. Set \code{pre.filter = FALSE} to make all
    processed variables available to the lasso analysis. The filter
    is skipped if the preliminary importance calculation fails.
  }

  \subsection{Lasso controls, scaling, and the second stage}{
    Each local classification uses binomial lasso regression from
    \pkg{glmnet}, with \eqn{\alpha = 1}{alpha = 1}. By default,
    cross-validation chooses a separate penalty for each comparison
    using binomial deviance. \code{lambda.sel = "lambda.1se"} uses
    the largest penalty within one standard error of the minimum;
    \code{"lambda.min"} uses the penalty with minimum error. Set
    \code{use.cv = FALSE} to use the last, smallest penalty on the
    computed path instead. \code{nlambda} and \code{lambda.min.ratio}
    control that path.

    Before fitting, \code{scale(X, center = FALSE)} divides each
    predictor by \eqn{\sqrt{\sum_i X_{ij}^2/(m-1)}}{sqrt(sum(X[,j]^2)/(m-1))},
    where \eqn{m} is the local sample size. This measures spread
    around zero rather than around the local mean. \code{glmnet}
    also uses its default internal standardization and returns
    coefficients on the scale of the matrix supplied to it. Predictors
    that are constant within a comparison are excluded from that fit;
    their contributions are zero in the importance matrix. A comparison
    can use a single varying predictor.

    With \code{second.stage = TRUE}, the variables selected by the
    logistic lasso enter a second local regression. This time the
    released variable is the response. The regression uses the pooled
    observations from both groups and a Gaussian lasso, with the same
    predictor scaling and response values from \code{o$x}. It uses
    the last computed penalty, controlled by \code{nlambda.lasso}
    and \code{lambda.min.ratio.lasso}.

    The absolute second-stage coefficients are averaged over the
    comparisons used by the logistic stage. Unselected predictors,
    failed second-stage fits, and second-stage fits selecting no
    predictors contribute zeros. Each row is then divided by the
    released variable's full-data standard deviation:
    \deqn{
      I^{(2)}_{sj} =
      \frac{1}{\widehat\sigma_s |\mathcal K_s|}
      \sum_{r \in \mathcal K_s} |\widehat\gamma_{srj}|.
    }{
      I2[s,j] = mean(abs(gamma_hat[s,r,j]), r in K_s) / sd(o$x[,s]).
    }
    A zero or nonfinite response standard deviation gives \code{NA}
    for that row. \code{get.beta.entropy} returns this second-stage
    matrix instead of the logistic matrix. Choose the graph cutoff
    for the scale of the matrix being displayed.

    \code{papply = lapply} runs the release-variable analyses in
    sequence. \code{parallel} separately controls the cross-validation
    folds. Avoid using more simultaneous workers than the available
    cores when enabling both levels of parallelism. See Note for
    reproducibility.
  }

  \subsection{The latent-variable example}{
    The example uses four independent standard normal source variables,
    \code{w}, \code{x}, \code{y}, and \code{z}. Each has five noisy
    copies with independent Gaussian errors of variance 0.1. Two
    additional variables are \code{h1 = w + x + e21} and
    \code{h2 = y + z + e22}, with independent Gaussian errors of
    variance 0.4. All errors are independent of the sources, and all
    26 variables are supplied to \code{uvarpro}.

    The four sources form the signal set: conditional on them, the
    other 22 variables are mutually independent. Releasing a noisy
    copy of \code{w} has \code{w} as its \eqn{s}-dependent signal;
    the other copy blocks have their respective sources. Releasing
    \code{h1} has signals \code{w} and \code{x}, while releasing
    \code{h2} has signals \code{y} and \code{z}.

    Conversely, when \code{w} is released, its copies and \code{h1}
    can help distinguish the groups; \code{x} can also contribute
    through its component of \code{h1}. The graph can therefore have
    arrows in both directions. Repeated contributions across releases
    motivate the column-sum ranking. The example uses
    \code{pre.filter = FALSE} to include every variable and compares
    the estimates with this known dependence structure.
  }

  \subsection{Custom scoring functions}{
    Supply \code{entropy} through \code{...} to replace the preliminary
    scoring function. It must accept \code{xC}, \code{xO}, and
    \code{...}. The first two arguments contain the scaled near-miss
    and original-group data, with the released variable first.
    Additional information includes \code{compMembership},
    \code{oobMembership}, the processed \code{data}, and
    \code{xvar.names}.

    Return a numeric rule score, or a list with the score first and
    additional information second. To use \code{get.beta.entropy}
    afterwards, the second element must provide the memberships as
    \code{list(comp = compMembership, oob = oobMembership)}, in that
    order. The custom-scoring example reproduces the default score
    while preserving these memberships.
  }
}

\value{
  \code{uvarpro} returns an object of class \code{"uvarpro"}. Its
  principal components are:
  \describe{
    \item{rf}{The forest used to generate rules.}
    \item{results}{Rule-level information with columns \code{tree},
      \code{branch}, \code{variable}, \code{n.oob}, and \code{imp}.
      The variable index refers to \code{xvar.names}. Unevaluated
      comparisons have \code{NA} importance.}
    \item{x}{The complete-case, hot-encoded feature data, before the
      scaling used in scoring or local regressions.}
    \item{xvar.names, xvar.org.names}{Processed and original feature
      names, respectively.}
    \item{entropy}{With the default scoring function, membership pairs
      grouped by released variable. Each pair contains \code{comp}
      and \code{oob} observation indices into \code{x}. May be
      \code{NULL} when no usable memberships are available. Custom
      callbacks can change the auxiliary information stored here.}
    \item{max.rules.tree, max.tree}{Rule extraction limits.}
    \item{family}{The value \code{"unsupv"}, irrespective of the
      forest construction method.}
  }

  \code{get.beta.entropy} returns a named numeric matrix, with released
  variables as rows and eligible predictors as columns. Entries are
  mean absolute logistic-lasso coefficients when
  \code{second.stage = FALSE}, and response-scaled mean absolute
  linear-lasso coefficients when \code{second.stage = TRUE}. Rows
  with no retained comparisons are omitted. Returns \code{NULL} if
  no usable release rows remain.

  \code{sdependent} invisibly returns a list with components:
  \describe{
    \item{signal.vars}{Names of vertices meeting both the degree and
      importance-quantile conditions. Can be empty even when the
      graph is nonempty.}
    \item{imp.score}{Named global column-sum scores for nonisolated
      vertices, sorted in increasing order.}
    \item{degree}{Named degrees for nonisolated vertices, in graph
      vertex order. These are out-degrees when \code{directed = TRUE}.}
  }
  If no edges remain, \code{sdependent} returns a character diagnostic.
  See Note.
}

\note{
  For reproducible serial lasso calculations, call \code{set.seed}
  immediately before \code{get.beta.entropy(o, papply = lapply)} to
  control its randomized cross-validation folds. Also call
  \code{set.seed} before \code{uvarpro} to control R-level sampling
  in forest construction and rule extraction. The \code{seed} argument
  supplies the forest seed but does not replace these R-level seeds.

  Small local classes, degenerate predictors, and excessive preliminary
  filtering can leave no usable comparisons.
  A failed local lasso fit is excluded from the coefficient average.

  For an undirected display, supply a symmetric importance matrix.
  For square input with matching row and column order,
  \code{pmax(I, t(I))} retains either direction. Symmetrization can
  change global scores and signal designations. Undirected edge
  colors use the first endpoint in the internal edge list.

  To admit weaker positive edges in an empty graph, decrease
  \code{threshold}. Keep the cutoff positive, since \code{>=} admits
  zero-valued entries at a zero cutoff. Check for \code{NULL} lasso
  output before graph construction.
}

\author{
  Min Lu and Hemant Ishwaran
}

\references{
  Tang F. and Ishwaran H. (2017). Random forest missing data
  algorithms. \emph{Statistical Analysis and Data Mining}, 10:363-377.

  Zhou L., Lu M. and Ishwaran H. (2026). Variable priority for
  unsupervised variable selection. \emph{Pattern Recognition},
  172:112727. \doi{10.1016/j.patcog.2025.112727}.
}

\seealso{
  \code{\link{varpro}}, \code{\link{importance}},
  \code{\link[glmnet:cv.glmnet]{cv.glmnet}},
  \code{\link[glmnet:glmnet]{glmnet}},
  \code{\link[base:scale]{scale}},
  \code{\link[igraph:graph_from_adjacency_matrix]{graph_from_adjacency_matrix}}
}

\examples{
## Small forest for a quick check
set.seed(1)
o <- uvarpro(mtcars, ntree = 1)

\donttest{
## ------------------------------------------------------------
## Latent-variable model: known signals and s-dependence
## ------------------------------------------------------------
set.seed(21)
n <- 1000
w <- rnorm(n)
x <- rnorm(n)
y <- rnorm(n)
z <- rnorm(n)
ei <- matrix(rnorm(n * 20, sd = sqrt(.1)), ncol = 20)
e21 <- rnorm(n, sd = sqrt(.4))
e22 <- rnorm(n, sd = sqrt(.4))
wi <- w + ei[, 1:5]
xi <- x + ei[, 6:10]
yi <- y + ei[, 11:15]
zi <- z + ei[, 16:20]
h1 <- w + x + e21
h2 <- y + z + e22
dta <- data.frame(w = w, wi = wi, x = x, xi = xi,
                  y = y, yi = yi, z = z, zi = zi, h1 = h1, h2 = h2)
signal <- c("w", "x", "y", "z")

## All 26 columns are observed features. The remaining 22 are
## mutually independent conditional on the four signal variables.
o <- uvarpro(dta)
print(importance(o))

## Retain every feature for the local lasso analysis, including
## the noisy-copy and mixed-variable releases that reveal signals.
set.seed(22)
beta <- get.beta.entropy(o, pre.filter = FALSE,
                         papply = lapply, nfolds = 5)

if (!is.null(beta)) {
  ## Compare the column-sum ranking with the known signal set.
  score <- sort(colSums(beta), decreasing = TRUE)
  print(data.frame(variable = names(score),
                   importance = unname(score),
                   signal = names(score) \%in\% signal))

  ## Inspect one copy from each block and the two mixed variables.
  ## Population targets: each copy points to its source; h1 points
  ## to w and x, and h2 to y and z. See Details for source releases.
  release <- c("wi.1", "xi.1", "yi.1", "zi.1", "h1", "h2")
  release <- intersect(release, rownames(beta))
  print(round(beta[release, signal, drop = FALSE], 2))

  ## Inspect the corresponding adjacency block.
  ## A[s, j] = 1 gives the arrow s -> j.
  threshold <- 0.25
  A <- 1L * (beta[release, signal, drop = FALSE] >= threshold)
  print(A)

  if (all(is.finite(beta)) &&
      requireNamespace("igraph", quietly = TRUE)) {
    set.seed(23)
    ## Pass the lasso matrix directly; row alignment is automatic.
    graph.info <- sdependent(beta, threshold = threshold, layout = "fr",
                              title = "Latent model: s-dependence")
    if (is.list(graph.info)) {
      print(graph.info$signal.vars)
      print(graph.info$degree)
    } else {
      print(graph.info)
    }

    ## Return graph summaries without drawing the figure:
    ## graph.info <- sdependent(beta, threshold = threshold, plot = FALSE)
  }
}

## Optional second-stage lasso, keeping the same feature set.
## set.seed(22)
## beta.second <- get.beta.entropy(o, pre.filter = FALSE,
##                                  second.stage = TRUE,
##                                  papply = lapply, nfolds = 5)

## ------------------------------------------------------------
## Boston housing: alternative forest and categorical features
## ------------------------------------------------------------
if (requireNamespace("mlbench", quietly = TRUE)) {
  data(BostonHousing, package = "mlbench")

  ## Every column is treated as a feature; no response is specified.
  o.unsupv <- uvarpro(BostonHousing, method = "unsupv")
  print(importance(o.unsupv))

  ## Random splitting uses the same subsequent analysis.
  ## o.random <- uvarpro(BostonHousing, method = "rnd")

  Boston <- BostonHousing
  Boston$zn <- factor(Boston$zn)
  Boston$chas <- factor(Boston$chas)
  Boston$lstat <- factor(round(0.2 * Boston$lstat))
  Boston$nox <- factor(round(20 * Boston$nox))
  Boston$rm <- factor(round(Boston$rm))

  o.factor <- uvarpro(Boston)
  print(importance(o.factor))
  print(get.orgvimp(o.factor))
  print(get.orgvimp(o.factor, pretty = FALSE))
}

## ------------------------------------------------------------
## Iowa housing: larger lasso analysis
## ------------------------------------------------------------
data(housing, package = "randomForestSRC")
iowa <- roughfix(housing)
## Numeric coding below is only a speed-oriented illustration;
## retain factors to use the usual hot-encoding instead.
iowa <- data.frame(data.matrix(iowa))
o.iowa <- uvarpro(iowa, ntree = 50, max.tree = 50,
                  max.rules.tree = 5)
set.seed(22)
beta.iowa <- get.beta.entropy(o.iowa, papply = lapply, nfolds = 5)
if (!is.null(beta.iowa)) {
  print(sort(colMeans(beta.iowa), decreasing = TRUE))
  ## sdependent(beta.iowa)
}

## ------------------------------------------------------------
## Custom scoring callback: reproduces the default dispersion ratio
## ------------------------------------------------------------
my.entropy <- function(xC, xO, ...) {
  mean.sd <- function(x) mean(apply(x, 2, sd, na.rm = TRUE))
  imp <- 0.5 * (mean.sd(xC) + mean.sd(xO)) /
    mean.sd(rbind(xC, xO))
  dots <- list(...)
  list(imp = imp,
       membership = list(comp = dots$compMembership,
                         oob = dots$oobMembership))
}

if (requireNamespace("mlbench", quietly = TRUE)) {
  data(BostonHousing, package = "mlbench")
  o.custom <- uvarpro(BostonHousing, entropy = my.entropy,
                      ntree = 50, max.tree = 50, max.rules.tree = 5)
  print(importance(o.custom))
  ## The stored memberships also allow the lasso analysis.
  ## beta.custom <- get.beta.entropy(o.custom, papply = lapply)
}
}
}

\keyword{multivariate}
\keyword{graphs}
