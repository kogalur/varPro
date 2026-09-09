\name{uvarpro}
\alias{uvarpro}
\alias{get.beta.entropy}
\alias{sdependent}

\title{Unsupervised Variable Selection and Dependence Graphs using UVarPro}

\description{
  Performs unsupervised variable selection using the Variable Priority
  framework. \code{uvarpro} constructs forest rules and region-release
  contrasts from unlabeled data. \code{get.beta.entropy} uses local lasso
  regressions to summarize which variables distinguish these contrasts.
  \code{sdependent} displays the resulting importance matrix as a
  variable-dependence graph and identifies candidate signal variables.
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

  \item{method}{Forest construction method. \code{"auto"} uses a
    multivariate forest autoencoder, \code{"unsupv"} uses unsupervised
    splitting, and \code{"rnd"} uses random splitting.}

  \item{ntree}{Number of trees to grow.}

  \item{nodesize}{Minimum terminal node size. If \code{NULL}, the
    requested size is \code{n / 10} for \code{n < 100}, and
    \code{max(n / 10, 20)} otherwise, where \code{n} is the number of
    complete observations.}

  \item{max.rules.tree}{Maximum number of rules sampled per tree for
    the region-release analysis.}

  \item{max.tree}{Maximum number of trees used to extract rules.}

  \item{verbose}{Currently unused.}

  \item{seed}{Seed argument for \code{uvarpro}. Seed the lasso
    cross-validation separately; see Note.}

  \item{...}{Additional named forest arguments recognized by
    \code{rfsrc}, such as \code{mtry} or \code{ytry}, and a custom
    scoring function supplied as \code{entropy}; see Details.
    Supply lasso controls to \code{get.beta.entropy}.}

  \item{o}{An object returned by \code{uvarpro}, containing the
    processed feature data in \code{x} and the region membership lists
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
    \code{mclapply} is resolved through the package parallel backend
    using \code{getOption("mc.cores", 1L)}. The \code{parallel}
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
    distinguish its region-release contrasts. For an undirected
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
  \subsection{Overview and forest construction}{
    UVarPro studies dependence among features in unlabeled data. The
    population framework of Zhou et al. (2026) seeks a signal set that
    accounts for dependence among the remaining variables. Redundant
    or interchangeable variables can give rise to multiple signal sets.

    The analysis has three steps: \code{uvarpro} constructs local
    region-release comparisons, \code{get.beta.entropy} estimates
    a lasso coefficient importance matrix, and \code{sdependent}
    displays its dependence graph.

    For \code{method = "auto"}, copies of the processed features
    serve as the multivariate response and the original features
    serve as predictors in \code{rfsrc}. Regressing the feature vector
    on itself produces a forest autoencoder that partitions feature
    space.

    For \code{method = "unsupv"}, the forest uses unsupervised
    splitting with internally selected pseudo-responses. The default
    \code{ytry} is \code{min(ceiling(sqrt(p)), p - 1)}, where \code{p}
    is the number of processed features. For \code{method = "rnd"},
    an independent Gaussian response is generated and
    \code{splitrule = "random"} is used. All three methods use the
    same subsequent region-release and lasso calculations.

    Complete observations are hot-encoded and saved as \code{o$x}.
    Membership indices refer to these observations; lasso rows and
    columns and graph vertices refer to the processed features.
    Each encoded column is analyzed separately.
  }

  \subsection{Rules, release regions, and local classification}{
    A tree rule defines a region \eqn{R} through restrictions on
    feature values. Releasing variable \eqn{s} removes its restrictions
    while retaining the others. Write the enlarged region as
    \eqn{R^{(s)}}{R^(s)} and its complementary, or near-miss, region as
    \deqn{C_s = R^{(s)} \setminus R.}{C_s = R^(s) minus R.}
    The complementary region contains observations admitted by
    releasing \eqn{s} that still satisfy the other rule conditions.

    \code{uvarpro} extracts original-region out-of-bag membership
    and complementary-region membership. Comparisons are retained
    when both sets are nonempty. The default \code{o$entropy} stores
    these index pairs by released variable, with \code{comp} first
    and \code{oob} second.

    For each comparison, \code{get.beta.entropy} combines the
    complementary observations (class 0) and original observations
    (class 1). It predicts this membership label using the eligible
    features other than the released variable. Excluding the released
    variable prevents the classifier from simply recovering the rule
    restriction that separates the groups. The remaining predictors
    identify features that distinguish the local contrast. Repeating
    this analysis across rules allows different predictors to
    contribute in different parts of feature space.
  }

  \subsection{Preliminary dispersion importance}{
    The default entropy function computes a dispersion ratio. Let
    \eqn{d(M)} be the average column standard deviation of a feature
    matrix \eqn{M}. For the original and complementary feature matrices
    \eqn{X_O} and \eqn{X_C}, the stored rule score is
    \deqn{
      H = \frac{d(X_C) + d(X_O)}{2 d(X_C \cup X_O)}.
    }{
      H = (d(X_C) + d(X_O)) / (2 * d(rbind(X_C, X_O))).
    }
    Here the union denotes row concatenation. The ratio compares
    average within-group dispersion with pooled dispersion. Before
    scoring, features are divided by their full-data standard
    deviations without centering; near-zero standard deviations are
    replaced by one. All features, including the released variable,
    enter this calculation.

    \code{importance(o)} summarizes the rule scores. By default,
    \code{get.beta.entropy} uses preliminary importance to screen
    predictors and release variables before the local regressions.
    Set \code{pre.filter = FALSE} to include all processed features.
  }

  \subsection{Logistic lasso and the importance matrix}{
    Each local classification problem uses binomial lasso logistic
    regression from \pkg{glmnet}, with penalty parameter
    \eqn{\alpha = 1}{alpha = 1}. With \code{use.cv = TRUE},
    \code{cv.glmnet} selects a penalty separately for each comparison
    using binomial deviance; the default choice is \code{lambda.1se}.
    With \code{use.cv = FALSE}, the last computed penalty on the
    regularization path is used.

    For released variable \eqn{s} and retained comparison \eqn{r},
    let \eqn{\widehat\beta_{srj}}{beta_hat[s,r,j]} denote the
    coefficient of predictor \eqn{j}. The entries of the importance
    matrix are mean absolute coefficients across retained comparisons.
    The intercept is discarded; unselected predictors and the released
    coordinate receive zero. Writing \eqn{\mathcal K_s}{K_s} for the
    retained comparisons, the default matrix has entries
    \deqn{
      I_{sj} = \frac{1}{|\mathcal K_s|}
        \sum_{r \in \mathcal K_s} |\widehat\beta_{srj}|,
      \qquad j \ne s, \qquad I_{ss} = 0.
    }{
      I[s,j] = mean(abs(beta_hat[s,r,j]), r in K_s), j != s;
      I[s,s] = 0.
    }

    Rows identify released variables and columns identify predictors.
    Thus \code{I[s, j]} measures predictor \code{j}'s contribution
    when variable \code{s} is released. The matrix is generally
    asymmetric. Column sums aggregate each predictor's contributions
    across release tasks; column means give the same ranking.

    A comparison is retained when fitting and coefficient extraction
    succeed and at least one finite, nonzero predictor coefficient
    remains. Failed fits and intercept-only solutions are excluded.
    Retained comparisons receive equal weight, including zeros for
    unselected predictors. A release row is omitted when it has no
    retained comparisons, so predictor columns can outnumber rows.
    The result is \code{NULL} when no rows remain.
  }

  \subsection{Coefficient scaling and the optional second stage}{
    Before each logistic fit, \code{scale(X, center = FALSE)} divides
    each predictor column by
    \eqn{\sqrt{\sum_i X_{ij}^2/(m-1)}}{sqrt(sum(X[,j]^2)/(m-1))},
    where \eqn{m} is the local sample size. \code{glmnet} also applies
    its default internal standardization and returns coefficients on
    the scale of the matrix supplied to it.

    With \code{second.stage = TRUE}, predictors selected by the
    logistic lasso enter a local Gaussian lasso with the released
    variable as its response. This regression uses the combined
    complementary and original observations, the same predictor
    scaling, and the response from \code{o$x}. Coefficients are taken
    at the last computed penalty on the linear-lasso path.

    Absolute second-stage coefficients are averaged over the
    comparisons retained by the logistic stage, with zeros for
    unselected predictors. A second-stage fit that fails or selects
    no predictors contributes zeros to this average. Row \eqn{s} is
    then divided by the full-data standard deviation of the released
    variable:
    \deqn{
      I^{(2)}_{sj} =
      \frac{1}{\widehat\sigma_s |\mathcal K_s|}
      \sum_{r \in \mathcal K_s} |\widehat\gamma_{srj}|.
    }{
      I2[s,j] = mean(abs(gamma_hat[s,r,j]), r in K_s) / sd(o$x[,s]).
    }
    Here \eqn{\widehat\gamma_{srj}}{gamma_hat[s,r,j]} is a
    second-stage coefficient. A zero or nonfinite response standard
    deviation produces \code{NA} in that row.

    \code{get.beta.entropy} returns this matrix when
    \code{second.stage = TRUE}. Choose the graph cutoff on the
    coefficient scale of the selected stage.
  }

  \subsection{Adjacency matrix and interpretation of the graph}{
    In UVarPro, \eqn{s}-dependence concerns which variables remain
    informative about the local contrast induced by releasing
    \eqn{s}. The graph summarizes their estimated contributions.

    The theoretical motivation distinguishes releasing a noise
    variable, whose contrast is explained by associated signal
    variables, from releasing a signal variable, whose contrast can
    also involve associated noisy variables. This asymmetry motivates
    aggregating contributions across release tasks.

    \code{sdependent} aligns rows to column names, adds zero rows
    for missing release variables, and then clears the diagonal.
    Column order determines vertex order before isolated vertices
    are removed. It calculates the column scores
    \eqn{G_j = \sum_s I_{sj}}{G[j] = sum(I[,j])} and constructs
    the binary adjacency matrix
    \deqn{
      A_{sj} = \mathbf{1}\{I_{sj} \ge t\}, \quad s \ne j,
      \qquad A_{ss} = 0,
    }{
      A[s,j] = 1 if I[s,j] >= threshold and s != j; otherwise 0.
    }
    where \eqn{t} is \code{threshold}. Each retained connection is
    one edge, with its plotted width determined by the corresponding
    importance entry.

    With \code{directed = TRUE}, \eqn{A_{sj} = 1}{A[s,j] = 1}
    creates the arrow \eqn{s \longrightarrow j}{s -> j}: predictor
    \eqn{j} contributes to distinguishing the comparisons formed by
    releasing \eqn{s}. Arrows describe predictive contributions.
    Reciprocal arrows and cycles are allowed.

    The out-degree of \eqn{s} counts thresholded entries in its row,
    giving the number of predictors connected to its release tasks.
    The in-degree of \eqn{j} counts thresholded entries in its column,
    giving the number of release variables connected to that predictor.
    Global importance \eqn{G_j} sums the unthresholded magnitudes.

    Isolated vertices are removed. Global scores are calculated before
    thresholding and removal, so they include subthreshold contributions
    and contributions from rows whose vertices are later removed.
    The signal-designation quantile uses the remaining vertices.
  }

  \subsection{Signal designation and graphical appearance}{
    A retained vertex enters \code{signal.vars} when its degree is
    at least \code{min.degree} and its global importance is at least
    the \code{q.signal} quantile of retained scores. For directed
    graphs, these conditions use out-degree and column-sum importance.
    The defaults require at least one outgoing edge and importance
    at or above the upper quartile of nonisolated vertices, including
    ties.

    Signal vertices are blue; other retained vertices are gray.
    A directed edge is blue when its source is designated as signal
    and gray otherwise. Vertex size is
    \code{6 + 2 * log1p(imp.score)}. An edge with importance \eqn{w}
    has width \eqn{1 + 3w/w_{\max}}{1 + 3*w/w_max}, where
    \eqn{w_{\max}}{w_max} is the largest plotted edge value. Widths
    are rescaled for each plot.

    \code{threshold} controls the displayed connections;
    \code{q.signal} and \code{min.degree} control vertex highlighting.
    Changing the edge cutoff can also change the quantile reference
    set through vertex removal. Signal designations are exploratory.
  }

  \subsection{Latent-variable example}{
    The simulation in Examples contains four independent standard
    normal variables \code{w}, \code{x}, \code{y}, and \code{z}, each
    accompanied by five noisy copies with independent Gaussian errors
    of variance 0.1. Two further features are
    \eqn{h_1 = w + x + \epsilon_{21}}{h1 = w + x + e21} and
    \eqn{h_2 = y + z + \epsilon_{22}}{h2 = y + z + e22}, with
    independent Gaussian errors of variance 0.4. All errors are
    independent of the four source variables. All 26 features,
    including the sources, are supplied to \code{uvarpro}.

    The signal set is \eqn{\{w, x, y, z\}}{\{w, x, y, z\}}:
    conditioning on these four variables makes the other 22 features
    mutually independent. Releasing any of \code{wi.1}, \ldots,
    \code{wi.5} has \code{w} as its \eqn{s}-dependent signal;
    the \code{xi}, \code{yi}, and \code{zi} blocks similarly point
    to \code{x}, \code{y}, and \code{z}. Releasing \code{h1} has
    \eqn{s}-dependent signals \code{w} and \code{x}, while releasing
    \code{h2} has \code{y} and \code{z}.

    When a source such as \code{w} is released, its noisy copies and
    \code{h1} can help predict local membership; \code{x} can also
    contribute by accounting for its component of \code{h1}. The
    graph can therefore contain connections in both directions.
    Repeated contributions across the noisy-feature releases motivate
    the column-sum ranking of the four signals.

    The example uses \code{pre.filter = FALSE} to keep all 26 features
    eligible as predictors and release variables. It compares the
    estimated importance entries and graph connections with this known
    dependence structure.
  }

  \subsection{Custom scoring functions}{
    A custom \code{entropy} function accepts \code{xC}, \code{xO},
    and \code{...}. The first two arguments contain the scaled
    complementary and original features, with the released feature
    first. Additional information includes \code{compMembership},
    \code{oobMembership}, the processed \code{data}, and
    \code{xvar.names}.

    Return a numeric rule score, or a list containing the score first
    and auxiliary information second. For subsequent use of
    \code{get.beta.entropy}, supply
    \code{list(comp = compMembership, oob = oobMembership)} as the
    second element, preserving that order. See Examples.
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
  control its randomized cross-validation folds. Seed forest
  construction separately. Small local classes, degenerate predictors,
  and excessive pre-filtering can leave no usable comparisons.
  Individual lasso errors are caught and warnings suppressed.

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
  ## you can still run the lasso if you want
  ## beta.custom <- get.beta.entropy(o.custom, papply = lapply)
}
}
}

\keyword{multivariate}
\keyword{graphs}
