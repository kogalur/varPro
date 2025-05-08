\name{gliomas}
\docType{data}
\alias{glioma}
\title{Diffuse Adult Glioma}

\description{

Subset of the data used in Ceccarelli et al. (2016) for molecular
profiling of adult diffuse gliomas.  As part of the analysis, the
authors developed a supervised analysis using DNA methylation data.
Their original dataset was collected from a core set of 25,978 CpG
probes which was reduced to eliminate sites that were methylated.  This
reduced set of 1206 probes from 880 tissues makes up part of the
features of this data.  Also included are clinical data and other
molecular data collected for the samples.  The outcome is a supervised
class label developed in the study with labels: Classic-like,
Codel, G-CIMP-high, G-CIMP-low, LGm6-GBM, Mesenchymal-like and PA-like.

}
\references{

Ceccarelli, M., Barthel, F.P., Malta, T.M., Sabedot, T.S., Salama, S.R.,
Murray, B.A., Morozova, O., Newton, Y., Radenbaugh, A., Pagnotta, S.M.
et al. (2016). Molecular profiling reveals biologically discrete subsets
and pathways of progression in diffuse glioma. Cell, 164, 550-563.

}
\examples{
\donttest{
data(glioma, package = "varPro")
o <- varpro(y~., glioma, nodesize=2, max.tree=250)
imp <- importance(o)
print(head(imp$unconditional))
print(imp$conditional.z)
}}

\keyword{datasets}
