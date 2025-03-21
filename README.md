varPro - Model independent variable selection via rule based variable priority
========================================================
<!-- badges: start -->

[![cranlogs](http://cranlogs.r-pkg.org/badges/varpro)](http://cranlogs.r-pkg.org/badges/varpro)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/varpro)](https://cran.r-project.org/package=varpro)

[![active](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/badges/latest/active.svg)

[![R-CMD-check](https://github.com/kogalur/varPro/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/kogalur/varPro/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/kogalur/varPro/graph/badge.svg)](https://app.codecov.io/gh/kogalur/varPro)
<!-- badges: end -->

A new framework of variable selection, which instead of generating artificial covariates such as permutation importance and knockoffs, creates release rules to examine the affect on the response for each covariate where the conditional distribution of the response variable can be arbitrary and unknown.

While achieving high prediction accuracy is a fundamental goal in machine learning, an equally important task is finding a small number of features with high explanatory power. One popular selection technique is permutation importance, which assesses a variable’s impact by measuring the change in prediction error after permuting the variable. 

Variable Priority (VarPro) works by utilizing rules without the need to generate artificial data or evaluate prediction error. The method only requires the calculation of sample averages of simple statistics, and can be applied to many data settings, including regression, classification, and survival.

## Installation

Install the development version into your R environment using the `devtools` package:
```{r}
install.packages("devtools") # If you don't have it.
devtools::install_github("kogalur/varpro")
```
## References

Lu M., Ishwaran H. Model-independent variable selection via the rule-based variable priority [arXiv:2409.09003 ](https://arxiv.org/abs/2409.09003) **[stat.ML]**
