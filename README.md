varPro - Model independant variable selection via rule based variable priority
========================================================
<!-- badges: start -->
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.11526.svg)](https://doi.org/10.5281/zenodo.11526)
[![cranlogs](http://cranlogs.r-pkg.org/badges/varpro)](http://cranlogs.r-pkg.org/badges/varpro)

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/varpro)](https://cran.r-project.org/package=varpro)
[![active](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/badges/latest/active.svg)
[![R-CMD-check](https://github.com/kogalur/varpro/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/kogalur/varpro/actions/workflows/R-CMD-check.yaml)

[![Codecov test coverage](https://codecov.io/gh/kogalur/varpro/graph/badge.svg)](https://app.codecov.io/gh/kogalur/varpro)
<!-- badges: end -->


While achieving high prediction accuracy is a fundamental goal in machine learning, an equally important task is finding a small number of features with high explanatory power. One popular selection technique is permutation importance, which assesses a variable’s impact by measuring the change in prediction error after permuting the variable. 

Variable Priority (VarPro) works by utilizing rules without the need to generate artificial data or evaluate prediction error. The method only requires the calculation of sample averages of simple statistics, and can be applied to many data settings, including regression, classification, and survival.



