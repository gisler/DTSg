`DTSg` offers basic time series functionalities such as listing of missing values, application of arbitrary aggregation as well as rolling (asymmetric) window functions and automatic detection of periodicity. As it is mainly based on `data.table`, it is fast and – in combination with the `R6` package – offers reference semantics. In addition to its native R6 interface, it provides an S3 interface inclusive an S3 wrapper method generator for those who prefer the latter. Finally yet importantly, its functional approach allows incorporating functionalities from many other packages.

## Installation

Install the latest release from CRAN:

`install.packages("DTSg")`

[![CRAN Version](https://www.r-pkg.org/badges/version/DTSg)](https://cran.r-project.org/package=DTSg) [![CRAN Checks](https://cranchecks.info/badges/summary/DTSg)](https://cran.r-project.org/web/checks/check_results_DTSg.html)

Install the development version from GitHub (requires the `remotes` package):

`remotes::install_github("gisler/DTSg")`

[![R build status](https://github.com/gisler/DTSg/workflows/R-CMD-check/badge.svg)](https://github.com/gisler/DTSg/actions) [![codecov](https://codecov.io/gh/gisler/DTSg/branch/master/graph/badge.svg?token=309UB25J9G)](https://codecov.io/gh/gisler/DTSg) [![CodeFactor](https://www.codefactor.io/repository/github/gisler/dtsg/badge)](https://www.codefactor.io/repository/github/gisler/dtsg)

## Getting Started

[A. Basic Usage](https://CRAN.R-project.org/package=DTSg/vignettes/a_basicUsage.html)

[B. Advanced Usage](https://CRAN.R-project.org/package=DTSg/vignettes/b_advancedUsage.html)
