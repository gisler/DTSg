`DTSg` offers basic time series data functionalities such as listing of missing values, application of arbitrary aggregation as well as rolling (asymmetric) window functions and automatic detection of periodicity. As it is mainly based on `data.table`, it is fast and – in combination with the `R6` package – offers reference semantics. In addition to its native R6 interface, it provides an S3 interface inclusive an S3 wrapper method generator for those who prefer the latter. Finally yet importantly, its functional approach allows incorporating functionalities from many other packages.

## Installation

Install the latest release from CRAN:

`install.packages("DTSg")`

[![CRAN Version](https://www.r-pkg.org/badges/version/DTSg)](https://cran.r-project.org/package=DTSg) [![CRAN Checks](https://cranchecks.info/badges/summary/DTSg)](https://cran.r-project.org/web/checks/check_results_DTSg.html)

Install the development version from GitHub (requires the `remotes` package):

`remotes::install_github("gisler/DTSg")`

[![R build status](https://github.com/gisler/DTSg/workflows/R-CMD-check/badge.svg)](https://github.com/gisler/DTSg/actions?query=workflow%3AR-CMD-check) [![codecov](https://codecov.io/gh/gisler/DTSg/branch/master/graph/badge.svg?token=RgpKmhb899)](https://codecov.io/gh/gisler/DTSg) [![GitHub Super-Linter](https://github.com/gisler/DTSg/workflows/Lint%20Code%20Base/badge.svg)](https://github.com/gisler/DTSg/actions?query=workflow%3A%22Lint+Code+Base%22)

## Semantic versioning

Releases of this project are versioned following the rules of [SemVer](https://semver.org) with one exception: The public API of major version zero (0.y.z) should be considered stable.
