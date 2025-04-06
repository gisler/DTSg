`DTSg` offers basic time series data functionalities such as listing of missing values, application of arbitrary aggregation as well as rolling (asymmetric) window functions and automatic detection of periodicity. As it is mainly based on `data.table`, it is fast and (in combination with the `R6` package) offers reference semantics. In addition to its native R6 interface, it provides an S3 interface for those who prefer the latter. Finally yet importantly, its functional approach allows for incorporating functionalities from many other packages.

## Installation

Install the latest release from CRAN:

`install.packages("DTSg")`

[![CRAN Version](https://www.r-pkg.org/badges/version/DTSg)](https://cran.r-project.org/package=DTSg) [![CRAN Checks](https://badges.cranchecks.info/worst/DTSg.svg)](https://cran.r-project.org/web/checks/check_results_DTSg.html)

Install the [development version](https://gisler.github.io/DTSg/dev/) from GitHub (requires the `remotes` package):

`remotes::install_github("gisler/DTSg")`

[![R build status](https://github.com/gisler/DTSg/workflows/R-CMD-check/badge.svg)](https://github.com/gisler/DTSg/actions?query=workflow%3AR-CMD-check) [![codecov](https://codecov.io/gh/gisler/DTSg/branch/main/graph/badge.svg?token=AYXU0fdU9T)](https://app.codecov.io/gh/gisler/DTSg)

## Semantic versioning

Releases of this project are versioned following the rules of [SemVer](https://semver.org).
