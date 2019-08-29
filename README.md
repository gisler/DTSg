`DTSg` offers basic time series functionalities such as listing of missing values, application of arbitrary aggregation as well as rolling window functions and automatic detection of periodicity. As it is mainly based on `data.table`, it is fast and – in combination with the `R6` package – offers reference semantics. In addition to its native R6 interface, it provides an S3 interface inclusive an S3 wrapper method generator for those who prefer the latter.

## Installation

Install the latest release from CRAN:

`install.packages("DTSg")`

[![CRAN Version](https://www.r-pkg.org/badges/version/DTSg)](https://cran.r-project.org/package=DTSg)

Install the development version from GitHub (requires the `devtools` package):

`devtools::install_github("gisler/DTSg")`

[![Build Status](https://travis-ci.org/gisler/DTSg.svg?branch=master)](https://travis-ci.org/gisler/DTSg)

## Getting Started

[A. Basic Usage](https://CRAN.R-project.org/package=DTSg/vignettes/a_basicUsage.html)

[B. Advanced Usage](https://CRAN.R-project.org/package=DTSg/vignettes/b_advancedUsage.html)
