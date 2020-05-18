## DTSg v0.4.1.9000

* Added `funby` and `ignoreDST` arguments to colapply: allows for applying functions like `cumsum` to a certain temporal level
* `timezone` field can now be actively set in order to convert the time zone of the series
* Slightly improved vignettes and documentation
* Minor internal code improvements

## DTSg v0.4.1

* Fixed error "Error in as.POSIXct.numeric(e) : 'origin' must be supplied" related to `rollback` in upcoming R 4.1.0
* Slightly improved vignettes
* Minor internal code improvements

## DTSg v0.4.0

* Added `memoryOverCPU` argument to `rollapply` method: allows for preferring CPU over memory usage, which makes the method more flexible in terms of resource consumption
* Settings of option `DTSgClone` in e.g. *.RProfile* are now respected and not overwritten by `TRUE` when the package is loaded
* Fixed useless coercion of `POSIXct` *.dateTime* columns to `POSIXct` upon object creation in case they contained at least one `NA` value
* Switched to unit testing framework of the `tinytest` package instead of the `testthat` package
* Slightly improved vignettes and documentation
* Minor internal code improvements

## DTSg v0.3.0

* Added `resultCols` and `suffix` arguments to `colapply` and `rollapply` methods: allows for adding return values of applied functions as new columns instead of replacing existing ones
* `class` argument of `cols` method now accepts a character vector of class names
* Changed default value of `class` argument of `cols` method from `"all"` to `NULL`, however, for backward compatibility `"all"` can still be used for the same result, but will eventually be treated as a filter for classes of type `all`
* Added `pattern` and `...` arguments to `cols` method: allows for searching column names
* Switched to argument check functions of the `checkmate` package instead of the `assertive.base`, `assertive.numbers`, `assertive.sets` and `assertive.types` packages
* Slightly improved vignettes and documentation
* Minor internal code improvements

## DTSg v0.2.1

* Fixed error "Unsupported type passed to argument 'data'." related to `dygraphs` in at least some previous releases of R ≤ 3.5.3

## DTSg v0.2.0

* Added `swallow` argument to `new` method: allows for a more ressource efficient object creation
* Added `drop` argument to `values` method: allows for a ressource efficient destruction of a `DTSg` object while preserving its *values*
* Added `class` argument to `values` method: can be used to return the *values* of a `DTSg` object as a `data.frame` instead of a `data.table`
* `alter` method and linked with it `new` and other methods now are way more ressource efficient in some cases
* Added `timestamps` field providing the total number of timestamps
* `print` method now ommits empty metadata fields
* Added means to measure code coverage with the help of `covr`
* Removed `xts` from suggested packages list (already comes along with `dygraphs`)
* Slightly improved vignettes and documentation
* Minor internal code improvements

## DTSg v0.1.3

* Slightly improved vignettes and documentation

## DTSg v0.1.2

* First release
