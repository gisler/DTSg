## DTSg v0.3.0.9000

* Fixed useless coercion of *.dateTime* column in case it contained `NA`
* Minor internal code improvements

## DTSg v0.3.0

* Added `resultCols` and `suffix` arguments to `colapply` and `rollapply` methods: allows for adding return values of applied functions as new columns instead of replacing existing ones
* `class` argument of `cols` method now accepts a character vector of class names
* Changed default value of `class` argument of `cols` method from `"all"` to `NULL`, however, for backward compatibility `"all"` can still be used for the same result, but will eventually be treated as a filter for classes of type `all`
* Added `pattern` and `...` arguments to `cols` method: allows for searching column names
* Switched to argument check functions of `checkmate` package instead of `assertive.base`, `assertive.numbers`, `assertive.sets` and `assertive.types` packages
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
* Removed `xts` from suggested package list (already comes along with `dygraphs`)
* Slightly improved vignettes and documentation
* Minor internal code improvements

## DTSg v0.1.3

* Slightly improved vignettes and documentation

## DTSg v0.1.2

* First release
