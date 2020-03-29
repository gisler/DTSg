## Minor Release

* Added `memoryOverCPU` argument to `rollapply` method: allows for preferring CPU over memory usage, which makes the method more flexible in terms of resource consumption
* Settings of option `DTSgClone` in e.g. *.RProfile* are now respected and not overwritten by `TRUE` when the package is loaded
* Fixed useless coercion of `POSIXct` *.dateTime* columns to `POSIXct` upon object creation in case they contained at least one `NA` value
* Switched to unit testing framework of the `tinytest` package instead of the `testthat` package
* Slightly improved vignettes and documentation
* Minor internal code improvements

## Test Environments

* local Windows (R 3.5.3)
* local Windows (R 3.6.3)
* Linux on Travis CI (R 3.5.3)
* Linux on Travis CI (R 3.6.2)
* OS X on Travis CI (R 3.5.3)
* OS X on Travis CI (R 3.6.2)
* win-builder (devel)

## R CMD Check Results

There were no ERRORs, WARNINGs or NOTEs.

## Downstream Dependencies

There are currently no downstream dependencies for this package.
