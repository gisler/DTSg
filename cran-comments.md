## Minor Release

* Added `resultCols` and `suffix` arguments to `colapply` and `rollapply` methods: allows for adding return values of applied functions as new columns instead of replacing existing ones
* `class` argument of `cols` method now accepts a character vector of class names
* Changed default value of `class` argument of `cols` method from `"all"` to `NULL`, however, for backward compatibility `"all"` can still be used for the same result, but will eventually be treated as a filter for classes of type `all`
* Added `pattern` and `...` arguments to `cols` method: allows for searching column names
* Switched to argument check functions of `checkmate` package instead of `assertive.base`, `assertive.numbers`, `assertive.sets` and `assertive.types` packages
* Slightly improved vignettes and documentation
* Minor internal code improvements

## Test Environments

* local Windows (R 3.5.3)
* local Windows (R 3.6.2)
* Linux on Travis CI (R 3.5.3)
* Linux on Travis CI (R 3.6.1)
* OS X on Travis CI (R 3.5.3)
* OS X on Travis CI (R 3.6.2)
* win-builder (devel)

## R CMD Check Results

There were no ERRORs, WARNINGs or NOTEs.

## Downstream Dependencies

There are currently no downstream dependencies for this package.
