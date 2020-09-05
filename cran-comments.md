## Minor Release

* Added `rowaggregate` method: allows for applying summary functions row-wise to `DTSg` objects
* Added `rowbind` method: allows for combining the rows of `DTSg` objects
* Added `setColNames` method: allows for renaming columns of `DTSg` objects
* Added `multiplier` and `funbyHelpers` arguments to the `aggregate`, `colapply` and `subset` methods: allows for adjusting the temporal aggregation level of `TALFs` and for passing on user defined helper data to `TALFs`
* Added `multiplier` also to the `list` of helper data passed on to temporal aggregation level functions
* Added `helpers` argument to `colapply` and `rollapply` methods: controls if helper data is passed on to an applied function (makes occasionally needed anonymous function wrappers obsolete, e.g. `x$colapply(fun = function(x, ...) {cumsum(x)}, funby = byYm____)` can now be written as `x$colapply(fun = cumsum, helpers = FALSE, funby = byYm____)`)
* Temporal aggregation level functions based on the `fasttime` package now also support time zones equivalent to UTC (execute `grep("^(Etc/)?(UCT|UTC)$|^(Etc/)?GMT(\\+|-)?0?$", OlsonNames(), ignore.case = TRUE, value = TRUE)` for a full list of supported time zones)
* Fixed that a `DTSg` object with only one timestamp did not set the name of its *.dateTime* column as expected
* Improved vignettes and documentation
* Minor internal code improvements

## Test Environments

* local Windows (3.5.3)
* local Windows (4.0.2)
* Linux on Travis CI (3.6.3)
* Linux on Travis CI (4.0.2)
* OS X on Travis CI (4.0.2)
* win-builder (devel)

## R CMD Check Results

There was 1 NOTE:

unable to verify current time

## Downstream Dependencies

There are currently no downstream dependencies for this package.
