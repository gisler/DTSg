## Minor Release

* Added `getCol` method: allows for querying the values of a single column of a `DTSg` object
* Added `funby` and `ignoreDST` arguments to colapply: allows for applying functions like `cumsum` to a certain temporal level
* Added `na.status` argument to `new` and `alter` methods: allows for making missing values either `"explicit"` (default) or `"implicit"` or leaving them alone via `"undecided"`
* Added `na.status` field reflecting the status of missing values
* Added `na.status` also to the `list` of helper data passed on to temporal aggregation level funtions
* `funby` argument of `aggregate` method now also accepts a named `list` of functions: allows for calculating several summary statistics at once
* `periodicity` field can now be actively set in order to change the periodicity of the time series
* `timezone` field can now be actively set in order to convert the time zone of the series
* Fixed that a `DTSg` object with only one timestamp accepted a missing value in its *.dateTime* column
* Improved vignettes and documentation
* Minor internal code improvements

## Test Environments

* local Windows (3.5.3)
* local Windows (4.0.1)
* Linux on Travis CI (3.6.3)
* Linux on Travis CI (4.0.0)
* OS X on Travis CI (3.6.3)
* OS X on Travis CI (4.0.0)
* win-builder (devel)

## R CMD Check Results

There were no ERRORs, WARNINGs or NOTEs.

## Downstream Dependencies

There are currently no downstream dependencies for this package.
