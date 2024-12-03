# DTSg v1.1.3.9000

* Fixed support for `data.table`'s *GForce* optimisation when the `fun` argument of the `aggregate()` method is provided with a character vector specifying summary functions (either it stopped working somewhere down the line or it never worked). Please note that the column order of the resulting `DTSg` object is now different due to this fix.
* The `print()` method does not print its values' key anymore, as it used to be `data.table`'s default before v1.15.0.
* Slightly improved documentation.
* Minor internal code improvements.

# DTSg v1.1.3

* Ceased use of legacy time zone symlinks in tests.
* Bumped minimum tested R version from 4.0.2 to 4.2.3 using the corresponding _Posit_ public package manager snapshot.
* Slightly improved documentation.
* Minor internal code improvements.

# DTSg v1.1.1

* Column names can often now be additionally specified by a character string containing either comma separated column names, for example, `"x,y,z"`, or the start and end column separated by a colon, for example, `"x:z"`.
* Fixed a bug in `interpolateLinear()` causing partial last observation carried forward behaviour when its `roll` argument was specified smaller than the size of the gap to be interpolated.
* Fixed a bug in `interpolateLinear()` causing partial interpolation in certain cases when its `roll` argument was specified smaller than the size of the gap to be interpolated.
* Slightly improved documentation.

# DTSg v1.0.0

* Added `funbyApproach` argument to `new()`, `aggregate()`, `colapply()` and `subset()` methods: allows for specifying the flavour of the applied temporal aggregation level functions (either `"base"` utilising `as.POSIXct()` or `"fasttime"` utilising `fasttime::fastPOSIXct()` or `"RcppCCTZ"` utilising `RcppCCTZ::parseDatetime()` as the main function for transforming timestamps). Custom approaches for user defined TALFs are also possible. Please note that the `byFasttime*` versions of the TALFs are now deprecated. Use this argument from now on instead.
* Added `funbyApproach` also to the `list` of helper data (`funbyHelpers` argument) passed on to TALFs.
* Added `funbyApproach` field reflecting the individual `funbyApproach` of a `DTSg` object (can also be actively set in order to change the utilised approach).
* Added undocumented `names()` method exclusive only to the R6 interface acting as an alias for the `cols()` method.
* Added `mode` and `typeof` arguments to `cols()` method: allows for getting column names with a certain `mode()` and/or `typeof()`. This can be especially handy when making use of the `units` package.
* Added `DTSgFast`, `DTSgFunbyApproach` and `DTSgNA.status` options providing default values for the `fast`, `funbyApproach` and `na.status` arguments of the `new()` method.
* Added `DTSgDeprecatedWarnings` option: allows for disabling warnings from deprecated features.
* Greatly sped up `by______()` and `byFasttime______()` TALFs.
* It is no longer possible to use the deprecated value `"all"` with the `class` argument of the `cols()` method in order to get all column names. Use the default value `NULL` instead for this. `"all"` is treated as a filter for classes of type `all` from now on.
* Added an example to the `setCols()` method showing how to set measurement units with the help of the `units` package.
* Added an example to the documentation of the `colapply()` method showing how to calculate running correlations with the help of the `runner` package.
* Bumped minimum tested R version from 3.5.2 to 4.0.2 using the corresponding _MRAN_ repository snapshot.
* Slightly improved documentation.
* Minor internal code improvements.

# DTSg v0.8.1

* Added a “special class” called `".numerary"` available to the `class` argument of the `cols()` method: allows for querying the names of `integer` and `numeric` columns in one go.
* Added undocumented `raggregate()`, `rbind()`, `set()` and `setnames()` methods exclusive only to the R6 interface acting as aliases for the `rowaggregate()`, `rowbind()`, `setCols()` and `setColNames()` methods.
* `print()` method now truncates the number of printed rows of the values more aggressively.
* Created a `pkgdown` website.
* Improved vignettes and documentation.

# DTSg v0.7.1

* Fixed error "Error in prettyNum(.Internal(format(x, trim, digits, nsmall, width, 3L, : invalid value 0 for 'digits' argument" related to tests in upcoming R 4.2.0.
* Slightly improved documentation.
* Minor internal code improvements.

# DTSg v0.7.0

* Added `rowaggregate()` method: allows for applying summary functions row-wise to a `DTSg` object.
* Added `rowbind()` method: allows for combining the rows of a `DTSg` object.
* Added `setColNames()` method: allows for renaming columns of a `DTSg` object.
* Added `multiplier` and `funbyHelpers` arguments to `aggregate()`, `colapply()` and `subset()` methods: allows for adjusting the temporal aggregation level of TALFs and for passing on user defined helper data to TALFs.
* Added `multiplier` also to the `list` of helper data passed on to TALFs.
* Added `helpers` argument to `colapply()` and `rollapply()` methods: controls if helper data is passed on to an applied function (makes occasionally needed anonymous function wrappers obsolete, e.g. `x$colapply(fun = function(x, ...) {cumsum(x)}, funby = byYm____)` can now be written as `x$colapply(fun = cumsum, helpers = FALSE, funby = byYm____)`).
* TALFs based on the `fasttime` package now also support time zones equivalent to UTC (execute `grep("^(Etc/)?(UCT|UTC)$|^(Etc/)?GMT(\\+|-)?0?$", OlsonNames(), ignore.case = TRUE, value = TRUE)` for a full list of supported time zones).
* Fixed that a `DTSg` object with only one timestamp did not set the name of its *.dateTime* column as expected.
* Improved vignettes and documentation.
* Minor internal code improvements.

# DTSg v0.6.0

* Added `subset()` method: allows for filtering rows and/or selecting columns of a `DTSg` object.
* Added `setCols()` method: allows for setting the values of columns of, adding columns to and/or removing columns from a `DTSg` object.
* Added `[` extract operator: acts as a shortcut for the `getCol()` method.
* Added examples to the documentation of the `colapply()` method showing how to calculate moving averages with the help of the `runner` package instead of the `rollapply()` method.
* `aggregate()` method can benefit from `data.table`'s *GForce* optimisation now when its `fun` argument is provided with a character vector specifying summary functions.
* Greatly sped up `nas()` method.
* Temporal aggregation level functions supplied to the `funby` argument of the `colapply()` method are not forced to return a `POSIXct` timestamp any longer. They are, however, forced to return an atomic mode (the same goes for the `subset()` method).
* `getCol()` method now is capable of also querying the *.dateTime* column.
* `R6Method` argument of `S3WrapperGenerator()` function now also takes a public method of an `R6ClassGenerator` as a function and not only as an expression.
* Fixed that not all missing values were made explicit after a call to the `merge()` method despite an `"explicit"` `na.status` in some cases.
* Fixed that `getCol()` method tried to query all numeric columns instead of only the first one by default.
* Improved vignettes and documentation.
* Minor internal code improvements.

# DTSg v0.5.0

* Added `getCol()` method: allows for querying the values of a single column of a `DTSg` object.
* Added `funby` and `ignoreDST` arguments to `colapply()` method: allows for applying functions like `cumsum()` to a certain temporal level.
* Added `na.status` argument to `new()` and `alter()` methods: allows for making missing values either `"explicit"` (default) or `"implicit"` or leaving them alone via `"undecided"`.
* Added `na.status` field reflecting the status of missing values (can also be actively set).
* Added `na.status` also to the `list` of helper data passed on to temporal aggregation level functions.
* `funby` argument of `aggregate()` method now also accepts a named `list` of functions: allows for calculating several summary statistics at once.
* `periodicity` field can now be actively set in order to change the periodicity of the time series.
* `timezone` field can now be actively set in order to convert the time zone of the series.
* Fixed that a `DTSg` object with only one timestamp accepted a missing value in its *.dateTime* column.
* Improved vignettes and documentation.
* Minor internal code improvements.

# DTSg v0.4.1

* Fixed error "Error in as.POSIXct.numeric(e) : 'origin' must be supplied" related to `rollback()` function in upcoming R 4.1.0.
* Slightly improved vignettes.
* Minor internal code improvements.

# DTSg v0.4.0

* Added `memoryOverCPU` argument to `rollapply()` method: allows for preferring CPU over memory usage which makes the method more flexible in terms of resource consumption.
* Settings of option `DTSgClone` in e.g. *.RProfile* are now respected and not overwritten by `TRUE` when the package is loaded.
* Fixed useless coercion of `POSIXct` *.dateTime* columns to `POSIXct` upon object creation in case they contained at least one missing value.
* Switched to unit testing framework of the `tinytest` package instead of the `testthat` package.
* Slightly improved vignettes and documentation.
* Minor internal code improvements.

# DTSg v0.3.0

* Added `resultCols` and `suffix` arguments to `colapply()` and `rollapply()` methods: allows for adding return values of applied functions as new columns instead of replacing existing ones.
* `class` argument of `cols()` method now accepts a character vector of class names.
* Changed default value of `class` argument of `cols()` method from `"all"` to `NULL`, however, for backward compatibility `"all"` can still be used for the same result, but will eventually be treated as a filter for classes of type `all`.
* Added `pattern` and `...` arguments to `cols()` method: allows for searching column names.
* Switched to argument check functions of the `checkmate` package instead of the `assertive.base`, `assertive.numbers`, `assertive.sets` and `assertive.types` packages.
* Slightly improved vignettes and documentation.
* Minor internal code improvements.

# DTSg v0.2.1

* Fixed error "Unsupported type passed to argument 'data'." related to `dygraphs` in at least some previous releases of R ≤ 3.5.3.

# DTSg v0.2.0

* Added `swallow` argument to `new()` method: allows for a more resource efficient object creation.
* Added `drop` argument to `values()` method: allows for a resource efficient destruction of a `DTSg` object while preserving its values.
* Added `class` argument to `values()` method: can be used to return the values of a `DTSg` object as a `data.frame` instead of a `data.table`.
* `alter()` method and linked with it `new()` and other methods are now way more resource efficient in some cases.
* Added `timestamps` field providing the total number of timestamps.
* `print()` method now omits empty metadata fields.
* Added means to measure code coverage with the help of `covr`.
* Removed `xts` from suggested packages list (already comes along with `dygraphs`).
* Slightly improved vignettes and documentation.
* Minor internal code improvements.

# DTSg v0.1.3

* Slightly improved vignettes and documentation.

# DTSg v0.1.2

* First release.
