# Changelog

## DTSg v2.0.0.9000

- The day saving time shift for the `ignoreDST` argument of the
  [`aggregate()`](https://gisler.github.io/DTSg/dev/reference/aggregate.DTSg.md),
  [`colapply()`](https://gisler.github.io/DTSg/dev/reference/colapply.DTSg.md)
  and
  [`subset()`](https://gisler.github.io/DTSg/dev/reference/subset.DTSg.md)
  methods is now attempted to be estimated and not invariably assumed to
  be one hour long.
- More time zones are now considered UTC or equivalent (execute
  `grep("^(Etc/)?(UTC|UCT|Universal|Zulu)$|^(Etc/)?(GMT(\\+|-)?0?|Greenwich)$", OlsonNames(), ignore.case = TRUE, value = TRUE)`
  for a full list).
- Updated the package’s `pkgdown` website to Bootstrap 5.
- Slightly improved the documentation.
- Minor internal code improvements, which require R ≥ 4.0.0.

## DTSg v2.0.0

CRAN release: 2025-01-20

- Added `"timechange"` `funbyApproach` utilising
  [`timechange::time_floor()`](https://rdrr.io/pkg/timechange/man/time_round.html)
  as the main function for transforming timestamps. This new approach
  generally is way faster than the existing ones for both families of
  TALFs and all time zones, hence it is the new package’s default.
- Sped up the
  [`byY_____()`](https://gisler.github.io/DTSg/dev/reference/TALFs.md)
  and
  [`byYm____()`](https://gisler.github.io/DTSg/dev/reference/TALFs.md)
  TALFs of the `"base"` `funbyApproach`.
- The
  [`print()`](https://gisler.github.io/DTSg/dev/reference/print.DTSg.md)
  method does not print the values’ key anymore, as it used to be
  `data.table`’s default before v1.15.0.
- Fixed a bug in the
  [`aggregate()`](https://gisler.github.io/DTSg/dev/reference/aggregate.DTSg.md)
  method, which could lead to incorrect results under the following
  conditions:
  - The time zone of the `DTSg` object is not UTC or equivalent.
  - The `ignoreDST` argument is `TRUE`.
  - A single column with missing values is aggregated.
- Related to the above fix are the following breaking changes when
  aggregating a single column with missing values:
  - A possible `.n` column now contains `0` instead of `NA` in case all
    values of a certain temporal aggregation level are missing.
  - The stripping of missing values within summary functions now depends
    on the value of a possible `na.rm` argument.
- Fixed the support for `data.table`’s *GForce* optimisation when the
  `fun` argument of the
  [`aggregate()`](https://gisler.github.io/DTSg/dev/reference/aggregate.DTSg.md)
  method is provided with a character vector specifying summary
  functions. Please note that the column order of the resulting `DTSg`
  object is now different due to this fix.
- Fixed that the
  [`setCols()`](https://gisler.github.io/DTSg/dev/reference/setCols.DTSg.md)
  method and its R6 only alias
  [`set()`](https://gisler.github.io/DTSg/dev/reference/setCols.DTSg.md)
  was too anxious about removing all value columns.
- It is no longer possible to use the deprecated `byFasttime*()` TALFs.
  Please use the `"fasttime"` or even better the new `"timechange"`
  `funbyApproach` from now on.
- Removed `magrittr` from the suggested packages list (vignettes now use
  R’s native pipe operator).
- Slightly improved the documentation.
- Improved tests.
- Major internal code improvements.

## DTSg v1.1.3

CRAN release: 2023-09-27

- Ceased the use of legacy time zone symlinks in tests.
- Bumped the minimum tested R version from 4.0.2 to 4.2.3 using the
  corresponding *Posit* public package manager snapshot.
- Slightly improved the documentation.
- Minor internal code improvements.

## DTSg v1.1.1

CRAN release: 2022-06-08

- Column names can often now be additionally specified by a character
  string containing either comma separated column names, for example,
  `"x,y,z"`, or the start and end column separated by a colon, for
  example, `"x:z"`.
- Fixed a bug in the
  [`interpolateLinear()`](https://gisler.github.io/DTSg/dev/reference/interpolateLinear.md)
  function causing partial last observation carried forward behaviour
  when its `roll` argument was specified smaller than the size of the
  gap to be interpolated.
- Fixed a bug in the
  [`interpolateLinear()`](https://gisler.github.io/DTSg/dev/reference/interpolateLinear.md)
  function causing partial interpolation in certain cases when its
  `roll` argument was specified smaller than the size of the gap to be
  interpolated.
- Slightly improved the documentation.

## DTSg v1.0.0

CRAN release: 2022-05-02

- Added `funbyApproach` argument to the
  [`new()`](https://gisler.github.io/DTSg/dev/reference/DTSg.md),
  [`aggregate()`](https://gisler.github.io/DTSg/dev/reference/aggregate.DTSg.md),
  [`colapply()`](https://gisler.github.io/DTSg/dev/reference/colapply.DTSg.md)
  and
  [`subset()`](https://gisler.github.io/DTSg/dev/reference/subset.DTSg.md)
  methods: allows for specifying the flavour of the applied temporal
  aggregation level functions (either `"base"` utilising
  [`as.POSIXct()`](https://rdrr.io/r/base/as.POSIXlt.html) or
  `"fasttime"` utilising
  [`fasttime::fastPOSIXct()`](https://rdrr.io/pkg/fasttime/man/fastPOSIXct.html)
  or `"RcppCCTZ"` utilising
  [`RcppCCTZ::parseDatetime()`](https://rdrr.io/pkg/RcppCCTZ/man/parseDatetime.html)
  as the main function for transforming timestamps). Custom approaches
  for user defined TALFs are also possible. Please note that the
  `byFasttime*` versions of the TALFs are now deprecated. Use this
  argument from now on instead.
- Added `funbyApproach` also to the `list` of helper data
  (`funbyHelpers` argument) passed on to TALFs.
- Added `funbyApproach` field reflecting the individual `funbyApproach`
  of a `DTSg` object (can also be actively set in order to change the
  utilised approach).
- Added undocumented
  [`names()`](https://gisler.github.io/DTSg/dev/reference/cols.DTSg.md)
  method exclusively to the R6 interface acting as an alias for the
  [`cols()`](https://gisler.github.io/DTSg/dev/reference/cols.DTSg.md)
  method.
- Added `mode` and `typeof` arguments to the
  [`cols()`](https://gisler.github.io/DTSg/dev/reference/cols.DTSg.md)
  method: allows for getting column names with a certain
  [`mode()`](https://rdrr.io/r/base/mode.html) and/or
  [`typeof()`](https://rdrr.io/r/base/typeof.html). This can be
  especially handy when making use of the `units` package.
- Added `DTSgFast`, `DTSgFunbyApproach` and `DTSgNA.status` options
  providing default values for the `fast`, `funbyApproach` and
  `na.status` arguments of the
  [`new()`](https://gisler.github.io/DTSg/dev/reference/DTSg.md) method.
- Added `DTSgDeprecatedWarnings` option: allows for disabling warnings
  from deprecated features.
- Greatly sped up the
  [`by______()`](https://gisler.github.io/DTSg/dev/reference/TALFs.md)
  and `byFasttime______()` TALFs.
- It is no longer possible to use the deprecated value `"all"` with the
  `class` argument of the
  [`cols()`](https://gisler.github.io/DTSg/dev/reference/cols.DTSg.md)
  method in order to get all column names. Use the default value `NULL`
  instead for this. `"all"` is treated as a filter for classes of type
  `all` from now on.
- Added an example to the
  [`setCols()`](https://gisler.github.io/DTSg/dev/reference/setCols.DTSg.md)
  method showing how to set measurement units with the help of the
  `units` package.
- Added an example to the documentation of the
  [`colapply()`](https://gisler.github.io/DTSg/dev/reference/colapply.DTSg.md)
  method showing how to calculate running correlations with the help of
  the `runner` package.
- Bumped the minimum tested R version from 3.5.2 to 4.0.2 using the
  corresponding *MRAN* repository snapshot.
- Slightly improved the documentation.
- Minor internal code improvements.

## DTSg v0.8.1

CRAN release: 2021-10-27

- Added a “special class” called `".numerary"` available to the `class`
  argument of the
  [`cols()`](https://gisler.github.io/DTSg/dev/reference/cols.DTSg.md)
  method: allows for querying the names of `integer` and `numeric`
  columns in one go.
- Added undocumented
  [`raggregate()`](https://gisler.github.io/DTSg/dev/reference/rowaggregate.DTSg.md),
  [`rbind()`](https://gisler.github.io/DTSg/dev/reference/rowbind.DTSg.md),
  [`set()`](https://gisler.github.io/DTSg/dev/reference/setCols.DTSg.md)
  and
  [`setnames()`](https://gisler.github.io/DTSg/dev/reference/setColNames.DTSg.md)
  methods exclusively to the R6 interface acting as aliases for the
  [`rowaggregate()`](https://gisler.github.io/DTSg/dev/reference/rowaggregate.DTSg.md),
  [`rowbind()`](https://gisler.github.io/DTSg/dev/reference/rowbind.DTSg.md),
  [`setCols()`](https://gisler.github.io/DTSg/dev/reference/setCols.DTSg.md)
  and
  [`setColNames()`](https://gisler.github.io/DTSg/dev/reference/setColNames.DTSg.md)
  methods.
- The
  [`print()`](https://gisler.github.io/DTSg/dev/reference/print.DTSg.md)
  method now truncates the number of printed rows of the values more
  aggressively.
- Created a `pkgdown` website.
- Improved the vignettes and documentation.

## DTSg v0.7.1

CRAN release: 2021-05-30

- Fixed error “Error in prettyNum(.Internal(format(x, trim, digits,
  nsmall, width, 3L, : invalid value 0 for ‘digits’ argument” related to
  tests in upcoming R 4.2.0.
- Slightly improved the documentation.
- Minor internal code improvements.

## DTSg v0.7.0

CRAN release: 2020-09-05

- Added
  [`rowaggregate()`](https://gisler.github.io/DTSg/dev/reference/rowaggregate.DTSg.md)
  method: allows for applying summary functions row-wise to a `DTSg`
  object.
- Added
  [`rowbind()`](https://gisler.github.io/DTSg/dev/reference/rowbind.DTSg.md)
  method: allows for combining the rows of a `DTSg` object.
- Added
  [`setColNames()`](https://gisler.github.io/DTSg/dev/reference/setColNames.DTSg.md)
  method: allows for renaming columns of a `DTSg` object.
- Added `multiplier` and `funbyHelpers` arguments to the
  [`aggregate()`](https://gisler.github.io/DTSg/dev/reference/aggregate.DTSg.md),
  [`colapply()`](https://gisler.github.io/DTSg/dev/reference/colapply.DTSg.md)
  and
  [`subset()`](https://gisler.github.io/DTSg/dev/reference/subset.DTSg.md)
  methods: allows for adjusting the temporal aggregation level of TALFs
  and for passing on user defined helper data to TALFs.
- Added `multiplier` also to the `list` of helper data passed on to
  TALFs.
- Added `helpers` argument to the
  [`colapply()`](https://gisler.github.io/DTSg/dev/reference/colapply.DTSg.md)
  and
  [`rollapply()`](https://gisler.github.io/DTSg/dev/reference/rollapply.DTSg.md)
  methods: controls if helper data is passed on to an applied function
  (makes occasionally needed anonymous function wrappers obsolete,
  e.g. `x$colapply(fun = function(x, ...) {cumsum(x)}, funby = byYm____)`
  can now be written as
  `x$colapply(fun = cumsum, helpers = FALSE, funby = byYm____)`).
- TALFs based on the `fasttime` package now also support time zones
  equivalent to UTC (execute
  `grep("^(Etc/)?(UCT|UTC)$|^(Etc/)?GMT(\\+|-)?0?$", OlsonNames(), ignore.case = TRUE, value = TRUE)`
  for a full list of supported time zones).
- Fixed that a `DTSg` object with only one timestamp did not set the
  name of its *.dateTime* column as expected.
- Improved the vignettes and documentation.
- Minor internal code improvements.

## DTSg v0.6.0

CRAN release: 2020-07-12

- Added
  [`subset()`](https://gisler.github.io/DTSg/dev/reference/subset.DTSg.md)
  method: allows for filtering rows and/or selecting columns of a `DTSg`
  object.
- Added
  [`setCols()`](https://gisler.github.io/DTSg/dev/reference/setCols.DTSg.md)
  method: allows for setting the values of columns of, adding columns to
  and/or removing columns from a `DTSg` object.
- Added `[` extract operator: acts as a shortcut for the
  [`getCol()`](https://gisler.github.io/DTSg/dev/reference/getCol.DTSg.md)
  method.
- Added examples to the documentation of the
  [`colapply()`](https://gisler.github.io/DTSg/dev/reference/colapply.DTSg.md)
  method showing how to calculate moving averages with the help of the
  `runner` package instead of the
  [`rollapply()`](https://gisler.github.io/DTSg/dev/reference/rollapply.DTSg.md)
  method.
- The
  [`aggregate()`](https://gisler.github.io/DTSg/dev/reference/aggregate.DTSg.md)
  method can benefit from `data.table`’s *GForce* optimisation now when
  its `fun` argument is provided with a character vector specifying
  summary functions.
- Greatly sped up the
  [`nas()`](https://gisler.github.io/DTSg/dev/reference/nas.DTSg.md)
  method.
- Temporal aggregation level functions supplied to the `funby` argument
  of the
  [`colapply()`](https://gisler.github.io/DTSg/dev/reference/colapply.DTSg.md)
  method are not forced to return a `POSIXct` timestamp any longer. They
  are, however, forced to return an atomic mode (the same goes for the
  [`subset()`](https://gisler.github.io/DTSg/dev/reference/subset.DTSg.md)
  method).
- The
  [`getCol()`](https://gisler.github.io/DTSg/dev/reference/getCol.DTSg.md)
  method now is capable of also querying the *.dateTime* column.
- The `R6Method` argument of the
  [`S3WrapperGenerator()`](https://gisler.github.io/DTSg/dev/reference/S3WrapperGenerator.md)
  function now also takes a public method of an `R6ClassGenerator` as a
  function and not only as an expression.
- Fixed that not all missing values were made explicit after a call to
  the
  [`merge()`](https://gisler.github.io/DTSg/dev/reference/merge.DTSg.md)
  method despite an `"explicit"` `na.status` in some cases.
- Fixed that the
  [`getCol()`](https://gisler.github.io/DTSg/dev/reference/getCol.DTSg.md)
  method tried to query all numeric columns instead of only the first
  one by default.
- Improved the vignettes and documentation.
- Minor internal code improvements.

## DTSg v0.5.0

CRAN release: 2020-06-09

- Added
  [`getCol()`](https://gisler.github.io/DTSg/dev/reference/getCol.DTSg.md)
  method: allows for querying the values of a single column of a `DTSg`
  object.
- Added `funby` and `ignoreDST` arguments to the
  [`colapply()`](https://gisler.github.io/DTSg/dev/reference/colapply.DTSg.md)
  method: allows for applying functions like
  [`cumsum()`](https://rdrr.io/r/base/cumsum.html) to a certain temporal
  level.
- Added `na.status` argument to the
  [`new()`](https://gisler.github.io/DTSg/dev/reference/DTSg.md) and
  [`alter()`](https://gisler.github.io/DTSg/dev/reference/alter.DTSg.md)
  methods: allows for making missing values either `"explicit"`
  (default) or `"implicit"` or leaving them alone via `"undecided"`.
- Added `na.status` field reflecting the status of missing values (can
  also be actively set).
- Added `na.status` also to the `list` of helper data passed on to
  temporal aggregation level functions.
- The `funby` argument of the
  [`aggregate()`](https://gisler.github.io/DTSg/dev/reference/aggregate.DTSg.md)
  method now also accepts a named `list` of functions: allows for
  calculating several summary statistics at once.
- The `periodicity` field can now be actively set in order to change the
  periodicity of the time series.
- The `timezone` field can now be actively set in order to convert the
  time zone of the series.
- Fixed that a `DTSg` object with only one timestamp accepted a missing
  value in its *.dateTime* column.
- Improved the vignettes and documentation.
- Minor internal code improvements.

## DTSg v0.4.1

CRAN release: 2020-05-08

- Fixed error “Error in as.POSIXct.numeric(e) : ‘origin’ must be
  supplied” related to the
  [`rollback()`](https://gisler.github.io/DTSg/dev/reference/rollback.md)
  function in upcoming R 4.1.0.
- Slightly improved the vignettes.
- Minor internal code improvements.

## DTSg v0.4.0

CRAN release: 2020-03-28

- Added `memoryOverCPU` argument to the
  [`rollapply()`](https://gisler.github.io/DTSg/dev/reference/rollapply.DTSg.md)
  method: allows for preferring CPU over memory usage which makes the
  method more flexible in terms of resource consumption.
- Settings of the option `DTSgClone` in e.g. *.RProfile* are now
  respected and not overwritten by `TRUE` when the package is loaded.
- Fixed useless coercion of `POSIXct` *.dateTime* columns to `POSIXct`
  upon object creation in case they contained at least one missing
  value.
- Switched to the unit testing framework of the `tinytest` instead of
  the `testthat` package.
- Slightly improved the vignettes and documentation.
- Minor internal code improvements.

## DTSg v0.3.0

CRAN release: 2020-01-08

- Added `resultCols` and `suffix` arguments to the
  [`colapply()`](https://gisler.github.io/DTSg/dev/reference/colapply.DTSg.md)
  and
  [`rollapply()`](https://gisler.github.io/DTSg/dev/reference/rollapply.DTSg.md)
  methods: allows for adding return values of applied functions as new
  columns instead of replacing existing ones.
- `class` argument of the
  [`cols()`](https://gisler.github.io/DTSg/dev/reference/cols.DTSg.md)
  method now accepts a character vector of class names.
- Changed the default value of the `class` argument of the
  [`cols()`](https://gisler.github.io/DTSg/dev/reference/cols.DTSg.md)
  method from `"all"` to `NULL`, however, for backward compatibility
  `"all"` can still be used for the same result, but will eventually be
  treated as a filter for classes of type `all`.
- Added `pattern` and `...` arguments to the
  [`cols()`](https://gisler.github.io/DTSg/dev/reference/cols.DTSg.md)
  method: allows for searching column names.
- Switched to the argument check functions of the `checkmate` package
  instead of the `assertive.base`, `assertive.numbers`, `assertive.sets`
  and `assertive.types` packages.
- Slightly improved the vignettes and documentation.
- Minor internal code improvements.

## DTSg v0.2.1

CRAN release: 2019-10-20

- Fixed error “Unsupported type passed to argument ‘data’.” related to
  the `dygraphs` package in at least some releases of R ≤ 3.5.3.

## DTSg v0.2.0

CRAN release: 2019-10-14

- Added `swallow` argument to the
  [`new()`](https://gisler.github.io/DTSg/dev/reference/DTSg.md) method:
  allows for a more resource efficient object creation.
- Added `drop` argument to the
  [`values()`](https://gisler.github.io/DTSg/dev/reference/values.DTSg.md)
  method: allows for a resource efficient destruction of a `DTSg` object
  while preserving its values.
- Added `class` argument to the
  [`values()`](https://gisler.github.io/DTSg/dev/reference/values.DTSg.md)
  method: can be used to return the values of a `DTSg` object as a
  `data.frame` instead of a `data.table`.
- The
  [`alter()`](https://gisler.github.io/DTSg/dev/reference/alter.DTSg.md)
  method and linked with it the
  [`new()`](https://gisler.github.io/DTSg/dev/reference/DTSg.md) and
  other methods are now way more resource efficient in some cases.
- Added `timestamps` field providing the total number of timestamps.
- The
  [`print()`](https://gisler.github.io/DTSg/dev/reference/print.DTSg.md)
  method now omits empty metadata fields.
- Added means to measure code coverage with the help of the `covr`
  package.
- Removed `xts` from the suggested packages list (already comes along
  with the `dygraphs` package).
- Slightly improved the vignettes and documentation.
- Minor internal code improvements.

## DTSg v0.1.3

CRAN release: 2019-08-29

- Slightly improved the vignettes and documentation.

## DTSg v0.1.2

CRAN release: 2019-03-13

- First release.
