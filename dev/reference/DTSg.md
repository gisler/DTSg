# DTSg class

The `DTSg` class is the working horse of the package. It is an
[`R6::R6Class`](https://r6.r-lib.org/reference/R6Class.html) and offers
an S3 interface in addition to its native R6 interface. In the usage
sections of the documentation, unfortunately, only the usage of the S3
methods are displayed, however, the examples always show both ways of
calling the respective method. Generally, they are very similar anyway.
While the R6 interface always has the object first and the method is
then selected with the help of the `$` operator, for instance,
`x$cols()`, the S3 interface always has the method first and then the
object as its first argument, for instance, `cols(x)`. An exception is
the `new` method. It is not an S3 method, but an abused S4 constructor
with the character string `"DTSg"` as its first argument. Regarding the
R6 interface, the `DTSg` class generator has to be used to access the
`new` method with the help of the `$` operator.

## Usage

``` r
new(Class, values, ID = "", parameter = "", unit = "", variant = "",
  aggregated = FALSE, fast = getOption("DTSgFast"), swallow = FALSE,
  na.status = getOption("DTSgNA.status"), funbyApproach =
  getOption("DTSgFunbyApproach"))
```

## Arguments

- Class:

  A character string. Must be `"DTSg"` in order to create a `DTSg`
  object. Otherwise a different object may or may not be created (S4
  constructor only).

- values:

  A [`data.frame`](https://rdrr.io/r/base/data.frame.html) or object
  inherited from class
  [`data.frame`](https://rdrr.io/r/base/data.frame.html), e.g.
  [`data.table::data.table`](https://rdatatable.gitlab.io/data.table/reference/data.table.html).
  Its first column must be of class
  [`POSIXct`](https://rdrr.io/r/base/DateTimeClasses.html) or coercible
  to it. It serves as the object's time index and is renamed to
  *.dateTime.*

- ID:

  A character string specifying the ID (name) of the time series data
  object.

- parameter:

  A character string specifying the parameter name of the time series
  data.

- unit:

  A character string specifying the unit of the time series data.

- variant:

  A character string specifying further metadata of the time series, for
  instance, `"min"` to point out that it is a time series of lower bound
  measurements.

- aggregated:

  A logical specifying how the timestamps of the series have to be
  interpreted: as snap-shots (`FALSE`) or as periods between subsequent
  timestamps (`TRUE`).

- fast:

  A logical specifying if all rows (`FALSE`) or only the first 1000 rows
  (`TRUE`) shall be used to check the object's integrity and for the
  automatic detection of the time series' periodicity.

- swallow:

  A logical specifying if the object provided through the `values`
  argument shall be “swallowed” by the `DTSg` object, i.e. no copy of
  the data shall be made. This is generally more resource efficient, but
  only works when the provided object is a
  [`data.table::data.table`](https://rdatatable.gitlab.io/data.table/reference/data.table.html).
  Be warned, however, that when the creation of the `DTSg` object fails
  for some reason, the first column of the provided
  [`data.table::data.table`](https://rdatatable.gitlab.io/data.table/reference/data.table.html)
  might have been coerced to
  [`POSIXct`](https://rdrr.io/r/base/DateTimeClasses.html) and keyed
  (see
  [`data.table::setkey`](https://rdatatable.gitlab.io/data.table/reference/setkey.html)
  for further information). Furthermore, all references to the
  “swallowed”
  [`data.table::data.table`](https://rdatatable.gitlab.io/data.table/reference/data.table.html)
  in the global (and only the global) environment are removed upon the
  successful creation of the `DTSg` object.

- na.status:

  A character string. Either `"explicit"`, which makes missing
  timestamps explicit according to the recognised periodicity, or
  `"implicit"`, which removes timestamps with missing values on all
  value columns, or `"undecided"` for no such action. Please note that
  `DTSg` objects work best with explicitly missing values.

- funbyApproach:

  A character string specifying the default flavour of
  [`TALFs`](https://gisler.github.io/DTSg/dev/reference/TALFs.md) used
  with the created `DTSg` object. Either `"timechange"`, which utilises
  [`timechange::time_floor`](https://rdrr.io/pkg/timechange/man/time_round.html),
  or `"base"`, which utilises
  [`as.POSIXct`](https://rdrr.io/r/base/as.POSIXlt.html), or
  `"fasttime"`, which utilises
  [`fasttime::fastPOSIXct`](https://rdrr.io/pkg/fasttime/man/fastPOSIXct.html),
  or `"RcppCCTZ"`, which utilises
  [`RcppCCTZ::parseDatetime`](https://rdrr.io/pkg/RcppCCTZ/man/parseDatetime.html)
  as the main function for transforming timestamps. Custom approaches
  for user defined temporal aggregation level functions are also
  possible.

## Value

Returns a `DTSg` object.

## Note

Due to the [`POSIXct`](https://rdrr.io/r/base/DateTimeClasses.html)
nature of the *.dateTime* column, the same sub-second accuracy, issues
and limitations apply to `DTSg` objects. In order to prevent at least
some of the possible precision issues, the lags between subsequent
timestamps are rounded to microseconds during integrity checks. This
corresponds to the maximum value allowed for
[`options`](https://rdrr.io/r/base/options.html)`("digits.secs")`. As a
consequence, time series with a sub-second accuracy higher than a
microsecond will never work.

## Methods

A `DTSg` object has the following methods:

- `aggregate`: See
  [`aggregate`](https://gisler.github.io/DTSg/dev/reference/aggregate.DTSg.md)
  for further information.

- `alter`: See
  [`alter`](https://gisler.github.io/DTSg/dev/reference/alter.DTSg.md)
  for further information.

- `clone`: See
  [`clone`](https://gisler.github.io/DTSg/dev/reference/clone.DTSg.md)
  for further information.

- `colapply`: See
  [`colapply`](https://gisler.github.io/DTSg/dev/reference/colapply.DTSg.md)
  for further information.

- `cols` and its R6 only alias `names`: See
  [`cols`](https://gisler.github.io/DTSg/dev/reference/cols.DTSg.md) for
  further information.

- `getCol`: See
  [`getCol`](https://gisler.github.io/DTSg/dev/reference/getCol.DTSg.md)
  for further information.

- `merge`: See
  [`merge`](https://gisler.github.io/DTSg/dev/reference/merge.DTSg.md)
  for further information.

- `nas`: See
  [`nas`](https://gisler.github.io/DTSg/dev/reference/nas.DTSg.md) for
  further information.

- `plot`: See
  [`plot`](https://gisler.github.io/DTSg/dev/reference/plot.DTSg.md) for
  further information.

- `print`: See
  [`print`](https://gisler.github.io/DTSg/dev/reference/print.DTSg.md)
  for further information.

- `refresh`: See
  [`refresh`](https://gisler.github.io/DTSg/dev/reference/refresh.DTSg.md)
  for further information.

- `rollapply`: See
  [`rollapply`](https://gisler.github.io/DTSg/dev/reference/rollapply.DTSg.md)
  for further information.

- `rowaggregate` and its R6 only alias `raggregate`: See
  [`rowaggregate`](https://gisler.github.io/DTSg/dev/reference/rowaggregate.DTSg.md)
  for further information.

- `rowbind` and its R6 only alias `rbind`: See
  [`rowbind`](https://gisler.github.io/DTSg/dev/reference/rowbind.DTSg.md)
  for further information.

- `setColNames` and its R6 only alias `setnames`: See
  [`setColNames`](https://gisler.github.io/DTSg/dev/reference/setColNames.DTSg.md)
  for further information.

- `setCols` and its R6 only alias `set`: See
  [`setCols`](https://gisler.github.io/DTSg/dev/reference/setCols.DTSg.md)
  for further information.

- `subset`: See
  [`subset`](https://gisler.github.io/DTSg/dev/reference/subset.DTSg.md)
  for further information.

- `summary`: See
  [`summary`](https://gisler.github.io/DTSg/dev/reference/summary.DTSg.md)
  for further information.

- `values`: See
  [`values`](https://gisler.github.io/DTSg/dev/reference/values.DTSg.md)
  for further information.

## Fields

A `DTSg` object has the following fields or properties as they are often
called. They are implemented through so called active bindings, which
means that they can be accessed and actively set with the help of the
`$` operator, for instance, `x$ID` gets the value of the `ID` field and
`x$ID <- "River Flow"` sets its value. Please note that fields are
always modified in place, i.e. no deep clone (copy) of the object is
made beforehand. See
[`clone`](https://gisler.github.io/DTSg/dev/reference/clone.DTSg.md) for
further information. Some of the fields are read-only though:

- `aggregated`: Same as the `aggregated` argument.

- `fast`: Same as the `fast` argument.

- `funbyApproach`: Same as the `funbyApproach` argument.

- `ID`: Same as the `ID` argument. It is used as the title of plots.

- `na.status`: Same as the `na.status` argument. When set, the missing
  values of the object are expanded or collapsed accordingly.

- `parameter`: Same as the `parameter` argument. It is used as the label
  of the primary axis of plots.

- `periodicity`: A [`difftime`](https://rdrr.io/r/base/difftime.html)
  object for a regular and a character string for an irregular `DTSg`
  object describing its periodicity or containing `"unrecognised"` in
  case it could not be detected. When set, the periodicity of the time
  series is changed as specified. See the `by` argument of
  [`alter`](https://gisler.github.io/DTSg/dev/reference/alter.DTSg.md)
  for further information.

- `regular`: A logical signalling if all lags in seconds between
  subsequent timestamps are the same (`TRUE`) or if some are different
  (`FALSE`). A, for instance, monthly time series is considered
  irregular in this sense (read-only).

- `timestamps`: An integer showing the total number of timestamps of the
  time series (read-only).

- `timezone`: A character string showing the time zone of the time
  series. When set, the series is converted to the specified time zone.
  Only names from [`OlsonNames`](https://rdrr.io/r/base/timezones.html)
  are accepted.

- `unit`: Same as the `unit` argument. It is added to the label of the
  primary axis of plots when the `parameter` field is set.

- `variant`: Same as the `variant` argument. It is added to the label of
  the primary axis of plots when the `parameter` field is set.

The `parameter`, `unit` and `variant` fields are especially useful for
time series of a single variable. For time series of multiple variables
with differing units the functionality of the units package may pose a
viable alternative.

## Options

The behaviour of `DTSg` objects can be customised with the help of the
following option. See [`options`](https://rdrr.io/r/base/options.html)
for further information:

- *DTSgClone:* A logical specifying if `DTSg` objects are, by default,
  modified in place (`FALSE`) or if a deep clone (copy) shall be made
  beforehand (`TRUE`) (package's default is `TRUE`).

- *DTSgDeprecatedWarnings:* A logical specifying if warnings are
  displayed when calling deprecated features (package's default is
  `TRUE`).

- *DTSgFast:* Default value for the `fast` argument (package's default
  is `FALSE`).

- *DTSgFunbyApproach:* Default value for the `funbyApproach` argument
  (package's default is `"timechange"`).

- *DTSgNA.status:* Default value for the `na.status` argument (package's
  default is `"explicit"`).

## Examples

``` r
# new DTSg object
## R6 constructor
DTSg$new(
  values = flow,
  ID = "River Flow"
)
#> Values:
#>        .dateTime   flow
#>           <POSc>  <num>
#>    1: 2007-01-01  9.540
#>    2: 2007-01-02  9.285
#>    3: 2007-01-03  8.940
#>    4: 2007-01-04  8.745
#>    5: 2007-01-05  8.490
#>   ---                  
#> 2188: 2012-12-27 26.685
#> 2189: 2012-12-28 28.050
#> 2190: 2012-12-29 23.580
#> 2191: 2012-12-30 18.840
#> 2192: 2012-12-31 17.250
#> 
#> ID:             River Flow
#> Aggregated:     FALSE
#> Regular:        TRUE
#> Periodicity:    Time difference of 1 days
#> Missing values: explicit
#> Time zone:      UTC
#> Timestamps:     2192

## abused S4 constructor
new(
  Class = "DTSg",
  values = flow,
  ID = "River Flow"
)
#> Values:
#>        .dateTime   flow
#>           <POSc>  <num>
#>    1: 2007-01-01  9.540
#>    2: 2007-01-02  9.285
#>    3: 2007-01-03  8.940
#>    4: 2007-01-04  8.745
#>    5: 2007-01-05  8.490
#>   ---                  
#> 2188: 2012-12-27 26.685
#> 2189: 2012-12-28 28.050
#> 2190: 2012-12-29 23.580
#> 2191: 2012-12-30 18.840
#> 2192: 2012-12-31 17.250
#> 
#> ID:             River Flow
#> Aggregated:     FALSE
#> Regular:        TRUE
#> Periodicity:    Time difference of 1 days
#> Missing values: explicit
#> Time zone:      UTC
#> Timestamps:     2192
```
