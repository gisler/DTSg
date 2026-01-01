# Subset time series data

Filters rows and/or selects columns of a
[`DTSg`](https://gisler.github.io/DTSg/dev/reference/DTSg.md) object.

## Usage

``` r
# S3 method for class 'DTSg'
subset(
  x,
  i,
  cols = self$cols(),
  funby = NULL,
  ignoreDST = FALSE,
  na.status = "implicit",
  clone = getOption("DTSgClone"),
  multiplier = 1L,
  funbyHelpers = NULL,
  funbyApproach = self$funbyApproach,
  ...
)
```

## Arguments

- x:

  A [`DTSg`](https://gisler.github.io/DTSg/dev/reference/DTSg.md) object
  (S3 method only).

- i:

  An integerish vector indexing rows (positive numbers pick and negative
  numbers omit rows) or a filter expression accepted by the `i` argument
  of
  [`data.table::data.table`](https://rdatatable.gitlab.io/data.table/reference/data.table.html).
  Filter expressions can contain the special symbol
  [`.N`](https://rdatatable.gitlab.io/data.table/reference/special-symbols.html).

- cols:

  A character vector specifying the columns to select. Another
  possibility is a character string containing either comma separated
  column names, for example, `"x,y,z"`, or the start and end column
  separated by a colon, for example, `"x:z"`. The *.dateTime* column is
  always selected and cannot be part of it.

- funby:

  One of the temporal aggregation level functions described in
  [`TALFs`](https://gisler.github.io/DTSg/dev/reference/TALFs.md) or a
  user defined temporal aggregation level function. Can be used to, for
  instance, select the last two observations of a certain temporal
  level. See corresponding section and examples for further information.

- ignoreDST:

  A logical specifying if day saving time shall be ignored by `funby`.
  See corresponding section for further information.

- na.status:

  A character string. Either `"explicit"`, which makes missing
  timestamps explicit according to the recognised periodicity, or
  `"implicit"`, which removes timestamps with missing values on all
  value columns. See corresponding section for further information.

- clone:

  A logical specifying if the object shall be modified in place or if a
  deep clone (copy) shall be made beforehand.

- multiplier:

  A positive integerish value “multiplying” the temporal aggregation
  level of certain
  [`TALFs`](https://gisler.github.io/DTSg/dev/reference/TALFs.md). See
  corresponding section for further information.

- funbyHelpers:

  An optional [`list`](https://rdrr.io/r/base/list.html) with helper
  data passed on to `funby`. See corresponding section for further
  information.

- funbyApproach:

  A character string specifying the flavour of the applied temporal
  aggregation level function. Either `"timechange"`, which utilises
  [`timechange::time_floor`](https://rdrr.io/pkg/timechange/man/time_round.html),
  or `"base"`, which utilises
  [`as.POSIXct`](https://rdrr.io/r/base/as.POSIXlt.html), or
  `"fasttime"`, which utilises
  [`fasttime::fastPOSIXct`](https://rdrr.io/pkg/fasttime/man/fastPOSIXct.html),
  or `"RcppCCTZ"`, which utilises
  [`RcppCCTZ::parseDatetime`](https://rdrr.io/pkg/RcppCCTZ/man/parseDatetime.html)
  as the main function for transforming timestamps.

- ...:

  Not used (S3 method only).

## Value

Returns a [`DTSg`](https://gisler.github.io/DTSg/dev/reference/DTSg.md)
object.

## Status of missing values

Please note that filtering rows and having or making missing timestamps
explicit equals to setting the values of all other timestamps to
missing. The default value of `na.status` is therefore `"implicit"`. To
simply filter for a consecutive range of a
[`DTSg`](https://gisler.github.io/DTSg/dev/reference/DTSg.md) object
while leaving the `na.status` untouched,
[`alter`](https://gisler.github.io/DTSg/dev/reference/alter.DTSg.md) is
probably the better choice.

## User defined TALFs, TALFs helper data and multiplier

User defined temporal aggregation level functions have to return a
[`POSIXct`](https://rdrr.io/r/base/DateTimeClasses.html) vector of the
same length as the time series and accept two arguments: a
[`POSIXct`](https://rdrr.io/r/base/DateTimeClasses.html) vector as its
first and a [`list`](https://rdrr.io/r/base/list.html) with helper data
as its second. The default elements of this
[`list`](https://rdrr.io/r/base/list.html) are as follows:

- *timezone:* Same as the
  [`timezone`](https://gisler.github.io/DTSg/dev/reference/DTSg.md)
  field.

- *ignoreDST:* Same as the `ignoreDST` argument.

- *periodicity:* Same as the
  [`periodicity`](https://gisler.github.io/DTSg/dev/reference/DTSg.md)
  field.

- *na.status:* Same as the
  [`na.status`](https://gisler.github.io/DTSg/dev/reference/DTSg.md)
  field.

- *multiplier:* Same as the `multiplier` argument.

- *funbyApproach:* Same as the `funbyApproach` argument.

- *assertion:* A logical specifying if the TALF is called by an
  assertion.

Any additional element specified in the `funbyHelpers` argument is
appended to the end of the default
[`list`](https://rdrr.io/r/base/list.html). In case `funbyHelpers`
contains an *ignoreDST, multiplier* or *funbyApproach* element, it takes
precedence over the respective method argument. *timezone, periodicity*
and *na.status* elements are rejected, as they are always taken directly
from the object.

The temporal aggregation level of certain
[`TALFs`](https://gisler.github.io/DTSg/dev/reference/TALFs.md) can be
adjusted with the help of the `multiplier` argument. A `multiplier` of
`10`, for example, makes
[`byY_____`](https://gisler.github.io/DTSg/dev/reference/TALFs.md)
aggregate to decades instead of years. Another example is a `multiplier`
of `6` provided to
[`by_m____`](https://gisler.github.io/DTSg/dev/reference/TALFs.md). The
function then aggregates all months of all first and all months of all
second half years instead of all months of all years separately. This
feature is supported by the following
[`TALFs`](https://gisler.github.io/DTSg/dev/reference/TALFs.md) of the
package:

- [`byY_____`](https://gisler.github.io/DTSg/dev/reference/TALFs.md)

- [`byYm____`](https://gisler.github.io/DTSg/dev/reference/TALFs.md)

- [`byYmdH__`](https://gisler.github.io/DTSg/dev/reference/TALFs.md)
  (UTC and equivalent as well as all Etc/GMT time zones only)

- [`byYmdHM_`](https://gisler.github.io/DTSg/dev/reference/TALFs.md)

- [`byYmdHMS`](https://gisler.github.io/DTSg/dev/reference/TALFs.md)

- [`by_m____`](https://gisler.github.io/DTSg/dev/reference/TALFs.md)

- [`by___H__`](https://gisler.github.io/DTSg/dev/reference/TALFs.md)
  (UTC and equivalent as well as all Etc/GMT time zones only)

- [`by____M_`](https://gisler.github.io/DTSg/dev/reference/TALFs.md)

- [`by_____S`](https://gisler.github.io/DTSg/dev/reference/TALFs.md)

## Ignore day saving time

`ignoreDST` tells a temporal aggregation level function if it is
supposed to ignore day saving time while transforming the timestamps.
This can be a desired feature for time series strictly following the
position of the sun such as hydrological time series. Doing so ensures
that diurnal variations are preserved by all means and all intervals are
of the “correct” length, however, a possible limitation might be that
when the day saving time shift cannot be estimated, it is assumed to be
one hour long and a warning is issued. This feature requires that the
periodicity of the time series has been recognised and is supported by
the following
[`TALFs`](https://gisler.github.io/DTSg/dev/reference/TALFs.md) of the
package:

- [`byY_____`](https://gisler.github.io/DTSg/dev/reference/TALFs.md)

- [`byYQ____`](https://gisler.github.io/DTSg/dev/reference/TALFs.md)

- [`byYm____`](https://gisler.github.io/DTSg/dev/reference/TALFs.md)

- [`byYmd___`](https://gisler.github.io/DTSg/dev/reference/TALFs.md)

- [`by_Q____`](https://gisler.github.io/DTSg/dev/reference/TALFs.md)

- [`by_m____`](https://gisler.github.io/DTSg/dev/reference/TALFs.md)

- [`by___H__`](https://gisler.github.io/DTSg/dev/reference/TALFs.md)

## See also

[`cols`](https://gisler.github.io/DTSg/dev/reference/cols.DTSg.md),
[`getOption`](https://rdrr.io/r/base/options.html)

## Examples

``` r
# new DTSg object
x <- DTSg$new(values = flow)

# filter for the first six observations
## R6 method
x$subset(i = 1:6)$print()
#> Values:
#>     .dateTime  flow
#>        <POSc> <num>
#> 1: 2007-01-01 9.540
#> 2: 2007-01-02 9.285
#> 3: 2007-01-03 8.940
#> 4: 2007-01-04 8.745
#> 5: 2007-01-05 8.490
#> 6: 2007-01-06 8.400
#> 
#> Aggregated:     FALSE
#> Regular:        TRUE
#> Periodicity:    Time difference of 1 days
#> Missing values: implicit
#> Time zone:      UTC
#> Timestamps:     6

## S3 method
print(subset(x = x, i = 1:6))
#> Values:
#>     .dateTime  flow
#>        <POSc> <num>
#> 1: 2007-01-01 9.540
#> 2: 2007-01-02 9.285
#> 3: 2007-01-03 8.940
#> 4: 2007-01-04 8.745
#> 5: 2007-01-05 8.490
#> 6: 2007-01-06 8.400
#> 
#> Aggregated:     FALSE
#> Regular:        TRUE
#> Periodicity:    Time difference of 1 days
#> Missing values: implicit
#> Time zone:      UTC
#> Timestamps:     6

# filter for the last two observations per year
## R6 method
x$subset(
  i = (.N - 1):.N,
  funby = function(x, ...) {data.table::year(x)}
)$print()
#> Values:
#>      .dateTime  flow
#>         <POSc> <num>
#>  1: 2007-12-30 11.49
#>  2: 2007-12-31 11.61
#>  3: 2008-12-30 12.54
#>  4: 2008-12-31 11.94
#>  5: 2009-12-30 10.11
#> ---                 
#>  8: 2010-12-31  9.87
#>  9: 2011-12-30  8.04
#> 10: 2011-12-31  7.71
#> 11: 2012-12-30 18.84
#> 12: 2012-12-31 17.25
#> 
#> Aggregated:     FALSE
#> Regular:        TRUE
#> Periodicity:    Time difference of 1 days
#> Missing values: implicit
#> Time zone:      UTC
#> Timestamps:     12

## S3 method
print(subset(
  x = x,
  i = (.N - 1):.N,
  funby = function(x, ...) {data.table::year(x)}
))
#> Values:
#>      .dateTime  flow
#>         <POSc> <num>
#>  1: 2007-12-30 11.49
#>  2: 2007-12-31 11.61
#>  3: 2008-12-30 12.54
#>  4: 2008-12-31 11.94
#>  5: 2009-12-30 10.11
#> ---                 
#>  8: 2010-12-31  9.87
#>  9: 2011-12-30  8.04
#> 10: 2011-12-31  7.71
#> 11: 2012-12-30 18.84
#> 12: 2012-12-31 17.25
#> 
#> Aggregated:     FALSE
#> Regular:        TRUE
#> Periodicity:    Time difference of 1 days
#> Missing values: implicit
#> Time zone:      UTC
#> Timestamps:     12
```
