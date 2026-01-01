# Aggregate values

Applies a temporal aggregation level function to the *.dateTime* column
of a [`DTSg`](https://gisler.github.io/DTSg/dev/reference/DTSg.md)
object and aggregates its values column-wise to the function's temporal
aggregation level utilising one or more provided summary functions.
Additionally, it sets the object's
[`aggregated`](https://gisler.github.io/DTSg/dev/reference/DTSg.md)
field to `TRUE`.

## Usage

``` r
# S3 method for class 'DTSg'
aggregate(
  x,
  funby,
  fun,
  ...,
  cols = self$cols(class = "numeric"),
  n = FALSE,
  ignoreDST = FALSE,
  multiplier = 1L,
  funbyHelpers = NULL,
  funbyApproach = self$funbyApproach,
  clone = getOption("DTSgClone")
)
```

## Arguments

- x:

  A [`DTSg`](https://gisler.github.io/DTSg/dev/reference/DTSg.md) object
  (S3 method only).

- funby:

  One of the temporal aggregation level functions described in
  [`TALFs`](https://gisler.github.io/DTSg/dev/reference/TALFs.md) or a
  user defined temporal aggregation level function. See corresponding
  section for further information.

- fun:

  A summary function, (named) [`list`](https://rdrr.io/r/base/list.html)
  of summary functions or (named) character vector specifying summary
  functions applied column-wise to all the values of the same temporal
  aggregation level. The return value(s) must be of length one. See
  corresponding section for further information.

- ...:

  Further arguments passed on to `fun`.

- cols:

  A character vector specifying the columns to aggregate. Another
  possibility is a character string containing either comma separated
  column names, for example, `"x,y,z"`, or the start and end column
  separated by a colon, for example, `"x:z"`.

- n:

  A logical specifying if a column named `.n` giving the number of
  values per temporal aggregation level shall be added. See
  corresponding section for further information.

- ignoreDST:

  A logical specifying if day saving time shall be ignored by `funby`.
  See corresponding section for further information.

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

- clone:

  A logical specifying if the object shall be modified in place or if a
  deep clone (copy) shall be made beforehand.

## Value

Returns an aggregated
[`DTSg`](https://gisler.github.io/DTSg/dev/reference/DTSg.md) object.

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

## Summary functions

Some examples for `fun` are as follows:

- [`mean`](https://rdrr.io/r/base/mean.html)

- [`list`](https://rdrr.io/r/base/list.html)`(min = `[`min`](https://rdrr.io/r/base/Extremes.html)`, max = `[`max`](https://rdrr.io/r/base/Extremes.html)`)`

- `c(sd = "sd", var = "var")`

A [`list`](https://rdrr.io/r/base/list.html) or character vector must
have names in case more than one summary function is provided. The
method can benefit from data.table's
*[GForce](https://rdatatable.gitlab.io/data.table/reference/datatable-optimize.html)*
optimisation in case a character vector specifying summary functions is
provided.

## Number of values per temporal aggregation level

Depending on the number of columns to aggregate, the `.n` column
contains different counts:

- One column: The counts are calculated from the columns' values
  disregarding any missing values.

- More than one column: The counts are calculated from the *.dateTime*
  column including all missing values.

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

# mean yearly river flows
## R6 method
x$aggregate(
  funby = byY_____,
  fun = "mean",
  na.rm = TRUE
)$print()
#> Values:
#>     .dateTime     flow
#>        <POSc>    <num>
#> 1: 2007-01-01 14.40711
#> 2: 2008-01-01 15.75086
#> 3: 2009-01-01 23.10053
#> 4: 2010-01-01 16.15381
#> 5: 2011-01-01 12.23289
#> 6: 2012-01-01 15.42943
#> 
#> Aggregated:     TRUE
#> Regular:        FALSE
#> Periodicity:    1 years
#> Min lag:        Time difference of 365 days
#> Max lag:        Time difference of 366 days
#> Missing values: explicit
#> Time zone:      UTC
#> Timestamps:     6

## S3 method
print(aggregate(
  x = x,
  funby = byY_____,
  fun = "mean",
  na.rm = TRUE
))
#> Values:
#>     .dateTime     flow
#>        <POSc>    <num>
#> 1: 2007-01-01 14.40711
#> 2: 2008-01-01 15.75086
#> 3: 2009-01-01 23.10053
#> 4: 2010-01-01 16.15381
#> 5: 2011-01-01 12.23289
#> 6: 2012-01-01 15.42943
#> 
#> Aggregated:     TRUE
#> Regular:        FALSE
#> Periodicity:    1 years
#> Min lag:        Time difference of 365 days
#> Max lag:        Time difference of 366 days
#> Missing values: explicit
#> Time zone:      UTC
#> Timestamps:     6

# variance and standard deviation of river flows per quarter
## R6 method
x$aggregate(
  funby = byYQ____,
  fun = c(var = "var", sd = "sd"),
  na.rm = TRUE
)$print()
#> Values:
#>      .dateTime  flow.var   flow.sd
#>         <POSc>     <num>     <num>
#>  1: 2007-01-01 453.86416 21.304088
#>  2: 2007-04-01  11.39625  3.375833
#>  3: 2007-07-01  29.19673  5.403400
#>  4: 2007-10-01  77.37213  8.796143
#>  5: 2008-01-01  68.45223  8.273586
#> ---                               
#> 20: 2011-10-01 143.44106 11.976688
#> 21: 2012-01-01 208.99699 14.456728
#> 22: 2012-04-01  44.17733  6.646603
#> 23: 2012-07-01  36.50542  6.041972
#> 24: 2012-10-01  97.72395  9.885543
#> 
#> Aggregated:     TRUE
#> Regular:        FALSE
#> Periodicity:    3 months
#> Min lag:        Time difference of 90 days
#> Max lag:        Time difference of 92 days
#> Missing values: explicit
#> Time zone:      UTC
#> Timestamps:     24

## S3 method
print(aggregate(
  x = x,
  funby = byYQ____,
  fun = c(var = "var", sd = "sd"),
  na.rm = TRUE
))
#> Values:
#>      .dateTime  flow.var   flow.sd
#>         <POSc>     <num>     <num>
#>  1: 2007-01-01 453.86416 21.304088
#>  2: 2007-04-01  11.39625  3.375833
#>  3: 2007-07-01  29.19673  5.403400
#>  4: 2007-10-01  77.37213  8.796143
#>  5: 2008-01-01  68.45223  8.273586
#> ---                               
#> 20: 2011-10-01 143.44106 11.976688
#> 21: 2012-01-01 208.99699 14.456728
#> 22: 2012-04-01  44.17733  6.646603
#> 23: 2012-07-01  36.50542  6.041972
#> 24: 2012-10-01  97.72395  9.885543
#> 
#> Aggregated:     TRUE
#> Regular:        FALSE
#> Periodicity:    3 months
#> Min lag:        Time difference of 90 days
#> Max lag:        Time difference of 92 days
#> Missing values: explicit
#> Time zone:      UTC
#> Timestamps:     24

# mean of river flows of all first and all second half years
## R6 method
x$aggregate(
  funby = by_m____,
  fun = "mean",
  na.rm = TRUE,
  multiplier = 6
)$print()
#> Values:
#>     .dateTime     flow
#>        <POSc>    <num>
#> 1: 2199-01-01 19.32837
#> 2: 2199-07-01 13.04606
#> 
#> Aggregated:     TRUE
#> Regular:        TRUE
#> Periodicity:    Time difference of 181 days
#> Missing values: explicit
#> Time zone:      UTC
#> Timestamps:     2

## S3 method
print(aggregate(
  x = x,
  funby = by_m____,
  fun = "mean",
  na.rm = TRUE,
  multiplier = 6
))
#> Values:
#>     .dateTime     flow
#>        <POSc>    <num>
#> 1: 2199-01-01 19.32837
#> 2: 2199-07-01 13.04606
#> 
#> Aggregated:     TRUE
#> Regular:        TRUE
#> Periodicity:    Time difference of 181 days
#> Missing values: explicit
#> Time zone:      UTC
#> Timestamps:     2
```
