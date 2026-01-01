# Apply function column-wise

Applies an arbitrary function to selected columns of a
[`DTSg`](https://gisler.github.io/DTSg/dev/reference/DTSg.md) object.

## Usage

``` r
# S3 method for class 'DTSg'
colapply(
  x,
  fun,
  ...,
  cols = self$cols(class = "numeric")[1L],
  resultCols = NULL,
  suffix = NULL,
  helpers = TRUE,
  funby = NULL,
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

- fun:

  A [`function`](https://rdrr.io/r/base/function.html). Its return value
  must be of length one.

- ...:

  Further arguments passed on to `fun`.

- cols:

  A character vector specifying the columns to apply `fun` to. Another
  possibility is a character string containing either comma separated
  column names, for example, `"x,y,z"`, or the start and end column
  separated by a colon, for example, `"x:z"`.

- resultCols:

  An optional character vector of the same length as `cols` specifying
  the column names for the return values of `fun`. Another possibility
  is a character string containing comma separated column names, for
  example, `"x,y,z"`. Non-existing columns are added and existing
  columns are overwritten. Columns are matched element-wise between
  `cols` and `resultCols`.

- suffix:

  An optional character string. The return values of `fun` are added as
  new columns with names consisting of the columns specified in `cols`
  and this suffix. Existing columns are never overwritten. Only used
  when `resultCols` is not specified.

- helpers:

  A logical specifying if helper data shall be handed over to `fun`. See
  corresponding section for further information.

- funby:

  One of the temporal aggregation level functions described in
  [`TALFs`](https://gisler.github.io/DTSg/dev/reference/TALFs.md) or a
  user defined temporal aggregation level function. Can be used to apply
  functions like [`cumsum`](https://rdrr.io/r/base/cumsum.html) to a
  certain temporal level. See corresponding section and examples for
  further information.

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

Returns a [`DTSg`](https://gisler.github.io/DTSg/dev/reference/DTSg.md)
object.

## Helper data

In addition to the `...` argument, this method optionally hands over a
[`list`](https://rdrr.io/r/base/list.html) argument with helper data
called `.helpers` to `fun`. This
[`list`](https://rdrr.io/r/base/list.html) contains the following
elements:

- *.dateTime:* A
  [`POSIXct`](https://rdrr.io/r/base/DateTimeClasses.html) vector
  containing the *.dateTime* column.

- *periodicity:* Same as the
  [`periodicity`](https://gisler.github.io/DTSg/dev/reference/DTSg.md)
  field.

- *minLag:* A [`difftime`](https://rdrr.io/r/base/difftime.html) object
  containing the minimum time difference between two subsequent
  timestamps.

- *maxLag:* A [`difftime`](https://rdrr.io/r/base/difftime.html) object
  containing the maximum time difference between two subsequent
  timestamps.

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

# linear interpolation of missing values
## R6 method
x$colapply(fun = interpolateLinear)$print()
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
#> Aggregated:     FALSE
#> Regular:        TRUE
#> Periodicity:    Time difference of 1 days
#> Missing values: explicit
#> Time zone:      UTC
#> Timestamps:     2192

## S3 method
print(colapply(x = x, fun = interpolateLinear))
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
#> Aggregated:     FALSE
#> Regular:        TRUE
#> Periodicity:    Time difference of 1 days
#> Missing values: explicit
#> Time zone:      UTC
#> Timestamps:     2192

# daily cumulative sums per month
## R6 method
x$colapply(
  fun = cumsum,
  helpers = FALSE,
  funby = byYm____
)$print()
#> Values:
#>        .dateTime    flow
#>           <POSc>   <num>
#>    1: 2007-01-01   9.540
#>    2: 2007-01-02  18.825
#>    3: 2007-01-03  27.765
#>    4: 2007-01-04  36.510
#>    5: 2007-01-05  45.000
#>   ---                   
#> 2188: 2012-12-27 376.185
#> 2189: 2012-12-28 404.235
#> 2190: 2012-12-29 427.815
#> 2191: 2012-12-30 446.655
#> 2192: 2012-12-31 463.905
#> 
#> Aggregated:     FALSE
#> Regular:        TRUE
#> Periodicity:    Time difference of 1 days
#> Missing values: explicit
#> Time zone:      UTC
#> Timestamps:     2192

## S3 method
print(colapply(
  x = x,
  fun = cumsum,
  helpers = FALSE,
  funby = byYm____
))
#> Values:
#>        .dateTime    flow
#>           <POSc>   <num>
#>    1: 2007-01-01   9.540
#>    2: 2007-01-02  18.825
#>    3: 2007-01-03  27.765
#>    4: 2007-01-04  36.510
#>    5: 2007-01-05  45.000
#>   ---                   
#> 2188: 2012-12-27 376.185
#> 2189: 2012-12-28 404.235
#> 2190: 2012-12-29 427.815
#> 2191: 2012-12-30 446.655
#> 2192: 2012-12-31 463.905
#> 
#> Aggregated:     FALSE
#> Regular:        TRUE
#> Periodicity:    Time difference of 1 days
#> Missing values: explicit
#> Time zone:      UTC
#> Timestamps:     2192

# calculate moving averages with the help of 'runner' (all four given
# approaches provide the same result with explicitly missing timestamps)
if (requireNamespace("runner", quietly = TRUE) &&
    packageVersion("runner") >= package_version("0.3.5")) {
  wrapper <- function(..., .helpers) {
    runner::runner(..., idx = .helpers[[".dateTime"]])
  }

  ## R6 method
  x$colapply(
    fun = runner::runner,
    f = mean,
    k = 5,
    lag = -2
  )$print()
  x$colapply(
    fun = wrapper,
    f = mean,
    k = "5 days",
    lag = "-2 days"
  )$print()
  x$colapply(
    fun = runner::runner,
    f = mean,
    k = "5 days",
    lag = "-2 days",
    idx = x$getCol(col = ".dateTime")
  )$print()
  x$colapply(
    fun = runner::runner,
    f = mean,
    k = "5 days",
    lag = "-2 days",
    idx = x[".dateTime"]
  )$print()

  ## S3 method
  print(colapply(
    x = x,
    fun = runner::runner,
    f = mean,
    k = 5,
    lag = -2
  ))
  print(colapply(
    x = x,
    fun = wrapper,
    f = mean,
    k = "5 days",
    lag = "-2 days"
  ))
  print(colapply(
    x = x,
    fun = runner::runner,
    f = mean,
    k = "5 days",
    lag = "-2 days",
    idx = getCol(x = x, col = ".dateTime")
  ))
  print(colapply(
    x = x,
    fun = runner::runner,
    f = mean,
    k = "5 days",
    lag = "-2 days",
    idx = x[".dateTime"]
  ))
}
#> Values:
#>        .dateTime    flow
#>           <POSc>   <num>
#>    1: 2007-01-01  9.2550
#>    2: 2007-01-02  9.1275
#>    3: 2007-01-03  9.0000
#>    4: 2007-01-04  8.7720
#>    5: 2007-01-05  8.5710
#>   ---                   
#> 2188: 2012-12-27 28.9860
#> 2189: 2012-12-28 25.3200
#> 2190: 2012-12-29 22.8810
#> 2191: 2012-12-30 21.9300
#> 2192: 2012-12-31 19.8900
#> 
#> Aggregated:     FALSE
#> Regular:        TRUE
#> Periodicity:    Time difference of 1 days
#> Missing values: explicit
#> Time zone:      UTC
#> Timestamps:     2192
#> Values:
#>        .dateTime    flow
#>           <POSc>   <num>
#>    1: 2007-01-01  9.2550
#>    2: 2007-01-02  9.1275
#>    3: 2007-01-03  9.0000
#>    4: 2007-01-04  8.7720
#>    5: 2007-01-05  8.5710
#>   ---                   
#> 2188: 2012-12-27 28.9860
#> 2189: 2012-12-28 25.3200
#> 2190: 2012-12-29 22.8810
#> 2191: 2012-12-30 21.9300
#> 2192: 2012-12-31 19.8900
#> 
#> Aggregated:     FALSE
#> Regular:        TRUE
#> Periodicity:    Time difference of 1 days
#> Missing values: explicit
#> Time zone:      UTC
#> Timestamps:     2192
#> Values:
#>        .dateTime    flow
#>           <POSc>   <num>
#>    1: 2007-01-01  9.2550
#>    2: 2007-01-02  9.1275
#>    3: 2007-01-03  9.0000
#>    4: 2007-01-04  8.7720
#>    5: 2007-01-05  8.5710
#>   ---                   
#> 2188: 2012-12-27 28.9860
#> 2189: 2012-12-28 25.3200
#> 2190: 2012-12-29 22.8810
#> 2191: 2012-12-30 21.9300
#> 2192: 2012-12-31 19.8900
#> 
#> Aggregated:     FALSE
#> Regular:        TRUE
#> Periodicity:    Time difference of 1 days
#> Missing values: explicit
#> Time zone:      UTC
#> Timestamps:     2192
#> Values:
#>        .dateTime    flow
#>           <POSc>   <num>
#>    1: 2007-01-01  9.2550
#>    2: 2007-01-02  9.1275
#>    3: 2007-01-03  9.0000
#>    4: 2007-01-04  8.7720
#>    5: 2007-01-05  8.5710
#>   ---                   
#> 2188: 2012-12-27 28.9860
#> 2189: 2012-12-28 25.3200
#> 2190: 2012-12-29 22.8810
#> 2191: 2012-12-30 21.9300
#> 2192: 2012-12-31 19.8900
#> 
#> Aggregated:     FALSE
#> Regular:        TRUE
#> Periodicity:    Time difference of 1 days
#> Missing values: explicit
#> Time zone:      UTC
#> Timestamps:     2192
#> Values:
#>        .dateTime    flow
#>           <POSc>   <num>
#>    1: 2007-01-01  9.2550
#>    2: 2007-01-02  9.1275
#>    3: 2007-01-03  9.0000
#>    4: 2007-01-04  8.7720
#>    5: 2007-01-05  8.5710
#>   ---                   
#> 2188: 2012-12-27 28.9860
#> 2189: 2012-12-28 25.3200
#> 2190: 2012-12-29 22.8810
#> 2191: 2012-12-30 21.9300
#> 2192: 2012-12-31 19.8900
#> 
#> Aggregated:     FALSE
#> Regular:        TRUE
#> Periodicity:    Time difference of 1 days
#> Missing values: explicit
#> Time zone:      UTC
#> Timestamps:     2192
#> Error in eval(call): object 'wrapper' not found

# calculate rolling correlations somewhat inefficiently with the help of
# 'runner'
if (requireNamespace("runner", quietly = TRUE) &&
    packageVersion("runner") >= package_version("0.3.8")) {
  wrapper <- function(x, y, f, k, lag, ...) {
    runner::runner(
      cbind(x, y),
      f = function(x) f(x[, 1], x[, 2]),
      k = k,
      lag = lag
    )
  }

  ## R6 method
  x$colapply(
    fun = wrapper,
    y = x["flow"] + rnorm(length(x["flow"])),
    f = cor,
    k = 5,
    lag = -2
  )$print()

  ## S3 method
  print(colapply(
    x = x,
    fun = wrapper,
    y = x["flow"] + rnorm(length(x["flow"])),
    f = cor,
    k = 5,
    lag = -2
  ))
}
#> Values:
#>        .dateTime        flow
#>           <POSc>       <num>
#>    1: 2007-01-01 -0.65071468
#>    2: 2007-01-02 -0.43767757
#>    3: 2007-01-03  0.01808569
#>    4: 2007-01-04  0.41567684
#>    5: 2007-01-05 -0.53300739
#>   ---                       
#> 2188: 2012-12-27  0.98727366
#> 2189: 2012-12-28  0.98700468
#> 2190: 2012-12-29  0.98780552
#> 2191: 2012-12-30  0.98822407
#> 2192: 2012-12-31  0.99540697
#> 
#> Aggregated:     FALSE
#> Regular:        TRUE
#> Periodicity:    Time difference of 1 days
#> Missing values: explicit
#> Time zone:      UTC
#> Timestamps:     2192
#> Error in eval(call): object 'wrapper' not found
```
