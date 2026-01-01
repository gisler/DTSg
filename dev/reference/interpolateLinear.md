# Linear interpolation

Linearly interpolates missing values of a numeric vector. For use with
the
[`colapply`](https://gisler.github.io/DTSg/dev/reference/colapply.DTSg.md)
method of [`DTSg`](https://gisler.github.io/DTSg/dev/reference/DTSg.md)
objects. Other uses are possible, but not recommended.

This [`function`](https://rdrr.io/r/base/function.html) mainly serves as
an example for writing user defined
[`function`](https://rdrr.io/r/base/function.html)s utilising one of the
[`list`](https://rdrr.io/r/base/list.html)s with helper data handed over
by some of the methods of
[`DTSg`](https://gisler.github.io/DTSg/dev/reference/DTSg.md) objects.

## Usage

``` r
interpolateLinear(.col, roll = Inf, rollends = TRUE, .helpers)
```

## Arguments

- .col:

  A numeric vector.

- roll:

  A positive numeric specifying the maximum size of gaps whose missing
  values shall be interpolated. For time series with unrecognised
  periodicity it is interpreted in seconds and for time series with
  recognised periodicity it is multiplied with the maximum time
  difference between two subsequent time steps in seconds. Thus, for
  regular time series it is the number of time steps and for irregular
  it is an approximation of it.

- rollends:

  A logical specifying if missing values at the start and end of the
  time series shall be filled. See
  [`data.table::data.table`](https://rdatatable.gitlab.io/data.table/reference/data.table.html)
  for further information.

- .helpers:

  A [`list`](https://rdrr.io/r/base/list.html) with helper data as
  handed over by
  [`colapply`](https://gisler.github.io/DTSg/dev/reference/colapply.DTSg.md).
  See
  [`colapply`](https://gisler.github.io/DTSg/dev/reference/colapply.DTSg.md)
  for further information.

## Value

Returns a numeric vector.

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
```
