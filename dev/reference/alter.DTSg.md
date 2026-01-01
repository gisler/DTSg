# Alter time series

Shortens, lengthens, filters for a consecutive range, changes the
periodicity and/or the status of missing values of a
[`DTSg`](https://gisler.github.io/DTSg/dev/reference/DTSg.md) object.

## Usage

``` r
# S3 method for class 'DTSg'
alter(
  x,
  from = first(self$values(reference = TRUE)[[".dateTime"]]),
  to = last(self$values(reference = TRUE)[[".dateTime"]]),
  by = self$periodicity,
  rollback = TRUE,
  clone = getOption("DTSgClone"),
  na.status = self$na.status,
  ...
)
```

## Arguments

- x:

  A [`DTSg`](https://gisler.github.io/DTSg/dev/reference/DTSg.md) object
  (S3 method only).

- from:

  A [`POSIXct`](https://rdrr.io/r/base/DateTimeClasses.html) timestamp
  in the same time zone as the time series or a character string
  coercible to one. Specifies the new start of the time series.

- to:

  A [`POSIXct`](https://rdrr.io/r/base/DateTimeClasses.html) timestamp
  in the same time zone as the time series or a character string
  coercible to one. Specifies the new end of the time series.

- by:

  Specifies the new periodicity in one of the ways the `by` argument of
  [`seq.POSIXt`](https://rdrr.io/r/base/seq.POSIXt.html) can be
  specified. Must be specified for time series with unrecognised
  periodicity. Time steps out of sync with the new periodicity are
  dropped.

- rollback:

  A logical specifying if a call to
  [`rollback`](https://gisler.github.io/DTSg/dev/reference/rollback.md)
  shall be made when appropriate.

- clone:

  A logical specifying if the object shall be modified in place or if a
  deep clone (copy) shall be made beforehand.

- na.status:

  A character string. Either `"explicit"`, which makes missing
  timestamps explicit according to the recognised periodicity, or
  `"implicit"`, which removes timestamps with missing values on all
  value columns. Please note that
  [`DTSg`](https://gisler.github.io/DTSg/dev/reference/DTSg.md) objects
  work best with explicitly missing values.

- ...:

  Not used (S3 method only).

## Value

Returns a [`DTSg`](https://gisler.github.io/DTSg/dev/reference/DTSg.md)
object.

## See also

[`getOption`](https://rdrr.io/r/base/options.html),
[`subset`](https://gisler.github.io/DTSg/dev/reference/subset.DTSg.md),
[`nas`](https://gisler.github.io/DTSg/dev/reference/nas.DTSg.md)

## Examples

``` r
# new DTSg object
x <- DTSg$new(values = flow)

# filter for the first two years
## R6 method
x$alter(
  from = "2007-01-01",
  to = "2008-12-31"
)$print()
#> Values:
#>       .dateTime   flow
#>          <POSc>  <num>
#>   1: 2007-01-01  9.540
#>   2: 2007-01-02  9.285
#>   3: 2007-01-03  8.940
#>   4: 2007-01-04  8.745
#>   5: 2007-01-05  8.490
#>  ---                  
#> 727: 2008-12-27 18.180
#> 728: 2008-12-28 16.575
#> 729: 2008-12-29 13.695
#> 730: 2008-12-30 12.540
#> 731: 2008-12-31 11.940
#> 
#> Aggregated:     FALSE
#> Regular:        TRUE
#> Periodicity:    Time difference of 1 days
#> Missing values: explicit
#> Time zone:      UTC
#> Timestamps:     731

## S3 method
print(alter(
  x = x,
  from = "2007-01-01",
  to = "2008-12-31"
))
#> Values:
#>       .dateTime   flow
#>          <POSc>  <num>
#>   1: 2007-01-01  9.540
#>   2: 2007-01-02  9.285
#>   3: 2007-01-03  8.940
#>   4: 2007-01-04  8.745
#>   5: 2007-01-05  8.490
#>  ---                  
#> 727: 2008-12-27 18.180
#> 728: 2008-12-28 16.575
#> 729: 2008-12-29 13.695
#> 730: 2008-12-30 12.540
#> 731: 2008-12-31 11.940
#> 
#> Aggregated:     FALSE
#> Regular:        TRUE
#> Periodicity:    Time difference of 1 days
#> Missing values: explicit
#> Time zone:      UTC
#> Timestamps:     731

# change periodicity to one month
## R6 method
x$alter(by = "1 month")$print()
#> Values:
#>      .dateTime   flow
#>         <POSc>  <num>
#>  1: 2007-01-01  9.540
#>  2: 2007-02-01 11.055
#>  3: 2007-03-01  8.865
#>  4: 2007-04-01 16.770
#>  5: 2007-05-01  8.355
#> ---                  
#> 68: 2012-08-01 11.325
#> 69: 2012-09-01  7.365
#> 70: 2012-10-01  7.140
#> 71: 2012-11-01 10.890
#> 72: 2012-12-01  5.475
#> 
#> Aggregated:     FALSE
#> Regular:        FALSE
#> Periodicity:    1 months
#> Min lag:        Time difference of 28 days
#> Max lag:        Time difference of 31 days
#> Missing values: explicit
#> Time zone:      UTC
#> Timestamps:     72

## S3 method
print(alter(x = x, by = "1 month"))
#> Values:
#>      .dateTime   flow
#>         <POSc>  <num>
#>  1: 2007-01-01  9.540
#>  2: 2007-02-01 11.055
#>  3: 2007-03-01  8.865
#>  4: 2007-04-01 16.770
#>  5: 2007-05-01  8.355
#> ---                  
#> 68: 2012-08-01 11.325
#> 69: 2012-09-01  7.365
#> 70: 2012-10-01  7.140
#> 71: 2012-11-01 10.890
#> 72: 2012-12-01  5.475
#> 
#> Aggregated:     FALSE
#> Regular:        FALSE
#> Periodicity:    1 months
#> Min lag:        Time difference of 28 days
#> Max lag:        Time difference of 31 days
#> Missing values: explicit
#> Time zone:      UTC
#> Timestamps:     72
```
