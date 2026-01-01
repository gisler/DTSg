# Rollback of months

Generating regular sequences of time with the help of
[`seq.POSIXt`](https://rdrr.io/r/base/seq.POSIXt.html) can have
undesirable effects. This function “first advances the month without
changing the day: if this results in an invalid day of the month, it is
counted forward into the next month”. Monthly or yearly sequences
starting at the end of a month with 30 or 31 days (or 29 in case of a
leap year) therefore do not always fall on the end of shorter months.
`rollback` fixes this by counting the days of affected months backwards
again.

## Usage

``` r
rollback(.dateTime, periodicity)
```

## Arguments

- .dateTime:

  A [`POSIXct`](https://rdrr.io/r/base/DateTimeClasses.html) vector.

- periodicity:

  A character string specifying a multiple of month(s) or year(s). See
  [`seq.POSIXt`](https://rdrr.io/r/base/seq.POSIXt.html) for further
  information.

## Value

Returns a [`POSIXct`](https://rdrr.io/r/base/DateTimeClasses.html)
vector.

## Examples

``` r
# rollback monthly time series
by <- "1 month"
rollback(
  .dateTime = seq(
    from = as.POSIXct("2000-01-31", tz = "UTC"),
    to = as.POSIXct("2000-12-31", tz = "UTC"),
    by = by
  ),
  periodicity = by
)
#>  [1] "2000-01-31 UTC" "2000-02-29 UTC" "2000-03-31 UTC" "2000-04-30 UTC"
#>  [5] "2000-05-31 UTC" "2000-06-30 UTC" "2000-07-31 UTC" "2000-08-31 UTC"
#>  [9] "2000-09-30 UTC" "2000-10-31 UTC" "2000-11-30 UTC" "2000-12-31 UTC"
```
