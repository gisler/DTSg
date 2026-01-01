# Merge two objects

Joins two [`DTSg`](https://gisler.github.io/DTSg/dev/reference/DTSg.md)
objects based on their *.dateTime* column. Their time zones and
`aggregated` fields must match.

## Usage

``` r
# S3 method for class 'DTSg'
merge(x, y, ..., clone = getOption("DTSgClone"))
```

## Arguments

- x:

  A [`DTSg`](https://gisler.github.io/DTSg/dev/reference/DTSg.md) object
  (S3 method only).

- y:

  A [`DTSg`](https://gisler.github.io/DTSg/dev/reference/DTSg.md) object
  or an object coercible to one. See
  [`new`](https://gisler.github.io/DTSg/dev/reference/DTSg.md) for
  further information.

- ...:

  Further arguments passed on to
  [`data.table::merge`](https://rdatatable.gitlab.io/data.table/reference/merge.html).
  As the `by`, `by.x` and `by.y` arguments can endanger the object's
  integrity, they are rejected.

- clone:

  A logical specifying if the object shall be modified in place or if a
  deep clone (copy) shall be made beforehand.

## Value

Returns a [`DTSg`](https://gisler.github.io/DTSg/dev/reference/DTSg.md)
object.

## See also

[`getOption`](https://rdrr.io/r/base/options.html)

## Examples

``` r
# new DTSg object
x <- DTSg$new(values = flow)

# merge with 'data.table'
## R6 method
x$merge(
  y = flow,
  suffixes = c("_1", "_2")
)$print()
#> Values:
#>        .dateTime flow_1 flow_2
#>           <POSc>  <num>  <num>
#>    1: 2007-01-01  9.540  9.540
#>    2: 2007-01-02  9.285  9.285
#>    3: 2007-01-03  8.940  8.940
#>    4: 2007-01-04  8.745  8.745
#>    5: 2007-01-05  8.490  8.490
#>   ---                         
#> 2188: 2012-12-27 26.685 26.685
#> 2189: 2012-12-28 28.050 28.050
#> 2190: 2012-12-29 23.580 23.580
#> 2191: 2012-12-30 18.840 18.840
#> 2192: 2012-12-31 17.250 17.250
#> 
#> Aggregated:     FALSE
#> Regular:        TRUE
#> Periodicity:    Time difference of 1 days
#> Missing values: explicit
#> Time zone:      UTC
#> Timestamps:     2192

## S3 method
print(merge(
  x = x,
  y = flow,
  suffixes = c("_1", "_2")
))
#> Values:
#>        .dateTime flow_1 flow_2
#>           <POSc>  <num>  <num>
#>    1: 2007-01-01  9.540  9.540
#>    2: 2007-01-02  9.285  9.285
#>    3: 2007-01-03  8.940  8.940
#>    4: 2007-01-04  8.745  8.745
#>    5: 2007-01-05  8.490  8.490
#>   ---                         
#> 2188: 2012-12-27 26.685 26.685
#> 2189: 2012-12-28 28.050 28.050
#> 2190: 2012-12-29 23.580 23.580
#> 2191: 2012-12-30 18.840 18.840
#> 2192: 2012-12-31 17.250 17.250
#> 
#> Aggregated:     FALSE
#> Regular:        TRUE
#> Periodicity:    Time difference of 1 days
#> Missing values: explicit
#> Time zone:      UTC
#> Timestamps:     2192
```
