# Combine rows

Combines the rows of
[`DTSg`](https://gisler.github.io/DTSg/dev/reference/DTSg.md) and other
suitable objects.

## Usage

``` r
# S3 method for class 'DTSg'
rowbind(x, ..., clone = getOption("DTSgClone"))
```

## Arguments

- x:

  A [`DTSg`](https://gisler.github.io/DTSg/dev/reference/DTSg.md) object
  (S3 method only).

- ...:

  Any number of
  [`DTSg`](https://gisler.github.io/DTSg/dev/reference/DTSg.md) objects
  or objects coercible to one (see
  [`new`](https://gisler.github.io/DTSg/dev/reference/DTSg.md) for
  further information). [`list`](https://rdrr.io/r/base/list.html)s of
  such objects or a mixture of lists and non-lists are also accepted.

- clone:

  A logical specifying if the object shall be modified in place or if a
  deep clone (copy) shall be made beforehand.

## Value

Returns a [`DTSg`](https://gisler.github.io/DTSg/dev/reference/DTSg.md)
object.

## R6 alias

The `rbind()` alias for this method is exclusively available in the R6
interface.

## See also

[`cols`](https://gisler.github.io/DTSg/dev/reference/cols.DTSg.md),
[`getOption`](https://rdrr.io/r/base/options.html)

## Examples

``` r
# new DTSg object
x <- DTSg$new(values = flow[1:500, ])

# combine rows
## R6 method
x$rowbind(
  list(flow[1001:1500, ], DTSg$new(values = flow[501:1000, ])),
  flow[1501:.N, ]
)$print()
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

## 'rbind()' is an R6 alias for 'rowbind()'
x$rbind(
  list(flow[1001:1500, ], DTSg$new(values = flow[501:1000, ])),
  flow[1501:.N, ]
)$print()
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
print(rowbind(
  x = x,
  list(flow[1001:1500, ], DTSg$new(values = flow[501:1000, ])),
  flow[1501:.N, ]
))
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
