# Clone object

Clones (copies) a
[`DTSg`](https://gisler.github.io/DTSg/dev/reference/DTSg.md) object.
Merely assigning a variable representing a
[`DTSg`](https://gisler.github.io/DTSg/dev/reference/DTSg.md) object to
a new variable does not result in a copy of the object. Instead, both
variables will reference and access the same data under the hood, i.e.
changing one will also affect the other. This is not an issue when
calling methods with the *DTSgClone* option or `clone` argument set to
`TRUE`, but has to be kept in mind when setting fields, as they are
always modified in place. See
[`DTSg`](https://gisler.github.io/DTSg/dev/reference/DTSg.md) for
further information.

## Usage

``` r
# S3 method for class 'DTSg'
clone(x, deep = FALSE, ...)
```

## Arguments

- x:

  A [`DTSg`](https://gisler.github.io/DTSg/dev/reference/DTSg.md) object
  (S3 method only).

- deep:

  A logical specifying if a deep copy shall be made (for consistency
  with the [`R6::R6Class`](https://r6.r-lib.org/reference/R6Class.html)
  the default is `FALSE`, but should generally be set to `TRUE`).

- ...:

  Not used (S3 method only).

## Value

Returns a cloned
[`DTSg`](https://gisler.github.io/DTSg/dev/reference/DTSg.md) object.

## See also

[`options`](https://rdrr.io/r/base/options.html)

## Examples

``` r
# new DTSg object
x <- DTSg$new(values = flow)

# make a deep copy
## R6 method
x$clone(deep = TRUE)
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
clone(x = x, deep = TRUE)
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
