# Set column names

Changes the column names of
[`DTSg`](https://gisler.github.io/DTSg/dev/reference/DTSg.md) objects.

## Usage

``` r
# S3 method for class 'DTSg'
setColNames(
  x,
  cols = self$cols(class = "numeric")[1L],
  values,
  clone = getOption("DTSgClone"),
  ...
)
```

## Arguments

- x:

  A [`DTSg`](https://gisler.github.io/DTSg/dev/reference/DTSg.md) object
  (S3 method only).

- cols:

  A character vector specifying the columns whose names shall be set.
  Another possibility is a character string containing either comma
  separated column names, for example, `"x,y,z"`, or the start and end
  column separated by a colon, for example, `"x:z"`. The name of the
  *.dateTime* column cannot be changed.

- values:

  A character vector of the same length as `cols` specifying the desired
  column names. Another possibility is a character string containing
  comma separated column names, for example, `"x,y,z"`.

- clone:

  A logical specifying if the object shall be modified in place or if a
  deep clone (copy) shall be made beforehand.

- ...:

  Not used (S3 method only).

## Value

Returns a [`DTSg`](https://gisler.github.io/DTSg/dev/reference/DTSg.md)
object.

## R6 alias

The `setnames()` alias for this method is exclusively available in the
R6 interface.

## See also

[`cols`](https://gisler.github.io/DTSg/dev/reference/cols.DTSg.md),
[`getOption`](https://rdrr.io/r/base/options.html)

## Examples

``` r
# new DTSg object
x <- DTSg$new(values = flow)

# rename column "flow" to "River Flow"
## R6 method
x$setColNames(
  cols = "flow",
  values = "River Flow"
)$print()
#> Values:
#>        .dateTime River Flow
#>           <POSc>      <num>
#>    1: 2007-01-01      9.540
#>    2: 2007-01-02      9.285
#>    3: 2007-01-03      8.940
#>    4: 2007-01-04      8.745
#>    5: 2007-01-05      8.490
#>   ---                      
#> 2188: 2012-12-27     26.685
#> 2189: 2012-12-28     28.050
#> 2190: 2012-12-29     23.580
#> 2191: 2012-12-30     18.840
#> 2192: 2012-12-31     17.250
#> 
#> Aggregated:     FALSE
#> Regular:        TRUE
#> Periodicity:    Time difference of 1 days
#> Missing values: explicit
#> Time zone:      UTC
#> Timestamps:     2192

## 'setnames()' is an R6 alias for 'setColNames()'
x$setnames(
  cols = "flow",
  values = "River Flow"
)$print()
#> Values:
#>        .dateTime River Flow
#>           <POSc>      <num>
#>    1: 2007-01-01      9.540
#>    2: 2007-01-02      9.285
#>    3: 2007-01-03      8.940
#>    4: 2007-01-04      8.745
#>    5: 2007-01-05      8.490
#>   ---                      
#> 2188: 2012-12-27     26.685
#> 2189: 2012-12-28     28.050
#> 2190: 2012-12-29     23.580
#> 2191: 2012-12-30     18.840
#> 2192: 2012-12-31     17.250
#> 
#> Aggregated:     FALSE
#> Regular:        TRUE
#> Periodicity:    Time difference of 1 days
#> Missing values: explicit
#> Time zone:      UTC
#> Timestamps:     2192

## S3 method
print(setColNames(
  x = x,
  cols = "flow",
  values = "River Flow"
))
#> Values:
#>        .dateTime River Flow
#>           <POSc>      <num>
#>    1: 2007-01-01      9.540
#>    2: 2007-01-02      9.285
#>    3: 2007-01-03      8.940
#>    4: 2007-01-04      8.745
#>    5: 2007-01-05      8.490
#>   ---                      
#> 2188: 2012-12-27     26.685
#> 2189: 2012-12-28     28.050
#> 2190: 2012-12-29     23.580
#> 2191: 2012-12-30     18.840
#> 2192: 2012-12-31     17.250
#> 
#> Aggregated:     FALSE
#> Regular:        TRUE
#> Periodicity:    Time difference of 1 days
#> Missing values: explicit
#> Time zone:      UTC
#> Timestamps:     2192
```
