# Set column values

Changes the values of columns, adds columns to and/or removes columns
from a [`DTSg`](https://gisler.github.io/DTSg/dev/reference/DTSg.md)
object. The values can optionally be set for certain rows only.

## Usage

``` r
# S3 method for class 'DTSg'
setCols(
  x,
  i,
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

- i:

  An integerish vector indexing rows (positive numbers pick and negative
  numbers omit rows) or a filter expression accepted by the `i` argument
  of
  [`data.table::data.table`](https://rdatatable.gitlab.io/data.table/reference/data.table.html).
  Filter expressions can contain the special symbol
  [`.N`](https://rdatatable.gitlab.io/data.table/reference/special-symbols.html).

- cols:

  A character vector specifying the columns whose values shall be set.
  Another possibility is a character string containing comma separated
  column names, for example, `"x,y,z"`. The values of the *.dateTime*
  column cannot be changed.

- values:

  A vector, [`list`](https://rdrr.io/r/base/list.html) or list-like
  object (e.g.
  [`data.table::data.table`](https://rdatatable.gitlab.io/data.table/reference/data.table.html))
  of replacement and/or new values accepted by the `value` argument of
  data.table's
  [`data.table::set`](https://rdatatable.gitlab.io/data.table/reference/assign.html)
  function. `NULL` as a value removes a column.

- clone:

  A logical specifying if the object shall be modified in place or if a
  deep clone (copy) shall be made beforehand.

- ...:

  Not used (S3 method only).

## Value

Returns a [`DTSg`](https://gisler.github.io/DTSg/dev/reference/DTSg.md)
object.

## R6 alias

The `set()` alias for this method is exclusively available in the R6
interface.

## See also

[`cols`](https://gisler.github.io/DTSg/dev/reference/cols.DTSg.md),
[`getOption`](https://rdrr.io/r/base/options.html)

## Examples

``` r
# new DTSg object
x <- DTSg$new(values = flow)

# cap river flows to 100
## R6 method
x$setCols(
  i = flow > 100,
  cols = "flow",
  values = 100
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

## 'set()' is an R6 alias for 'setCols()'
x$set(
  i = flow > 100,
  cols = "flow",
  values = 100
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
print(setCols(
  x = x,
  i = flow > 100,
  cols = "flow",
  values = 100
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

# set measurement unit with the help of 'units'
if (requireNamespace("units", quietly = TRUE)) {
  ## R6 method
  x$setCols(
    cols = "flow",
    values = units::set_units(x["flow"], "m^3/s")
  )$print()

  ## S3 method
  print(setCols(
    x = x,
    cols = "flow",
    values = units::set_units(x["flow"], "m^3/s")
  ))
}
#> Values:
#>        .dateTime           flow
#>           <POSc>        <units>
#>    1: 2007-01-01  9.540 [m^3/s]
#>    2: 2007-01-02  9.285 [m^3/s]
#>    3: 2007-01-03  8.940 [m^3/s]
#>    4: 2007-01-04  8.745 [m^3/s]
#>    5: 2007-01-05  8.490 [m^3/s]
#>   ---                          
#> 2188: 2012-12-27 26.685 [m^3/s]
#> 2189: 2012-12-28 28.050 [m^3/s]
#> 2190: 2012-12-29 23.580 [m^3/s]
#> 2191: 2012-12-30 18.840 [m^3/s]
#> 2192: 2012-12-31 17.250 [m^3/s]
#> 
#> Aggregated:     FALSE
#> Regular:        TRUE
#> Periodicity:    Time difference of 1 days
#> Missing values: explicit
#> Time zone:      UTC
#> Timestamps:     2192
#> Values:
#>        .dateTime           flow
#>           <POSc>        <units>
#>    1: 2007-01-01  9.540 [m^3/s]
#>    2: 2007-01-02  9.285 [m^3/s]
#>    3: 2007-01-03  8.940 [m^3/s]
#>    4: 2007-01-04  8.745 [m^3/s]
#>    5: 2007-01-05  8.490 [m^3/s]
#>   ---                          
#> 2188: 2012-12-27 26.685 [m^3/s]
#> 2189: 2012-12-28 28.050 [m^3/s]
#> 2190: 2012-12-29 23.580 [m^3/s]
#> 2191: 2012-12-30 18.840 [m^3/s]
#> 2192: 2012-12-31 17.250 [m^3/s]
#> 
#> Aggregated:     FALSE
#> Regular:        TRUE
#> Periodicity:    Time difference of 1 days
#> Missing values: explicit
#> Time zone:      UTC
#> Timestamps:     2192
```
