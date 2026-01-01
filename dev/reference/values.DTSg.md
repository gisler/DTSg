# Get values

Returns the values of a
[`DTSg`](https://gisler.github.io/DTSg/dev/reference/DTSg.md) object.

## Usage

``` r
# S3 method for class 'DTSg'
values(
  x,
  reference = FALSE,
  drop = FALSE,
  class = c("data.table", "data.frame"),
  ...
)
```

## Arguments

- x:

  A [`DTSg`](https://gisler.github.io/DTSg/dev/reference/DTSg.md) object
  (S3 method only).

- reference:

  A logical specifying if a copy of the values or a reference to the
  values shall be returned. See corresponding section for further
  information.

- drop:

  A logical specifying if the object and all references to it shall be
  removed from the global (and only the global) environment after
  successfully returning its values. This feature allows for a resource
  efficient destruction of a
  [`DTSg`](https://gisler.github.io/DTSg/dev/reference/DTSg.md) object
  while preserving its values.

- class:

  A character string specifying the class of the returned values.
  `"data.frame"` only works when either a copy of the values is returned
  or the object is dropped.

- ...:

  Not used (S3 method only).

## Value

Returns a
[`data.table::data.table`](https://rdatatable.gitlab.io/data.table/reference/data.table.html),
a reference to a
[`data.table::data.table`](https://rdatatable.gitlab.io/data.table/reference/data.table.html)
or a [`data.frame`](https://rdrr.io/r/base/data.frame.html).

## Note

The original name of the *.dateTime* column is restored when not
returned as a reference or when dropped.

## Reference to the values

A reference to the values of a
[`DTSg`](https://gisler.github.io/DTSg/dev/reference/DTSg.md) object can
be used to modify them in place. This includes the *.dateTime* column,
which serves as the object's time index. Modifying this column can
therefore endanger the object's integrity. In case needs to do so ever
arise,
[`refresh`](https://gisler.github.io/DTSg/dev/reference/refresh.DTSg.md)
should be called immediately afterwards in order to check the object's
integrity.

## Examples

``` r
# new DTSg object
x <- DTSg$new(values = flow)

# get values
## R6 method
x$values()
#> Key: <date>
#>             date   flow
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

## S3 method
values(x = x)
#> Key: <date>
#>             date   flow
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
```
