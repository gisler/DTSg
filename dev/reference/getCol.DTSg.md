# Get column vector

Returns the values of a column of a
[`DTSg`](https://gisler.github.io/DTSg/dev/reference/DTSg.md) object.

The extract operator (`[`) acts as a shortcut for `getCol`.

## Usage

``` r
# S3 method for class 'DTSg'
getCol(x, col = self$cols(class = "numeric")[1L], ...)

# S3 method for class 'DTSg'
x[...]
```

## Arguments

- x:

  A [`DTSg`](https://gisler.github.io/DTSg/dev/reference/DTSg.md) object
  (`getCol` S3 method only).

- col:

  A character string specifying a column name.

- ...:

  Arguments passed on to `getCol` (only used by the extract operator).

## Value

Returns a vector or a [`list`](https://rdrr.io/r/base/list.html) in case
of a [`list`](https://rdrr.io/r/base/list.html) column.

## See also

[`cols`](https://gisler.github.io/DTSg/dev/reference/cols.DTSg.md)

## Examples

``` r
# new DTSg object
x <- DTSg$new(values = flow)

# get the first ten values of the "flow" column
## R6 methods
x$getCol(col = "flow")[1:10]
#>  [1]  9.540  9.285  8.940  8.745  8.490  8.400  8.280  8.700  9.825 10.185
x$`[`("flow")[1:10]
#>  [1]  9.540  9.285  8.940  8.745  8.490  8.400  8.280  8.700  9.825 10.185

## S3 methods
getCol(x = x, col = "flow")[1:10]
#>  [1]  9.540  9.285  8.940  8.745  8.490  8.400  8.280  8.700  9.825 10.185
x["flow"][1:10]
#>  [1]  9.540  9.285  8.940  8.745  8.490  8.400  8.280  8.700  9.825 10.185
```
