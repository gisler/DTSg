# List missing values

Lists the missing values of selected columns of a
[`DTSg`](https://gisler.github.io/DTSg/dev/reference/DTSg.md) object
with recognised periodicity.

## Usage

``` r
# S3 method for class 'DTSg'
nas(x, cols = self$cols(), ...)
```

## Arguments

- x:

  A [`DTSg`](https://gisler.github.io/DTSg/dev/reference/DTSg.md) object
  (S3 method only).

- cols:

  A character vector specifying the columns whose missing values shall
  be listed. Another possibility is a character string containing either
  comma separated column names, for example, `"x,y,z"`, or the start and
  end column separated by a colon, for example, `"x:z"`.

- ...:

  Not used (S3 method only).

## Value

Returns a
[`data.table::data.table`](https://rdatatable.gitlab.io/data.table/reference/data.table.html)
with five columns:

- *.col:* the column name

- *.group:* the ID of the missing values group within each column

- *.from:* the first timestamp of the missing values group

- *.to:* the last timestamp of the missing values group

- *.n:* the number of missing values per group

## See also

[`cols`](https://gisler.github.io/DTSg/dev/reference/cols.DTSg.md)

## Examples

``` r
# new DTSg object
x <- DTSg$new(values = flow)

# list missing values
## R6 method
x$nas()
#>      .col .group      .from        .to    .n
#>    <char>  <int>     <POSc>     <POSc> <int>
#> 1:   flow      1 2007-10-12 2007-10-24    13
#> 2:   flow      2 2007-10-26 2007-11-03     9
#> 3:   flow      3 2007-11-10 2007-11-10     1

## S3 method
nas(x = x)
#>      .col .group      .from        .to    .n
#>    <char>  <int>     <POSc>     <POSc> <int>
#> 1:   flow      1 2007-10-12 2007-10-24    13
#> 2:   flow      2 2007-10-26 2007-11-03     9
#> 3:   flow      3 2007-11-10 2007-11-10     1
```
