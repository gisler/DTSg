# Get column names

Returns all column names of a
[`DTSg`](https://gisler.github.io/DTSg/dev/reference/DTSg.md) object,
those of certain [`class`](https://rdrr.io/r/base/class.html)es,
[`mode`](https://rdrr.io/r/base/mode.html)s,
[`typeof`](https://rdrr.io/r/base/typeof.html)s and/or those matching a
certain pattern only.

## Usage

``` r
# S3 method for class 'DTSg'
cols(x, class = NULL, pattern = NULL, mode = NULL, typeof = NULL, ...)
```

## Arguments

- x:

  A [`DTSg`](https://gisler.github.io/DTSg/dev/reference/DTSg.md) object
  (S3 method only).

- class:

  An optional character vector matched to the most specific class (first
  element) of each column's [`class`](https://rdrr.io/r/base/class.html)
  vector. The “special class” `".numerary"` matches the
  [`integer`](https://rdrr.io/r/base/integer.html) and
  [`numeric`](https://rdrr.io/r/base/numeric.html) classes.

- pattern:

  An optional character string passed on to the `pattern` argument of
  [`grep`](https://rdrr.io/r/base/grep.html).

- mode:

  An optional character vector matched to each column's
  [`mode`](https://rdrr.io/r/base/mode.html).

- typeof:

  An optional character vector matched to each column's
  [`typeof`](https://rdrr.io/r/base/typeof.html).

- ...:

  Further arguments passed on to
  [`grep`](https://rdrr.io/r/base/grep.html). The `value` argument is
  rejected.

## Value

Returns a character vector.

## R6 alias

The `names()` alias for this method is exclusively available in the R6
interface.

## Examples

``` r
# new DTSg object
x <- DTSg$new(values = flow)

# get names of numeric columns
## R6 method
x$cols(class = "numeric")
#> [1] "flow"

## 'names()' is an R6 alias for 'cols()'
x$names(class = "numeric")
#> [1] "flow"

## S3 method
cols(x = x, class = "numeric")
#> [1] "flow"
```
