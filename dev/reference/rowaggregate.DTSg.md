# Aggregate values row-wise

Applies one or more provided summary functions row-wise to selected
columns of a
[`DTSg`](https://gisler.github.io/DTSg/dev/reference/DTSg.md) object.

## Usage

``` r
# S3 method for class 'DTSg'
rowaggregate(
  x,
  resultCols,
  fun,
  ...,
  cols = self$cols(class = "numeric"),
  clone = getOption("DTSgClone")
)
```

## Arguments

- x:

  A [`DTSg`](https://gisler.github.io/DTSg/dev/reference/DTSg.md) object
  (S3 method only).

- resultCols:

  A character vector either of length one (names of `fun` are appended
  in the case one or more functions are provided) or the same length as
  `fun` specifying the column names for the return values of `fun`.

- fun:

  A summary function, (named) [`list`](https://rdrr.io/r/base/list.html)
  of summary functions or (named) character vector specifying summary
  functions applied row-wise to all the values of the specified `cols`.
  The return value(s) must be of length one. See corresponding section
  for further information.

- ...:

  Further arguments passed on to `fun`.

- cols:

  A character vector specifying the columns to apply `fun` to. Another
  possibility is a character string containing either comma separated
  column names, for example, `"x,y,z"`, or the start and end column
  separated by a colon, for example, `"x:z"`.

- clone:

  A logical specifying if the object shall be modified in place or if a
  deep clone (copy) shall be made beforehand.

## Value

Returns a [`DTSg`](https://gisler.github.io/DTSg/dev/reference/DTSg.md)
object.

## R6 alias

The `raggregate()` alias for this method is exclusively available in the
R6 interface.

## Summary functions

Some examples for `fun` are as follows:

- [`mean`](https://rdrr.io/r/base/mean.html)

- [`list`](https://rdrr.io/r/base/list.html)`(min = `[`min`](https://rdrr.io/r/base/Extremes.html)`, max = `[`max`](https://rdrr.io/r/base/Extremes.html)`)`

- `c(sd = "sd", var = "var")`

## See also

[`cols`](https://gisler.github.io/DTSg/dev/reference/cols.DTSg.md),
[`getOption`](https://rdrr.io/r/base/options.html)

## Examples

``` r
# new DTSg object
DT <- data.table::data.table(
  date = flow$date,
  flow1 = flow$flow - abs(rnorm(nrow(flow))),
  flow2 = flow$flow,
  flow3 = flow$flow + abs(rnorm(nrow(flow)))
)
x <- DTSg$new(values = DT)

# mean and standard deviation of multiple measurements per timestamp
## R6 method
x$rowaggregate(
  resultCols = "flow",
  fun = list(mean = mean, sd = sd)
)$print()
#> Values:
#>        .dateTime     flow1  flow2     flow3 flow.mean   flow.sd
#>           <POSc>     <num>  <num>     <num>     <num>     <num>
#>    1: 2007-01-01  7.809470  9.540 10.073822  9.141097 1.1837084
#>    2: 2007-01-02  8.152575  9.285  9.492132  8.976569 0.7210759
#>    3: 2007-01-03  7.984601  8.940  8.975120  8.633240 0.5620122
#>    4: 2007-01-04  8.030992  8.745  9.865976  8.880656 0.9249831
#>    5: 2007-01-05  8.359606  8.490  8.821966  8.557191 0.2383906
#>   ---                                                          
#> 2188: 2012-12-27 24.629565 26.685 26.890944 26.068503 1.2504042
#> 2189: 2012-12-28 27.529591 28.050 28.682806 28.087466 0.5775194
#> 2190: 2012-12-29 23.327736 23.580 23.973729 23.627155 0.3255679
#> 2191: 2012-12-30 18.686844 18.840 19.694849 19.073897 0.5431846
#> 2192: 2012-12-31 16.757876 17.250 18.066902 17.358259 0.6611942
#> 
#> Aggregated:     FALSE
#> Regular:        TRUE
#> Periodicity:    Time difference of 1 days
#> Missing values: explicit
#> Time zone:      UTC
#> Timestamps:     2192

## 'raggregate()' is an R6 alias for 'rowaggregate()'
x$raggregate(
  resultCols = "flow",
  fun = list(mean = mean, sd = sd)
)$print()
#> Values:
#>        .dateTime     flow1  flow2     flow3 flow.mean   flow.sd
#>           <POSc>     <num>  <num>     <num>     <num>     <num>
#>    1: 2007-01-01  7.809470  9.540 10.073822  9.141097 1.1837084
#>    2: 2007-01-02  8.152575  9.285  9.492132  8.976569 0.7210759
#>    3: 2007-01-03  7.984601  8.940  8.975120  8.633240 0.5620122
#>    4: 2007-01-04  8.030992  8.745  9.865976  8.880656 0.9249831
#>    5: 2007-01-05  8.359606  8.490  8.821966  8.557191 0.2383906
#>   ---                                                          
#> 2188: 2012-12-27 24.629565 26.685 26.890944 26.068503 1.2504042
#> 2189: 2012-12-28 27.529591 28.050 28.682806 28.087466 0.5775194
#> 2190: 2012-12-29 23.327736 23.580 23.973729 23.627155 0.3255679
#> 2191: 2012-12-30 18.686844 18.840 19.694849 19.073897 0.5431846
#> 2192: 2012-12-31 16.757876 17.250 18.066902 17.358259 0.6611942
#> 
#> Aggregated:     FALSE
#> Regular:        TRUE
#> Periodicity:    Time difference of 1 days
#> Missing values: explicit
#> Time zone:      UTC
#> Timestamps:     2192

## S3 method
print(rowaggregate(
  x = x,
  resultCols = "flow",
  fun = list(mean = mean, sd = sd)
))
#> Values:
#>        .dateTime     flow1  flow2     flow3 flow.mean   flow.sd
#>           <POSc>     <num>  <num>     <num>     <num>     <num>
#>    1: 2007-01-01  7.809470  9.540 10.073822  9.141097 1.1837084
#>    2: 2007-01-02  8.152575  9.285  9.492132  8.976569 0.7210759
#>    3: 2007-01-03  7.984601  8.940  8.975120  8.633240 0.5620122
#>    4: 2007-01-04  8.030992  8.745  9.865976  8.880656 0.9249831
#>    5: 2007-01-05  8.359606  8.490  8.821966  8.557191 0.2383906
#>   ---                                                          
#> 2188: 2012-12-27 24.629565 26.685 26.890944 26.068503 1.2504042
#> 2189: 2012-12-28 27.529591 28.050 28.682806 28.087466 0.5775194
#> 2190: 2012-12-29 23.327736 23.580 23.973729 23.627155 0.3255679
#> 2191: 2012-12-30 18.686844 18.840 19.694849 19.073897 0.5431846
#> 2192: 2012-12-31 16.757876 17.250 18.066902 17.358259 0.6611942
#> 
#> Aggregated:     FALSE
#> Regular:        TRUE
#> Periodicity:    Time difference of 1 days
#> Missing values: explicit
#> Time zone:      UTC
#> Timestamps:     2192
```
