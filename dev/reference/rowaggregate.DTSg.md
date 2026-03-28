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
#>    1: 2007-01-01  9.072267  9.540 11.404560 10.005609 1.2338928
#>    2: 2007-01-02  8.368617  9.285  9.592668  9.082095 0.6367520
#>    3: 2007-01-03  8.233940  8.940 10.813477  9.329139 1.3330697
#>    4: 2007-01-04  7.958064  8.745 10.126586  8.943217 1.0977656
#>    5: 2007-01-05  8.090116  8.490  9.704281  8.761466 0.8406262
#>   ---                                                          
#> 2188: 2012-12-27 26.501281 26.685 26.814830 26.667037 0.1575445
#> 2189: 2012-12-28 27.485451 28.050 28.724568 28.086673 0.6203722
#> 2190: 2012-12-29 22.934337 23.580 25.179549 23.897962 1.1558845
#> 2191: 2012-12-30 16.855802 18.840 21.005584 18.900462 2.0755512
#> 2192: 2012-12-31 16.317189 17.250 17.772145 17.113111 0.7370737
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
#>    1: 2007-01-01  9.072267  9.540 11.404560 10.005609 1.2338928
#>    2: 2007-01-02  8.368617  9.285  9.592668  9.082095 0.6367520
#>    3: 2007-01-03  8.233940  8.940 10.813477  9.329139 1.3330697
#>    4: 2007-01-04  7.958064  8.745 10.126586  8.943217 1.0977656
#>    5: 2007-01-05  8.090116  8.490  9.704281  8.761466 0.8406262
#>   ---                                                          
#> 2188: 2012-12-27 26.501281 26.685 26.814830 26.667037 0.1575445
#> 2189: 2012-12-28 27.485451 28.050 28.724568 28.086673 0.6203722
#> 2190: 2012-12-29 22.934337 23.580 25.179549 23.897962 1.1558845
#> 2191: 2012-12-30 16.855802 18.840 21.005584 18.900462 2.0755512
#> 2192: 2012-12-31 16.317189 17.250 17.772145 17.113111 0.7370737
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
#>    1: 2007-01-01  9.072267  9.540 11.404560 10.005609 1.2338928
#>    2: 2007-01-02  8.368617  9.285  9.592668  9.082095 0.6367520
#>    3: 2007-01-03  8.233940  8.940 10.813477  9.329139 1.3330697
#>    4: 2007-01-04  7.958064  8.745 10.126586  8.943217 1.0977656
#>    5: 2007-01-05  8.090116  8.490  9.704281  8.761466 0.8406262
#>   ---                                                          
#> 2188: 2012-12-27 26.501281 26.685 26.814830 26.667037 0.1575445
#> 2189: 2012-12-28 27.485451 28.050 28.724568 28.086673 0.6203722
#> 2190: 2012-12-29 22.934337 23.580 25.179549 23.897962 1.1558845
#> 2191: 2012-12-30 16.855802 18.840 21.005584 18.900462 2.0755512
#> 2192: 2012-12-31 16.317189 17.250 17.772145 17.113111 0.7370737
#> 
#> Aggregated:     FALSE
#> Regular:        TRUE
#> Periodicity:    Time difference of 1 days
#> Missing values: explicit
#> Time zone:      UTC
#> Timestamps:     2192
```
