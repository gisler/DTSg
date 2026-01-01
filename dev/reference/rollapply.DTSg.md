# Rolling window function

Applies an arbitrary function to a rolling window of selected columns of
a [`DTSg`](https://gisler.github.io/DTSg/dev/reference/DTSg.md) object
with recognised periodicity.

## Usage

``` r
# S3 method for class 'DTSg'
rollapply(
  x,
  fun,
  ...,
  cols = self$cols(class = "numeric")[1L],
  before = 1L,
  after = before,
  weights = "inverseDistance",
  parameters = list(power = 1),
  resultCols = NULL,
  suffix = NULL,
  helpers = TRUE,
  memoryOverCPU = TRUE,
  clone = getOption("DTSgClone")
)
```

## Arguments

- x:

  A [`DTSg`](https://gisler.github.io/DTSg/dev/reference/DTSg.md) object
  (S3 method only).

- fun:

  A [`function`](https://rdrr.io/r/base/function.html). Its return value
  must be of length one.

- ...:

  Further arguments passed on to `fun`.

- cols:

  A character vector specifying the columns whose rolling window `fun`
  shall be applied to. Another possibility is a character string
  containing either comma separated column names, for example,
  `"x,y,z"`, or the start and end column separated by a colon, for
  example, `"x:z"`.

- before:

  An integerish value specifying the size of the window in time steps
  before the “center” of the rolling window.

- after:

  An integerish value specifying the size of the window in time steps
  after the “center” of the rolling window.

- weights:

  A character string specifying the method applied to calculate the
  weights handed over to `fun`. These are useful for functions like
  [`weighted.mean`](https://rdrr.io/r/stats/weighted.mean.html). See
  corresponding section for further information.

- parameters:

  A [`list`](https://rdrr.io/r/base/list.html) specifying parameters for
  the weight calculation method. See corresponding section for further
  information.

- resultCols:

  An optional character vector of the same length as `cols` specifying
  the column names for the return values of `fun`. Another possibility
  is a character string containing comma separated column names, for
  example, `"x,y,z"`. Non-existing columns are added and existing
  columns are overwritten. Columns are matched element-wise between
  `cols` and `resultCols`.

- suffix:

  An optional character string. The return values of `fun` are added as
  new columns with names consisting of the columns specified in `cols`
  and this suffix. Existing columns are never overwritten. Only used
  when `resultCols` is not specified.

- helpers:

  A logical specifying if helper data shall be handed over to `fun`. See
  corresponding section for further information.

- memoryOverCPU:

  A logical specifying if memory usage shall be preferred over CPU usage
  for this method call. The former is generally faster for smaller
  windows and shorter time series, the latter for bigger windows and
  longer time series or might even be the only one which works depending
  on the available hardware.

- clone:

  A logical specifying if the object shall be modified in place or if a
  deep clone (copy) shall be made beforehand.

## Value

Returns a [`DTSg`](https://gisler.github.io/DTSg/dev/reference/DTSg.md)
object.

## Weights

Currently, only one method to calculate weights is supported:
`"inverseDistance"`. The distance \\d\\ of the “center” is one and each
time step further away from the “center” adds one to it. So, for
example, the distance of a timestamp three steps away from the “center”
is four. Additionally, the calculation of the weights accepts a power
parameter \\p\\ as a named element of a
[`list`](https://rdrr.io/r/base/list.html) provided through the
`parameters` argument: \\\frac{1}{d^p}\\.

## Helper data

In addition to the `...` argument, this method optionally hands over the
weights as a numeric vector (`w` argument) and a
[`list`](https://rdrr.io/r/base/list.html) argument with helper data
called `.helpers` to `fun`. This
[`list`](https://rdrr.io/r/base/list.html) contains the following
elements:

- *before:* Same as the `before` argument.

- *after:* Same as the `after` argument.

- *windowSize:* Size of the rolling window (`before + 1L + after`).

- *centerIndex:* Index of the “center” of the rolling window
  (`before + 1L`).

## See also

[`cols`](https://gisler.github.io/DTSg/dev/reference/cols.DTSg.md),
[`getOption`](https://rdrr.io/r/base/options.html)

## Examples

``` r
# new DTSg object
x <- DTSg$new(values = flow)

# calculate a moving average
## R6 method
x$rollapply(
  fun = mean,
  na.rm = TRUE,
  before = 2,
  after = 2
)$print()
#> Values:
#>        .dateTime    flow
#>           <POSc>   <num>
#>    1: 2007-01-01  9.2550
#>    2: 2007-01-02  9.1275
#>    3: 2007-01-03  9.0000
#>    4: 2007-01-04  8.7720
#>    5: 2007-01-05  8.5710
#>   ---                   
#> 2188: 2012-12-27 28.9860
#> 2189: 2012-12-28 25.3200
#> 2190: 2012-12-29 22.8810
#> 2191: 2012-12-30 21.9300
#> 2192: 2012-12-31 19.8900
#> 
#> Aggregated:     FALSE
#> Regular:        TRUE
#> Periodicity:    Time difference of 1 days
#> Missing values: explicit
#> Time zone:      UTC
#> Timestamps:     2192

## S3 method
print(rollapply(
  x = x,
  fun = mean,
  na.rm = TRUE,
  before = 2,
  after = 2
))
#> Values:
#>        .dateTime    flow
#>           <POSc>   <num>
#>    1: 2007-01-01  9.2550
#>    2: 2007-01-02  9.1275
#>    3: 2007-01-03  9.0000
#>    4: 2007-01-04  8.7720
#>    5: 2007-01-05  8.5710
#>   ---                   
#> 2188: 2012-12-27 28.9860
#> 2189: 2012-12-28 25.3200
#> 2190: 2012-12-29 22.8810
#> 2191: 2012-12-30 21.9300
#> 2192: 2012-12-31 19.8900
#> 
#> Aggregated:     FALSE
#> Regular:        TRUE
#> Periodicity:    Time difference of 1 days
#> Missing values: explicit
#> Time zone:      UTC
#> Timestamps:     2192
```
