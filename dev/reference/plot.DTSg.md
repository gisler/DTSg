# Plot time series data

Displays an interactive plot of a
[`DTSg`](https://gisler.github.io/DTSg/dev/reference/DTSg.md) object.
This method requires dygraphs and RColorBrewer to be installed. Its main
purpose is not to make pretty plots, but rather to offer a possibility
to interactively explore time series data. The title of the plot and the
label of its primary axis are automatically generated from the object's
metadata (fields). See
[`DTSg`](https://gisler.github.io/DTSg/dev/reference/DTSg.md) for
further information.

## Usage

``` r
# S3 method for class 'DTSg'
plot(
  x,
  from = first(self$values(reference = TRUE)[[".dateTime"]]),
  to = last(self$values(reference = TRUE)[[".dateTime"]]),
  cols = self$cols(class = "numeric"),
  secAxisCols = NULL,
  secAxisLabel = "",
  ...
)
```

## Arguments

- x:

  A [`DTSg`](https://gisler.github.io/DTSg/dev/reference/DTSg.md) object
  (S3 method only).

- from:

  A [`POSIXct`](https://rdrr.io/r/base/DateTimeClasses.html) timestamp
  in the same time zone as the time series or a character string
  coercible to one. The data is plotted from this timestamp on.

- to:

  A [`POSIXct`](https://rdrr.io/r/base/DateTimeClasses.html) timestamp
  in the same time zone as the time series or a character string
  coercible to one. The data is plotted up to this timestamp.

- cols:

  A character vector specifying the columns whose values shall be
  plotted. Another possibility is a character string containing either
  comma separated column names, for example, `"x,y,z"`, or the start and
  end column separated by a colon, for example, `"x:z"`.

- secAxisCols:

  An optional character vector specifying the columns whose values shall
  be plotted on a secondary axis. Another possibility is a character
  string containing either comma separated column names, for example,
  `"x,y,z"`, or the start and end column separated by a colon, for
  example, `"x:z"`. Must be a subset of `cols`.

- secAxisLabel:

  A character string specifying the label of the optional secondary
  axis.

- ...:

  Not used (S3 method only).

## Value

Returns a [`DTSg`](https://gisler.github.io/DTSg/dev/reference/DTSg.md)
object.

## See also

[`cols`](https://gisler.github.io/DTSg/dev/reference/cols.DTSg.md)

## Examples

``` r
# new DTSg object
x <- DTSg$new(values = flow)

# plot data
if (requireNamespace("dygraphs", quietly = TRUE) &&
    requireNamespace("RColorBrewer", quietly = TRUE)) {
  ## R6 method
  x$plot()

  ## S3 method
  plot(x = x)
}
```
