# Daily river flows

A dataset containing a fictional time series of daily river flows with
implicitly missing values.

## Usage

``` r
flow
```

## Format

A
[`data.table::data.table`](https://rdatatable.gitlab.io/data.table/reference/data.table.html)
with 2169 rows and two columns:

- date:

  A [`POSIXct`](https://rdrr.io/r/base/DateTimeClasses.html) vector
  ranging from the start of the year 2007 to the end of the year 2012.

- flow:

  A numeric vector with daily river flows in cubic metres per second.
