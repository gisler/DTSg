# Summarise time series data

Calculates summary statistics of selected columns of a
[`DTSg`](https://gisler.github.io/DTSg/dev/reference/DTSg.md) object.

## Usage

``` r
# S3 method for class 'DTSg'
summary(object, cols = self$cols(), ...)
```

## Arguments

- object:

  A [`DTSg`](https://gisler.github.io/DTSg/dev/reference/DTSg.md) object
  (S3 method only).

- cols:

  A character vector specifying the columns whose values shall be
  summarised. Another possibility is a character string containing
  either comma separated column names, for example, `"x,y,z"`, or the
  start and end column separated by a colon, for example, `"x:z"`.

- ...:

  Further arguments passed on to
  [`summary.data.frame`](https://rdrr.io/r/base/summary.html).

## Value

Returns a [`table`](https://rdrr.io/r/base/table.html).

## See also

[`cols`](https://gisler.github.io/DTSg/dev/reference/cols.DTSg.md)

## Examples

``` r
# new DTSg object
x <- DTSg$new(values = flow)

# calculate summary statistics
## R6 method
x$summary()
#>       flow        
#>  Min.   :  4.995  
#>  1st Qu.:  8.085  
#>  Median : 11.325  
#>  Mean   : 16.197  
#>  3rd Qu.: 18.375  
#>  Max.   :290.715  
#>  NA's   :23       

## S3 method
summary(object = x)
#>       flow        
#>  Min.   :  4.995  
#>  1st Qu.:  8.085  
#>  Median : 11.325  
#>  Mean   : 16.197  
#>  3rd Qu.: 18.375  
#>  Max.   :290.715  
#>  NA's   :23       
```
