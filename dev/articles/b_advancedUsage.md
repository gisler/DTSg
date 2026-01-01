# B. Advanced usage

This vignette illustrates some more advanced concepts of the `DTSg`
package, namely reference semantics, chaining and piping as well as
swallowing and dropping.

------------------------------------------------------------------------

First, let’s load the package as well as some data and let’s create a
`DTSg` object:

``` r
library(DTSg)

data(flow)
TS <- DTSg$new(flow)
TS
#> Values:
#>        .dateTime   flow
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
#> 
#> Aggregated:     FALSE
#> Regular:        TRUE
#> Periodicity:    Time difference of 1 days
#> Missing values: explicit
#> Time zone:      UTC
#> Timestamps:     2192
```

## Reference semantics

By default, every method manipulating the values of a `DTSg` object
creates a deep clone (copy) of it beforehand. This behaviour can be
overridden by setting the `clone` argument of the respective method to
`FALSE`. Globally, deep cloning can be controlled with the help of the
*DTSgClone* option:

``` r
TS$alter("2007-01-01", "2008-12-31")
# `TS` was deep cloned before shortening it, hence its end date is still in the
# year 2012
TS
#> Values:
#>        .dateTime   flow
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
#> 
#> Aggregated:     FALSE
#> Regular:        TRUE
#> Periodicity:    Time difference of 1 days
#> Missing values: explicit
#> Time zone:      UTC
#> Timestamps:     2192

options(DTSgClone = FALSE)
getOption("DTSgClone")
#> [1] FALSE
# `TS` was modified in place this time, hence its end date is in the year 2008
# now
TS$alter("2007-01-01", "2008-12-31")
TS
#> Values:
#>       .dateTime   flow
#>          <POSc>  <num>
#>   1: 2007-01-01  9.540
#>   2: 2007-01-02  9.285
#>   3: 2007-01-03  8.940
#>   4: 2007-01-04  8.745
#>   5: 2007-01-05  8.490
#>  ---                  
#> 727: 2008-12-27 18.180
#> 728: 2008-12-28 16.575
#> 729: 2008-12-29 13.695
#> 730: 2008-12-30 12.540
#> 731: 2008-12-31 11.940
#> 
#> Aggregated:     FALSE
#> Regular:        TRUE
#> Periodicity:    Time difference of 1 days
#> Missing values: explicit
#> Time zone:      UTC
#> Timestamps:     731
```

As we can see, with cloning set to `FALSE`, the object was altered in
place, i.e. no assignment to a new or reassignment to an existing
variable was necessary in order to make the changes stick. This is due
to the R6 nature of `DTSg` objects.

### Note

Using reference semantics can result in undesired behaviour. Merely
assigning a variable representing a `DTSg` object to a new variable does
not result in a copy of the object. Instead, both variables will
reference and access the same data under the hood, i.e. changing one
will also affect the other. In case you want a “real” copy of a `DTSg`
object, you will have to use the
[`clone()`](https://gisler.github.io/DTSg/dev/reference/clone.DTSg.md)
method with its `deep` argument set to `TRUE` (for consistency with the
`R6` package its default is `FALSE`):

``` r
TSc <- TS$clone(deep = TRUE)
# or 'clone(TS, deep = TRUE)'
```

## Chaining and piping

Especially in combination with reference semantics, chaining and piping
can be a fast and comfortable way to apply several object manipulations
in a row. While chaining only works in combination with the R6
interface, piping is an exclusive feature of the S3 interface.

Let’s start with chaining:

``` r
TS <- DTSg$
  new(flow)$
  alter("2007-01-01", "2008-12-31")$
  colapply(interpolateLinear)$
  aggregate(byYm____, mean)
TS
#> Values:
#>      .dateTime      flow
#>         <POSc>     <num>
#>  1: 2007-01-01 25.281290
#>  2: 2007-02-01 14.496964
#>  3: 2007-03-01 12.889839
#>  4: 2007-04-01 12.470500
#>  5: 2007-05-01  9.233226
#> ---                     
#> 20: 2008-08-01 12.641129
#> 21: 2008-09-01 13.710500
#> 22: 2008-10-01 10.626774
#> 23: 2008-11-01  8.902000
#> 24: 2008-12-01 16.435645
#> 
#> Aggregated:     TRUE
#> Regular:        FALSE
#> Periodicity:    1 months
#> Min lag:        Time difference of 28 days
#> Max lag:        Time difference of 31 days
#> Missing values: explicit
#> Time zone:      UTC
#> Timestamps:     24
```

For piping, we have to make sure that R 4.1.0 or later is installed in
order to have access to R’s native pipe operator `|>` (alternatively,
the same can be achieved with the forward-pipe operator `%>%` of the
`magrittr` package):

``` r
TS <- new("DTSg", flow) |>
  alter("2007-01-01", "2008-12-31") |>
  colapply(interpolateLinear) |>
  aggregate(byYm____, mean)
TS
#> Values:
#>      .dateTime      flow
#>         <POSc>     <num>
#>  1: 2007-01-01 25.281290
#>  2: 2007-02-01 14.496964
#>  3: 2007-03-01 12.889839
#>  4: 2007-04-01 12.470500
#>  5: 2007-05-01  9.233226
#> ---                     
#> 20: 2008-08-01 12.641129
#> 21: 2008-09-01 13.710500
#> 22: 2008-10-01 10.626774
#> 23: 2008-11-01  8.902000
#> 24: 2008-12-01 16.435645
#> 
#> Aggregated:     TRUE
#> Regular:        FALSE
#> Periodicity:    1 months
#> Min lag:        Time difference of 28 days
#> Max lag:        Time difference of 31 days
#> Missing values: explicit
#> Time zone:      UTC
#> Timestamps:     24
```

## Swallowing and dropping

An extension to reference semantics of existing `DTSg` objects are
reference semantics during object creation. This behaviour can be
triggered with the help of the `swallow` argument of the
[`new()`](https://gisler.github.io/DTSg/dev/reference/DTSg.md) method.
When set to `TRUE`, a `data.table` provided through the `values`
argument is “swallowed” by the `DTSg` object, i.e. no copy of it is made
and all references to it are removed from the global (and only the
global) environment upon successful object creation:

``` r
library(data.table)

DT <- copy(flow)
ls(pattern = "^DT$")
#> [1] "DT"
TS <- DTSg$new(DT, swallow = TRUE)
ls(pattern = "^DT$")
#> character(0)
```

The opposite of swallowing is called dropping. This term refers to
querying the values of a `DTSg` object as a reference while removing all
references to the original `DTSg` object from the global (and again only
the global) environment at the same time:

``` r
TS <- DTSg$new(flow)
ls(pattern = "^TS$")
#> [1] "TS"
DT <- TS$values(drop = TRUE)
ls(pattern = "^TS$")
#> character(0)
```

## Column access

Sometimes need may arise to access a column other than the one currently
processed from a function within the
[`colapply()`](https://gisler.github.io/DTSg/dev/reference/colapply.DTSg.md)
method. This can be accomplished in the following way:

``` r
# add a new column recording if a certain value is missing or not before
# carrying out a linear interpolation
TS <- DTSg$new(flow)
TS$summary()
#>       flow        
#>  Min.   :  4.995  
#>  1st Qu.:  8.085  
#>  Median : 11.325  
#>  Mean   : 16.197  
#>  3rd Qu.: 18.375  
#>  Max.   :290.715  
#>  NA's   :23
TS$
  colapply(
    function(x, ...) is.na(x),
    resultCols = "missing"
  )$
  colapply(interpolateLinear)$
  summary()
#>       flow          missing       
#>  Min.   :  4.995   Mode :logical  
#>  1st Qu.:  8.126   FALSE:2169     
#>  Median : 11.408   TRUE :23       
#>  Mean   : 16.212                  
#>  3rd Qu.: 18.439                  
#>  Max.   :290.715

# undo the linear interpolation (requires additional access to the previously
# created column named "missing", which can be carried out with the help of the
# `getCol` method or its shortcut, the `[` operator, and the freely chosen `y`
# argument)
TS$
  colapply(
    function(x, y, ...) ifelse(y, NA, x),
    y = TS$getCol("missing") # or 'y = TS["missing"]'
  )$
  summary()
#>       flow          missing       
#>  Min.   :  4.995   Mode :logical  
#>  1st Qu.:  8.085   FALSE:2169     
#>  Median : 11.325   TRUE :23       
#>  Mean   : 16.197                  
#>  3rd Qu.: 18.375                  
#>  Max.   :290.715                  
#>  NA's   :23
```

Please refer to the help pages for further details.
