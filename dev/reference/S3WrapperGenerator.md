# S3 wrapper method generator

Generates S3 wrapper methods for public methods of `R6ClassGenerator`s,
but can also be used to generate “plain” function wrappers.

## Usage

``` r
S3WrapperGenerator(R6Method, self = "x", dots = TRUE)
```

## Arguments

- R6Method:

  An [`expression`](https://rdrr.io/r/base/expression.html) with or a
  public method ([`function`](https://rdrr.io/r/base/function.html)) of
  an `R6ClassGenerator`.

- self:

  A character string specifying the name of the parameter, which will
  take the R6 object.

- dots:

  A logical specifying if a `...` parameter shall be added as last
  parameter in case none already exists. This might be required for S3
  generic/method consistency.

## Value

Returns an S3 method
([`function`](https://rdrr.io/r/base/function.html)).

## See also

[`S3Methods`](https://rdrr.io/r/base/UseMethod.html),
[`R6::R6Class`](https://r6.r-lib.org/reference/R6Class.html)

## Examples

``` r
# generate an S3 wrapper method for 'alter()' of 'DTSg'
alter.DTSg <- S3WrapperGenerator(
  R6Method = DTSg$public_methods$alter
)
```
