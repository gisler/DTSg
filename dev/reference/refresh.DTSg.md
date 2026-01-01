# Object integrity

Checks the integrity of a
[`DTSg`](https://gisler.github.io/DTSg/dev/reference/DTSg.md) object and
tries to automatically (re-)detect its periodicity. Normally, there is
no reason for a user to call this method. The only exception is stated
in
[`values`](https://gisler.github.io/DTSg/dev/reference/values.DTSg.md).

## Usage

``` r
# S3 method for class 'DTSg'
refresh(x, ...)
```

## Arguments

- x:

  A [`DTSg`](https://gisler.github.io/DTSg/dev/reference/DTSg.md) object
  (S3 method only).

- ...:

  Not used (S3 method only).

## Value

Returns a [`DTSg`](https://gisler.github.io/DTSg/dev/reference/DTSg.md)
object.

## Examples

``` r
# new DTSg object
x <- DTSg$new(values = flow)

# check the object's integrity
## R6 method
x$refresh()

## S3 method
refresh(x = x)
```
