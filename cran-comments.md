## Minor Release

* Added `subset` method: allows for filtering rows and/or selecting columns of a `DTSg` object
* Added `setCols` method: allows for setting the values of columns of, adding columns to and/or removing columns from a `DTSg` object
* Added `[` extract operator: acts as a shortcut for the `getCol` method
* Added examples to the documentation of the `colapply` method showing how to calculate moving averages with the help of the `runner` package instead of the `rollapply` method
* `aggregate` method can benefit from `data.table`'s *GForce* optimisation now when its `fun` argument is provided with a character vector specifying summary functions
* Greatly sped up `nas` method
* Temporal aggregation level functions supplied to the `funby` argument of the `colapply` method are not forced to return a `POSIXct` timestamp any longer. They are, however, forced to return an atomic mode (the same goes for the `subset` method).
* `getCol` method now is capable of also querying the *.dateTime* column
* `R6Method` argument of `S3WrapperGenerator` now also takes a public method of an `R6ClassGenerator` as a function and not only as an expression
* Fixed that not all missing values were made explicit after a call to the `merge` method despite an `"explicit"` `na.status` in some cases
* Fixed that `getCol` method tried to query all numeric columns instead of only the first one by default
* Improved vignettes and documentation
* Minor internal code improvements

## Test Environments

* local Linux (4.0.2)
* Linux on Travis CI (3.6.3)
* Linux on Travis CI (4.0.0)
* OS X on Travis CI (3.6.3)
* OS X on Travis CI (4.0.2)
* win-builder (devel)

## R CMD Check Results

There were no ERRORs, WARNINGs or NOTEs.

## Downstream Dependencies

There are currently no downstream dependencies for this package.
