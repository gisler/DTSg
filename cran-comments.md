# Major release

* Added `funbyApproach` argument to `new()`, `aggregate()`, `colapply()` and `subset()` methods: allows for specifying the flavour of the applied temporal aggregation level functions (either `"base"` utilising `as.POSIXct()` or `"fasttime"` utilising `fasttime::fastPOSIXct()` or `"RcppCCTZ"` utilising `RcppCCTZ::parseDatetime()` as the main function for transforming timestamps). Custom approaches for user defined TALFs are also possible. Please note that the `byFasttime*` versions of the TALFs are now deprecated. Use this argument from now on instead
* Added `funbyApproach` also to the `list` of helper data (`funbyHelpers` argument) passed on to TALFs
* Added `funbyApproach` field reflecting the individual `funbyApproach` of a `DTSg` object (can also be actively set in order to change the utilised approach)
* Added undocumented `names()` method exclusive only to the R6 interface acting as an alias for the `cols()` method
* Added `mode` and `typeof` arguments to `cols()` method: allows for getting column names with a certain `mode()` and/or `typeof()`. This can be especially handy when making use of the `units` package
* Added `DTSgFast`, `DTSgFunbyApproach` and `DTSgNA.status` options providing default values for the `fast`, `funbyApproach` and `na.status` arguments of the `new()` method
* Added `DTSgDeprecatedWarnings` option: allows for disabling warnings from deprecated features.
* Greatly sped up `by______()` and `byFasttime______()` TALFs
* It is no longer possible to use the deprecated value `"all"` with the `class` argument of the `cols()` method in order to get all column names. Use the default value `NULL` instead for this. `"all"` is treated as a filter for classes of type `all` from now on
* Added an example to the `setCols()` method showing how to set measurement units with the help of the `units` package
* Added an example to the documentation of the `colapply()` method showing how to calculate running correlations with the help of the `runner` package
* Bumped minimum tested R version from 3.5.2 to 4.0.2 using the corresponding MRAN repository snapshot
* Slightly improved documentation
* Minor internal code improvements

# Test environments

* Windows on GitHub Actions (4.0.2)
* Windows on GitHub Actions (devel)
* macOS on GitHub Actions (oldrel)
* macOS on GitHub Actions (release)
* Linux on GitHub Actions (oldrel)
* Linux on GitHub Actions (release)

# R CMD check results

There were no ERRORs, WARNINGs or NOTEs.

# Downstream dependencies

There are currently no downstream dependencies for this package.
