## Minor Release

* Added a "special class" called `".numerary"` to the `class` argument of the `cols` method: allows for querying the names of `integer` and `numeric` columns in one go
* Added undocumented methods `raggregate`, `rbind`, `set` and `setnames` acting as aliases for `rowaggregate`, `rowbind`, `setCols` and `setColNames`, which are exclusively available in the R6 interface
* `print` method now truncates the number of printed rows of the values more aggressively
* Created a `pkgdown` website
* Improved vignettes and documentation

## Test Environments

* Windows on GitHub Actions (3.5.2)
* Local Windows (4.0.2)
* Windows on GitHub Actions (devel)
* win-builder (devel)
* macOS on GitHub Actions (oldrel)
* macOS on GitHub Actions (release)
* Linux on GitHub Actions (oldrel)
* Linux on GitHub Actions (release)

## R CMD Check Results

There were no ERRORs, WARNINGs or NOTEs.

## Downstream Dependencies

There are currently no downstream dependencies for this package.
