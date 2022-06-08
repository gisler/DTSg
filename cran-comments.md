# Minor release

* Column names can often now be additionally specified by a character string containing either comma separated column names, for example, `"x,y,z"`, or the start and end column separated by a colon, for example, `"x:z"`
* Fixed a bug in `interpolateLinear()` causing partial last observation carried forward behaviour when its `roll` argument was specified smaller than the size of the gap to be interpolated
* Fixed a bug in `interpolateLinear()` causing partial interpolation in certain cases when its `roll` argument was specified smaller than the size of the gap to be interpolated
* Slightly improved documentation

# Test environments

* Windows on GitHub Actions (4.0.2)
* Local Windows (release)
* Windows on GitHub Actions (devel)
* macOS on GitHub Actions (oldrel)
* macOS on GitHub Actions (release)
* Linux on GitHub Actions (oldrel)
* Linux on GitHub Actions (release)

# R CMD check results

There were no ERRORs, WARNINGs or NOTEs.

# Downstream dependencies

There are currently no downstream dependencies for this package.
