#' S3 wrapper method generator
#'
#' Generates S3 wrapper methods for public methods of `R6ClassGenerator`s, but
#' can also be used to generate \dQuote{plain} function wrappers.
#'
#' @param R6Method An [`expression`] with or a public method ([`function`]) of
#'   an `R6ClassGenerator`.
#' @param self A character string specifying the name of the parameter, which
#'   will take the R6 object.
#' @param dots A logical specifying if a `...` parameter shall be added as last
#'   parameter in case none already exists. This might be required for S3
#'   generic/method consistency.
#'
#' @return Returns an S3 method ([`function`]).
#'
#' @seealso [`S3Methods`], [`R6::R6Class`]
#'
#' @examples
#' # generate an S3 wrapper method for 'alter()' of 'DTSg'
#' alter.DTSg <- S3WrapperGenerator(
#'   R6Method = DTSg$public_methods$alter
#' )
#'
#' @export
S3WrapperGenerator <- function(R6Method, self = "x", dots = TRUE) {
  if (is.function(R6Method)) {
    R6Method <- as.expression(substitute(R6Method))
  }
  if (!is.expression(R6Method) ||
        R6Method[[1L]][[2L]][[3L]] != "public_methods" ||
        !is.R6Class(eval(R6Method[[1L]][[2L]][[2L]]))) {
    stop('"R6Method" must contain a public method of an "R6ClassGenerator".')
  }
  qassert(self, "S1")
  qassert(dots, "B1")

  args <- list()
  args[[self]] <- alist(self = )$self
  args <- c(args, formals(eval(R6Method)))

  if (dots && !any(names(args) == "...")) {
    args[["..."]] <- alist(... = )$...
  }

  S3Method <- function() {
    call <- match.call()
    call[[1L]] <- call("$", as.name(self), R6Method[[1L]][[3L]])
    call[[2L]] <- NULL

    eval(call)
  }

  formals(S3Method) <- args

  S3Method
}

#### aggregate ####
#' @importFrom stats aggregate
NULL
#' Aggregate values
#'
#' Applies a temporal aggregation level function to the _.dateTime_ column of a
#' [`DTSg`] object and aggregates its values column-wise to the function's
#' temporal aggregation level utilising one or more provided summary functions.
#' Additionally, it sets the object's [`aggregated`][DTSg] field to `TRUE`.
#'
#' @param x A [`DTSg`] object (S3 method only).
#' @param funby One of the temporal aggregation level functions described in
#'   [`TALFs`] or a user defined temporal aggregation level function. See
#'   corresponding section for further information.
#' @param fun A summary function, (named) [`list`] of summary functions or
#'   (named) character vector specifying summary functions applied column-wise
#'   to all the values of the same temporal aggregation level. The return
#'   value(s) must be of length one. See corresponding section for further
#'   information.
#' @param \dots Further arguments passed on to `fun`.
#' @param cols A character vector specifying the columns to aggregate. Another
#'   possibility is a character string containing either comma separated column
#'   names, for example, `"x,y,z"`, or the start and end column separated by a
#'   colon, for example, `"x:z"`.
#' @param n A logical specifying if a column named `.n` giving the number of
#'   values per temporal aggregation level shall be added. See corresponding
#'   section for further information.
#' @param ignoreDST A logical specifying if day saving time shall be ignored
#'   by `funby`. See corresponding section for further information.
#' @param multiplier A positive integerish value \dQuote{multiplying} the
#'   temporal aggregation level of certain [`TALFs`]. See corresponding section
#'   for further information.
#' @param funbyHelpers An optional [`list`] with helper data passed on to
#'   `funby`. See corresponding section for further information.
#' @param funbyApproach A character string specifying the flavour of the applied
#'   temporal aggregation level function. Either `"timechange"`, which utilises
#'   [`timechange::time_floor`], or `"base"`, which utilises [`as.POSIXct`], or
#'   `"fasttime"`, which utilises [`fasttime::fastPOSIXct`], or `"RcppCCTZ"`,
#'   which utilises [`RcppCCTZ::parseDatetime`] as the main function for
#'   transforming timestamps.
#' @param clone A logical specifying if the object shall be modified in place or
#'   if a deep clone (copy) shall be made beforehand.
#'
#' @section User defined TALFs, TALFs helper data and multiplier:
#' User defined temporal aggregation level functions have to return a
#' [`POSIXct`] vector of the same length as the time series and accept two
#' arguments: a [`POSIXct`] vector as its first and a [`list`] with helper data
#' as its second. The default elements of this [`list`] are as follows:
#' * _timezone:_ Same as the [`timezone`][DTSg] field.
#' * _ignoreDST:_ Same as the `ignoreDST` argument.
#' * _periodicity:_ Same as the [`periodicity`][DTSg] field.
#' * _na.status:_ Same as the [`na.status`][DTSg] field.
#' * _multiplier:_ Same as the `multiplier` argument.
#' * _funbyApproach:_ Same as the `funbyApproach` argument.
#'
#' Any additional element specified in the `funbyHelpers` argument is appended
#' to the end of the default [`list`]. In case `funbyHelpers` contains an
#' _ignoreDST, multiplier_ or _funbyApproach_ element, it takes precedence over
#' the respective method argument. _timezone, periodicity_ and _na.status_
#' elements are rejected, as they are always taken directly from the object.
#'
#' The temporal aggregation level of certain [`TALFs`] can be adjusted with the
#' help of the `multiplier` argument. A `multiplier` of `10`, for example, makes
#' \code{\link{byY_____}} aggregate to decades instead of years. Another example
#' is a `multiplier` of `6` provided to \code{\link{by_m____}}. The function
#' then aggregates all months of all first and all months of all second half
#' years instead of all months of all years separately. This feature is
#' supported by the following [`TALFs`] of the package:
#' * \code{\link{byY_____}}
#' * \code{\link{byYm____}}
#' * \code{\link{byYmdH__}} (UTC and equivalent as well as all Etc/GMT time zones only)
#' * \code{\link{byYmdHM_}}
#' * \code{\link{byYmdHMS}}
#' * \code{\link{by_m____}}
#' * \code{\link{by___H__}} (UTC and equivalent as well as all Etc/GMT time zones only)
#' * \code{\link{by____M_}}
#' * \code{\link{by_____S}}
#'
#' @section Summary functions:
#' Some examples for `fun` are as follows:
#' * [`mean`]
#' * \code{\link{list}(min = \link{min}, max = \link{max})}
#' * `c(sd = "sd", var = "var")`
#'
#' A [`list`] or character vector must have names in case more than one summary
#' function is provided. The method can benefit from \pkg{data.table}'s
#' _[GForce][data.table::datatable-optimize]_ optimisation in case a character
#' vector specifying summary functions is provided.
#'
#' @section Number of values per temporal aggregation level:
#' Depending on the number of columns to aggregate, the `.n` column contains
#' different counts:
#' * One column: The counts are calculated from the columns' values disregarding
#' any missing values.
#' * More than one column: The counts are calculated from the _.dateTime_ column
#' including all missing values.
#'
#' @section Ignore day saving time:
#' `ignoreDST` tells a temporal aggregation level function if it is supposed to
#' ignore day saving time while transforming the timestamps. This can be a
#' desired feature for time series strictly following the position of the sun
#' such as hydrological time series. Doing so ensures that diurnal variations
#' are preserved by all means and all intervals are of the \dQuote{correct}
#' length, however, a possible limitation might be that the day saving time
#' shift is invariably assumed to be one hour long. This feature requires that
#' the periodicity of the time series has been recognised and is supported by
#' the following [`TALFs`] of the package:
#' * \code{\link{byY_____}}
#' * \code{\link{byYQ____}}
#' * \code{\link{byYm____}}
#' * \code{\link{byYmd___}}
#' * \code{\link{by_Q____}}
#' * \code{\link{by_m____}}
#' * \code{\link{by___H__}}
#'
#' @return Returns an aggregated [`DTSg`] object.
#'
#' @seealso [`cols`], [`getOption`]
#'
#' @examples
#' # new DTSg object
#' x <- DTSg$new(values = flow)
#'
#' # mean yearly river flows
#' ## R6 method
#' x$aggregate(
#'   funby = byY_____,
#'   fun = "mean",
#'   na.rm = TRUE
#' )$print()
#'
#' ## S3 method
#' print(aggregate(
#'   x = x,
#'   funby = byY_____,
#'   fun = "mean",
#'   na.rm = TRUE
#' ))
#'
#' # variance and standard deviation of river flows per quarter
#' ## R6 method
#' x$aggregate(
#'   funby = byYQ____,
#'   fun = c(var = "var", sd = "sd"),
#'   na.rm = TRUE
#' )$print()
#'
#' ## S3 method
#' print(aggregate(
#'   x = x,
#'   funby = byYQ____,
#'   fun = c(var = "var", sd = "sd"),
#'   na.rm = TRUE
#' ))
#'
#' # mean of river flows of all first and all second half years
#' ## R6 method
#' x$aggregate(
#'   funby = by_m____,
#'   fun = "mean",
#'   na.rm = TRUE,
#'   multiplier = 6
#' )$print()
#'
#' ## S3 method
#' print(aggregate(
#'   x = x,
#'   funby = by_m____,
#'   fun = "mean",
#'   na.rm = TRUE,
#'   multiplier = 6
#' ))
#'
#' @aliases aggregate
#'
#' @export
aggregate.DTSg <- S3WrapperGenerator(expression(DTSg$public_methods$aggregate))

#### alter ####
#' @export
alter <- function(x, ...) {
  UseMethod("alter", x)
}
#' Alter time series
#'
#' Shortens, lengthens, filters for a consecutive range, changes the periodicity
#' and/or the status of missing values of a [`DTSg`] object.
#'
#' @param from A [`POSIXct`] timestamp in the same time zone as the time series
#'   or a character string coercible to one. Specifies the new start of the time
#'   series.
#' @param to A [`POSIXct`] timestamp in the same time zone as the time series or
#'   a character string coercible to one. Specifies the new end of the time
#'   series.
#' @param by Specifies the new periodicity in one of the ways the `by` argument
#'   of [`seq.POSIXt`] can be specified. Must be specified for time series with
#'   unrecognised periodicity. Time steps out of sync with the new periodicity
#'   are dropped.
#' @param rollback A logical specifying if a call to [`rollback`] shall be made
#'   when appropriate.
#' @param na.status A character string. Either `"explicit"`, which makes missing
#'   timestamps explicit according to the recognised periodicity, or
#'   `"implicit"`, which removes timestamps with missing values on all value
#'   columns. Please note that [`DTSg`] objects work best with explicitly
#'   missing values.
#' @param \dots Not used (S3 method only).
#' @inheritParams aggregate.DTSg
#'
#' @return Returns a [`DTSg`] object.
#'
#' @seealso [`getOption`], [`subset`], [`nas`]
#'
#' @examples
#' # new DTSg object
#' x <- DTSg$new(values = flow)
#'
#' # filter for the first two years
#' ## R6 method
#' x$alter(
#'   from = "2007-01-01",
#'   to = "2008-12-31"
#' )$print()
#'
#' ## S3 method
#' print(alter(
#'   x = x,
#'   from = "2007-01-01",
#'   to = "2008-12-31"
#' ))
#'
#' # change periodicity to one month
#' ## R6 method
#' x$alter(by = "1 month")$print()
#'
#' ## S3 method
#' print(alter(x = x, by = "1 month"))
#'
#' @aliases alter
#'
#' @export
alter.DTSg <- S3WrapperGenerator(expression(DTSg$public_methods$alter))

#### clone ####
#' @export
clone <- function(x, ...) {
  UseMethod("clone", x)
}
#' Clone object
#'
#' Clones (copies) a [`DTSg`] object. Merely assigning a variable representing a
#' [`DTSg`] object to a new variable does not result in a copy of the object.
#' Instead, both variables will reference and access the same data under the
#' hood, i.e. changing one will also affect the other. This is not an issue when
#' calling methods with the _DTSgClone_ option or `clone` argument set to
#' `TRUE`, but has to be kept in mind when setting fields, as they are always
#' modified in place. See [`DTSg`] for further information.
#'
#' @param deep A logical specifying if a deep copy shall be made (for
#'   consistency with the [`R6::R6Class`] the default is `FALSE`, but should
#'   generally be set to `TRUE`).
#' @inheritParams alter.DTSg
#'
#' @return Returns a cloned [`DTSg`] object.
#'
#' @seealso [`options`]
#'
#' @examples
#' # new DTSg object
#' x <- DTSg$new(values = flow)
#'
#' # make a deep copy
#' ## R6 method
#' x$clone(deep = TRUE)
#'
#' ## S3 method
#' clone(x = x, deep = TRUE)
#'
#' @aliases clone
#'
#' @export
clone.DTSg <- S3WrapperGenerator(expression(DTSg$public_methods$clone))

#### colapply ####
#' @export
colapply <- function(x, ...) {
  UseMethod("colapply", x)
}
#' Apply function column-wise
#'
#' Applies an arbitrary function to selected columns of a [`DTSg`] object.
#'
#' @param fun A [`function`]. Its return value must be of length one.
#' @param cols A character vector specifying the columns to apply `fun` to.
#'   Another possibility is a character string containing either comma separated
#'   column names, for example, `"x,y,z"`, or the start and end column separated
#'   by a colon, for example, `"x:z"`.
#' @param resultCols An optional character vector of the same length as `cols`
#'   specifying the column names for the return values of `fun`. Another
#'   possibility is a character string containing comma separated column names,
#'   for example, `"x,y,z"`. Non-existing columns are added and existing columns
#'   are overwritten. Columns are matched element-wise between `cols` and
#'   `resultCols`.
#' @param suffix An optional character string. The return values of `fun` are
#'   added as new columns with names consisting of the columns specified in
#'   `cols` and this suffix. Existing columns are never overwritten. Only used
#'   when `resultCols` is not specified.
#' @param helpers A logical specifying if helper data shall be handed over to
#'   `fun`. See corresponding section for further information.
#' @param funby One of the temporal aggregation level functions described in
#'   [`TALFs`] or a user defined temporal aggregation level function. Can be
#'   used to apply functions like [`cumsum`] to a certain temporal level. See
#'   corresponding section and examples for further information.
#' @inheritParams aggregate.DTSg
#'
#' @section Helper data:
#' In addition to the `...` argument, this method optionally hands over a
#' [`list`] argument with helper data called `.helpers` to `fun`. This [`list`]
#' contains the following elements:
#' * _.dateTime:_ A [`POSIXct`] vector containing the _.dateTime_ column.
#' * _periodicity:_ Same as the [`periodicity`][DTSg] field.
#' * _minLag:_ A [`difftime`] object containing the minimum time difference
#' between two subsequent timestamps.
#' * _maxLag:_ A [`difftime`] object containing the maximum time difference
#' between two subsequent timestamps.
#'
#' @inheritSection aggregate.DTSg User defined TALFs, TALFs helper data and multiplier
#'
#' @inheritSection aggregate.DTSg Ignore day saving time
#'
#' @inherit alter.DTSg return
#'
#' @seealso [`cols`], [`getOption`]
#'
#' @example examples/colapply.R
#'
#' @aliases colapply
#'
#' @export
colapply.DTSg <- S3WrapperGenerator(expression(DTSg$public_methods$colapply))

#### cols ####
#' @export
cols <- function(x, ...) {
  UseMethod("cols", x)
}
#' Get column names
#'
#' Returns all column names of a [`DTSg`] object, those of certain [`class`]es,
#' [`mode`]s, [`typeof`]s and/or those matching a certain pattern only.
#'
#' @param class An optional character vector matched to the most specific class
#'   (first element) of each column's [`class`] vector. The \dQuote{special
#'   class} `".numerary"` matches the [`integer`] and [`numeric`] classes.
#' @param pattern An optional character string passed on to the `pattern`
#'   argument of [`grep`].
#' @param mode An optional character vector matched to each column's [`mode`].
#' @param typeof An optional character vector matched to each column's
#'   [`typeof`].
#' @param \dots Further arguments passed on to [`grep`]. The `value` argument is
#'   rejected.
#' @inheritParams aggregate.DTSg
#'
#' @return Returns a character vector.
#'
#' @examples
#' # new DTSg object
#' x <- DTSg$new(values = flow)
#'
#' # get names of numeric columns
#' ## R6 method
#' x$cols(class = "numeric")
#'
#' ## 'names()' is a "hidden" R6 alias for 'cols()'
#' x$names(class = "numeric")
#'
#' ## S3 method
#' cols(x = x, class = "numeric")
#'
#' @aliases cols
#'
#' @export
cols.DTSg <- S3WrapperGenerator(expression(DTSg$public_methods$cols))

#### getCol ####
#' @export
getCol <- function(x, ...) {
  UseMethod("getCol", x)
}
#' Get column vector
#'
#' @description
#' Returns the values of a column of a [`DTSg`] object.
#'
#' The extract operator (`[`) acts as a shortcut for `getCol`.
#'
#' @param x A [`DTSg`] object (`getCol` S3 method only).
#' @param col A character string specifying a column name.
#' @param \dots Arguments passed on to `getCol` (only used by the extract
#'   operator).
#'
#' @return Returns a vector or a [`list`] in case of a [`list`] column.
#'
#' @seealso [`cols`]
#'
#' @examples
#' # new DTSg object
#' x <- DTSg$new(values = flow)
#'
#' # get the first ten values of the "flow" column
#' ## R6 methods
#' x$getCol(col = "flow")[1:10]
#' x$`[`("flow")[1:10]
#'
#' ## S3 methods
#' getCol(x = x, col = "flow")[1:10]
#' x["flow"][1:10]
#'
#' @aliases getCol
#'
#' @export
getCol.DTSg <- S3WrapperGenerator(expression(DTSg$public_methods$getCol))
#' @rdname getCol.DTSg
#' @export
`[.DTSg` <- S3WrapperGenerator(expression(DTSg$public_methods$`[`))

#### merge ####
#' Merge two objects
#'
#' Joins two [`DTSg`] objects based on their _.dateTime_ column. Their time
#' zones and `aggregated` fields must match.
#'
#' @param y A [`DTSg`] object or an object coercible to one. See [`new`] for
#'   further information.
#' @param \dots Further arguments passed on to [`data.table::merge`]. As the
#'   `by`, `by.x` and `by.y` arguments can endanger the object's integrity, they
#'   are rejected.
#' @inheritParams aggregate.DTSg
#'
#' @inherit alter.DTSg return
#'
#' @seealso [`getOption`]
#'
#' @examples
#' # new DTSg object
#' x <- DTSg$new(values = flow)
#'
#' # merge with 'data.table'
#' ## R6 method
#' x$merge(
#'   y = flow,
#'   suffixes = c("_1", "_2")
#' )$print()
#'
#' ## S3 method
#' print(merge(
#'   x = x,
#'   y = flow,
#'   suffixes = c("_1", "_2")
#' ))
#'
#' @aliases merge
#'
#' @export
merge.DTSg <- S3WrapperGenerator(expression(DTSg$public_methods$merge))

#### nas ####
#' @export
nas <- function(x, ...) {
  UseMethod("nas", x)
}
#' List missing values
#'
#' Lists the missing values of selected columns of a [`DTSg`] object with
#' recognised periodicity.
#'
#' @param cols A character vector specifying the columns whose missing values
#'   shall be listed. Another possibility is a character string containing
#'   either comma separated column names, for example, `"x,y,z"`, or the start
#'   and end column separated by a colon, for example, `"x:z"`.
#' @inheritParams alter.DTSg
#'
#' @return Returns a [`data.table::data.table`] with five columns:
#' * _.col:_ the column name
#' * _.group:_ the ID of the missing values group within each column
#' * _.from:_ the first timestamp of the missing values group
#' * _.to:_ the last timestamp of the missing values group
#' * _.n:_ the number of missing values per group
#'
#' @seealso [`cols`]
#'
#' @examples
#' # new DTSg object
#' x <- DTSg$new(values = flow)
#'
#' # list missing values
#' ## R6 method
#' x$nas()
#'
#' ## S3 method
#' nas(x = x)
#'
#' @aliases nas
#'
#' @export
nas.DTSg <- S3WrapperGenerator(expression(DTSg$public_methods$nas))

#### new ####
new <- function(
  Class,
  values,
  ID = "",
  parameter = "",
  unit = "",
  variant = "",
  aggregated = FALSE,
  fast = getOption("DTSgFast"),
  swallow = FALSE,
  na.status = getOption("DTSgNA.status"),
  funbyApproach = getOption("DTSgFunbyApproach")
) {
  # no R CMD check warning
}
setClass("DTSg", slots = c(. = "logical"))
setMethod(
  "initialize",
  "DTSg",
  function(.Object, ...) {
    DTSg$new(...)
  }
)

#### plot ####
#' Plot time series data
#'
#' Displays an interactive plot of a [`DTSg`] object. This method requires
#' \pkg{dygraphs} and \pkg{RColorBrewer} to be installed. Its main purpose is
#' not to make pretty plots, but rather to offer a possibility to interactively
#' explore time series data. The title of the plot and the label of its primary
#' axis are automatically generated from the object's metadata (fields). See
#' [`DTSg`] for further information.
#'
#' @param from A [`POSIXct`] timestamp in the same time zone as the time series
#'   or a character string coercible to one. The data is plotted from this
#'   timestamp on.
#' @param to A [`POSIXct`] timestamp in the same time zone as the time series or
#'   a character string coercible to one. The data is plotted up to this
#'   timestamp.
#' @param cols A character vector specifying the columns whose values shall be
#'   plotted. Another possibility is a character string containing either comma
#'   separated column names, for example, `"x,y,z"`, or the start and end column
#'   separated by a colon, for example, `"x:z"`.
#' @param secAxisCols An optional character vector specifying the columns whose
#'   values shall be plotted on a secondary axis. Another possibility is a
#'   character string containing either comma separated column names, for
#'   example, `"x,y,z"`, or the start and end column separated by a colon, for
#'   example, `"x:z"`. Must be a subset of `cols`.
#' @param secAxisLabel A character string specifying the label of the optional
#'   secondary axis.
#' @inheritParams alter.DTSg
#'
#' @inherit alter.DTSg return
#'
#' @seealso [`cols`]
#'
#' @examples
#' # new DTSg object
#' x <- DTSg$new(values = flow)
#'
#' # plot data
#' if (requireNamespace("dygraphs", quietly = TRUE) &&
#'     requireNamespace("RColorBrewer", quietly = TRUE)) {
#'   ## R6 method
#'   x$plot()
#'
#'   ## S3 method
#'   plot(x = x)
#' }
#'
#' @aliases plot
#'
#' @export
plot.DTSg <- S3WrapperGenerator(expression(DTSg$public_methods$plot))

#### print ####
#' Print object
#'
#' Prints a [`DTSg`] object.
#'
#' @inheritParams alter.DTSg
#'
#' @inherit alter.DTSg return
#'
#' @examples
#' # new DTSg object
#' x <- DTSg$new(values = flow)
#'
#' # print object
#' ## R6 method
#' x$print()
#'
#' ## S3 method
#' print(x = x)
#'
#' @aliases print
#'
#' @export
print.DTSg <- S3WrapperGenerator(expression(DTSg$public_methods$print))

#### refresh ####
#' @export
refresh <- function(x, ...) {
  UseMethod("refresh", x)
}
#' Object integrity
#'
#' Checks the integrity of a [`DTSg`] object and tries to automatically
#' (re-)detect its periodicity. Normally, there is no reason for a user to call
#' this method. The only exception is stated in [`values`].
#'
#' @inheritParams alter.DTSg
#'
#' @inherit alter.DTSg return
#'
#' @examples
#' # new DTSg object
#' x <- DTSg$new(values = flow)
#'
#' # check the object's integrity
#' ## R6 method
#' x$refresh()
#'
#' ## S3 method
#' refresh(x = x)
#'
#' @aliases refresh
#'
#' @export
refresh.DTSg <- S3WrapperGenerator(expression(DTSg$public_methods$refresh))

#### rollapply ####
#' @export
rollapply <- function(x, ...) {
  UseMethod("rollapply", x)
}
#' Rolling window function
#'
#' Applies an arbitrary function to a rolling window of selected columns of a
#' [`DTSg`] object with recognised periodicity.
#'
#' @param cols A character vector specifying the columns whose rolling window
#'   `fun` shall be applied to. Another possibility is a character string
#'   containing either comma separated column names, for example, `"x,y,z"`, or
#'   the start and end column separated by a colon, for example, `"x:z"`.
#' @param before An integerish value specifying the size of the window in time
#'   steps before the \dQuote{center} of the rolling window.
#' @param after An integerish value specifying the size of the window in time
#'   steps after the \dQuote{center} of the rolling window.
#' @param weights A character string specifying the method applied to calculate
#'   the weights handed over to `fun`. These are useful for functions like
#'   [`weighted.mean`]. See corresponding section for further information.
#' @param parameters A [`list`] specifying parameters for the weight calculation
#'   method. See corresponding section for further information.
#' @param memoryOverCPU A logical specifying if memory usage shall be preferred
#'   over CPU usage for this method call. The former is generally faster for
#'   smaller windows and shorter time series, the latter for bigger windows and
#'   longer time series or might even be the only one which works depending on
#'   the available hardware.
#' @inheritParams colapply.DTSg
#'
#' @section Weights:
#' Currently, only one method to calculate weights is supported:
#' `"inverseDistance"`. The distance \eqn{d} of the \dQuote{center} is one and
#' each time step further away from the \dQuote{center} adds one to it. So, for
#' example, the distance of a timestamp three steps away from the
#' \dQuote{center} is four. Additionally, the calculation of the weights accepts
#' a power parameter \eqn{p} as a named element of a [`list`] provided through
#' the `parameters` argument: \eqn{\frac{1}{d^p}}{1 / d^p}.
#'
#' @section Helper data:
#' In addition to the `...` argument, this method optionally hands over the
#' weights as a numeric vector (`w` argument) and a [`list`] argument with
#' helper data called `.helpers` to `fun`. This [`list`] contains the following
#' elements:
#' * _before:_ Same as the `before` argument.
#' * _after:_ Same as the `after` argument.
#' * _windowSize:_ Size of the rolling window (`before + 1L + after`).
#' * _centerIndex:_ Index of the \dQuote{center} of the rolling window
#' (`before + 1L`).
#'
#' @inherit alter.DTSg return
#'
#' @seealso [`cols`], [`getOption`]
#'
#' @examples
#' # new DTSg object
#' x <- DTSg$new(values = flow)
#'
#' # calculate a moving average
#' ## R6 method
#' x$rollapply(
#'   fun = mean,
#'   na.rm = TRUE,
#'   before = 2,
#'   after = 2
#' )$print()
#'
#' ## S3 method
#' print(rollapply(
#'   x = x,
#'   fun = mean,
#'   na.rm = TRUE,
#'   before = 2,
#'   after = 2
#' ))
#'
#' @aliases rollapply
#'
#' @export
rollapply.DTSg <- S3WrapperGenerator(expression(DTSg$public_methods$rollapply))

#### rowaggregate ####
#' @export
rowaggregate <- function(x, ...) {
  UseMethod("rowaggregate", x)
}
#' Aggregate values row-wise
#'
#' Applies one or more provided summary functions row-wise to selected columns
#' of a [`DTSg`] object.
#'
#' @param resultCols A character vector either of length one (names of `fun` are
#'   appended in the case one or more functions are provided) or the same length
#'   as `fun` specifying the column names for the return values of `fun`.
#' @param fun A summary function, (named) [`list`] of summary functions or
#'   (named) character vector specifying summary functions applied row-wise to
#'   all the values of the specified `cols`. The return value(s) must be of
#'   length one. See corresponding section for further information.
#' @param cols A character vector specifying the columns to apply `fun` to.
#'   Another possibility is a character string containing either comma separated
#'   column names, for example, `"x,y,z"`, or the start and end column separated
#'   by a colon, for example, `"x:z"`.
#' @inheritParams aggregate.DTSg
#'
#' @section Summary functions:
#' Some examples for `fun` are as follows:
#' * [`mean`]
#' * \code{\link{list}(min = \link{min}, max = \link{max})}
#' * `c(sd = "sd", var = "var")`
#'
#' @inherit alter.DTSg return
#'
#' @seealso [`cols`], [`getOption`]
#'
#' @examples
#' # new DTSg object
#' DT <- data.table::data.table(
#'   date = flow$date,
#'   flow1 = flow$flow - abs(rnorm(nrow(flow))),
#'   flow2 = flow$flow,
#'   flow3 = flow$flow + abs(rnorm(nrow(flow)))
#' )
#' x <- DTSg$new(values = DT)
#'
#' # mean and standard deviation of multiple measurements per timestamp
#' ## R6 method
#' x$rowaggregate(
#'   resultCols = "flow",
#'   fun = list(mean = mean, sd = sd)
#' )$print()
#'
#' ## 'raggregate()' is a "hidden" R6 alias for 'rowaggregate()'
#' x$raggregate(
#'   resultCols = "flow",
#'   fun = list(mean = mean, sd = sd)
#' )$print()
#'
#' ## S3 method
#' print(rowaggregate(
#'   x = x,
#'   resultCols = "flow",
#'   fun = list(mean = mean, sd = sd)
#' ))
#'
#' @aliases rowaggregate raggregate
#'
#' @export
rowaggregate.DTSg <- S3WrapperGenerator(expression(DTSg$public_methods$rowaggregate))

#### rowbind ####
#' @export
rowbind <- function(x, ...) {
  UseMethod("rowbind", x)
}
#' Combine rows
#'
#' Combines the rows of [`DTSg`] and other suitable objects.
#'
#' @param \dots Any number of [`DTSg`] objects or objects coercible to one (see
#'   [`new`] for further information). [`list`]s of such objects or a mixture of
#'   lists and non-lists are also accepted.
#' @inheritParams alter.DTSg
#'
#' @inherit alter.DTSg return
#'
#' @seealso [`cols`], [`getOption`]
#'
#' @examples
#' # new DTSg object
#' x <- DTSg$new(values = flow[1:500, ])
#'
#' # combine rows
#' ## R6 method
#' x$rowbind(
#'   list(flow[1001:1500, ], DTSg$new(values = flow[501:1000, ])),
#'   flow[1501:.N, ]
#' )$print()
#'
#' ## 'rbind()' is a "hidden" R6 alias for 'rowbind()'
#' x$rbind(
#'   list(flow[1001:1500, ], DTSg$new(values = flow[501:1000, ])),
#'   flow[1501:.N, ]
#' )$print()
#'
#' ## S3 method
#' print(rowbind(
#'   x = x,
#'   list(flow[1001:1500, ], DTSg$new(values = flow[501:1000, ])),
#'   flow[1501:.N, ]
#' ))
#'
#' @aliases rowbind rbind
#'
#' @export
rowbind.DTSg <- S3WrapperGenerator(expression(DTSg$public_methods$rowbind))

#### setColNames ####
#' @export
setColNames <- function(x, ...) {
  UseMethod("setColNames", x)
}
#' Set column names
#'
#' Changes the column names of [`DTSg`] objects.
#'
#' @param cols A character vector specifying the columns whose names shall be
#'   set. Another possibility is a character string containing either comma
#'   separated column names, for example, `"x,y,z"`, or the start and end column
#'   separated by a colon, for example, `"x:z"`. The name of the _.dateTime_
#'   column cannot be changed.
#' @param values A character vector of the same length as `cols` specifying the
#'   desired column names. Another possibility is a character string containing
#'   comma separated column names, for example, `"x,y,z"`.
#' @inheritParams alter.DTSg
#'
#' @inherit alter.DTSg return
#'
#' @seealso [`cols`], [`getOption`]
#'
#' @examples
#' # new DTSg object
#' x <- DTSg$new(values = flow)
#'
#' # rename column "flow" to "River Flow"
#' ## R6 method
#' x$setColNames(
#'   cols = "flow",
#'   values = "River Flow"
#' )$print()
#'
#' ## 'setnames()' is a "hidden" R6 alias for 'setColNames()'
#' x$setnames(
#'   cols = "flow",
#'   values = "River Flow"
#' )$print()
#'
#' ## S3 method
#' print(setColNames(
#'   x = x,
#'   cols = "flow",
#'   values = "River Flow"
#' ))
#'
#' @aliases setColNames setnames
#'
#' @export
setColNames.DTSg <- S3WrapperGenerator(expression(DTSg$public_methods$setColNames))

#### setCols ####
#' @export
setCols <- function(x, ...) {
  UseMethod("setCols", x)
}
#' Set column values
#'
#' Changes the values of columns, adds columns to and/or removes columns from a
#' [`DTSg`] object. The values can optionally be set for certain rows only.
#'
#' @param i An integerish vector indexing rows (positive numbers pick and
#'   negative numbers omit rows) or a filter expression accepted by the `i`
#'   argument of [`data.table::data.table`]. Filter expressions can contain the
#'   special symbol [`.N`][data.table::special-symbols].
#' @param cols A character vector specifying the columns whose values shall be
#'   set. Another possibility is a character string containing comma separated
#'   column names, for example, `"x,y,z"`. The values of the _.dateTime_ column
#'   cannot be changed.
#' @param values A vector, [`list`] or list-like object (e.g.
#'   [`data.table::data.table`]) of replacement and/or new values accepted by
#'   the `value` argument of \pkg{data.table}'s [`data.table::set`] function.
#'   `NULL` as a value removes a column.
#' @inheritParams alter.DTSg
#'
#' @inherit alter.DTSg return
#'
#' @seealso [`cols`], [`getOption`]
#'
#' @examples
#' # new DTSg object
#' x <- DTSg$new(values = flow)
#'
#' # cap river flows to 100
#' ## R6 method
#' x$setCols(
#'   i = flow > 100,
#'   cols = "flow",
#'   values = 100
#' )$print()
#'
#' ## 'set()' is a "hidden" R6 alias for 'setCols()'
#' x$set(
#'   i = flow > 100,
#'   cols = "flow",
#'   values = 100
#' )$print()
#'
#' ## S3 method
#' print(setCols(
#'   x = x,
#'   i = flow > 100,
#'   cols = "flow",
#'   values = 100
#' ))
#'
#' # set measurement unit with the help of 'units'
#' if (requireNamespace("units", quietly = TRUE)) {
#'   ## R6 method
#'   x$setCols(
#'     cols = "flow",
#'     values = units::set_units(x["flow"], "m^3/s")
#'   )$print()
#'
#'   ## S3 method
#'   print(setCols(
#'     x = x,
#'     cols = "flow",
#'     values = units::set_units(x["flow"], "m^3/s")
#'   ))
#' }
#'
#' @aliases setCols set
#'
#' @export
setCols.DTSg <- S3WrapperGenerator(expression(DTSg$public_methods$setCols))

#### subset ####
#' Subset time series data
#'
#' Filters rows and/or selects columns of a [`DTSg`] object.
#'
#' @param cols A character vector specifying the columns to select. Another
#'   possibility is a character string containing either comma separated column
#'   names, for example, `"x,y,z"`, or the start and end column separated by a
#'   colon, for example, `"x:z"`. The _.dateTime_ column is always selected and
#'   cannot be part of it.
#' @param funby One of the temporal aggregation level functions described in
#'   [`TALFs`] or a user defined temporal aggregation level function. Can be
#'   used to, for instance, select the last two observations of a certain
#'   temporal level. See corresponding section and examples for further
#'   information.
#' @param na.status A character string. Either `"explicit"`, which makes missing
#'   timestamps explicit according to the recognised periodicity, or
#'   `"implicit"`, which removes timestamps with missing values on all value
#'   columns. See corresponding section for further information.
#' @inheritParams aggregate.DTSg
#' @inheritParams setCols.DTSg
#'
#' @inheritSection aggregate.DTSg User defined TALFs, TALFs helper data and multiplier
#'
#' @inheritSection aggregate.DTSg Ignore day saving time
#'
#' @section Status of missing values:
#' Please note that filtering rows and having or making missing timestamps
#' explicit equals to setting the values of all other timestamps to missing. The
#' default value of `na.status` is therefore `"implicit"`. To simply filter for
#' a consecutive range of a [`DTSg`] object while leaving the `na.status`
#' untouched, [`alter`] is probably the better choice.
#'
#' @inherit alter.DTSg return
#'
#' @seealso [`cols`], [`getOption`]
#'
#' @examples
#' # new DTSg object
#' x <- DTSg$new(values = flow)
#'
#' # filter for the first six observations
#' ## R6 method
#' x$subset(i = 1:6)$print()
#'
#' ## S3 method
#' print(subset(x = x, i = 1:6))
#'
#' # filter for the last two observations per year
#' ## R6 method
#' x$subset(
#'   i = (.N - 1):.N,
#'   funby = function(x, ...) {data.table::year(x)}
#' )$print()
#'
#' ## S3 method
#' print(subset(
#'   x = x,
#'   i = (.N - 1):.N,
#'   funby = function(x, ...) {data.table::year(x)}
#' ))
#'
#' @aliases subset
#'
#' @export
subset.DTSg <- S3WrapperGenerator(expression(DTSg$public_methods$subset))

#### summary ####
#' Summarise time series data
#'
#' Calculates summary statistics of selected columns of a [`DTSg`] object.
#'
#' @param object A [`DTSg`] object (S3 method only).
#' @param cols A character vector specifying the columns whose values shall be
#'   summarised. Another possibility is a character string containing either
#'   comma separated column names, for example, `"x,y,z"`, or the start and end
#'   column separated by a colon, for example, `"x:z"`.
#' @param \dots Further arguments passed on to [`summary.data.frame`].
#'
#' @return Returns a [`table`].
#'
#' @seealso [`cols`]
#'
#' @examples
#' # new DTSg object
#' x <- DTSg$new(values = flow)
#'
#' # calculate summary statistics
#' ## R6 method
#' x$summary()
#'
#' ## S3 method
#' summary(object = x)
#'
#' @aliases summary
#'
#' @export
summary.DTSg <- S3WrapperGenerator(expression(DTSg$public_methods$summary), "object")

#### values ####
#' @export
values <- function(x, ...) {
  UseMethod("values", x)
}
#' Get values
#'
#' Returns the values of a [`DTSg`] object.
#'
#' @param reference A logical specifying if a copy of the values or a reference
#'   to the values shall be returned. See corresponding section for further
#'   information.
#' @param drop A logical specifying if the object and all references to it shall
#'   be removed from the global (and only the global) environment after
#'   successfully returning its values. This feature allows for a resource
#'   efficient destruction of a [`DTSg`] object while preserving its values.
#' @param class A character string specifying the class of the returned values.
#'   `"data.frame"` only works when either a copy of the values is returned or
#'   the object is dropped.
#' @inheritParams alter.DTSg
#'
#' @section Reference to the values:
#' A reference to the values of a [`DTSg`] object can be used to modify them in
#' place. This includes the _.dateTime_ column, which serves as the object's
#' time index. Modifying this column can therefore endanger the object's
#' integrity. In case needs to do so ever arise, [`refresh`] should be called
#' immediately afterwards in order to check the object's integrity.
#'
#' @return Returns a [`data.table::data.table`], a reference to a
#'   [`data.table::data.table`] or a [`data.frame`].
#'
#' @note
#' The original name of the _.dateTime_ column is restored when not returned as
#' a reference or when dropped.
#'
#' @examples
#' # new DTSg object
#' x <- DTSg$new(values = flow)
#'
#' # get values
#' ## R6 method
#' x$values()
#'
#' ## S3 method
#' values(x = x)
#'
#' @aliases values
#'
#' @export
values.DTSg <- S3WrapperGenerator(expression(DTSg$public_methods$values))
