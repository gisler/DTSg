#### aggregate ####
#' @importFrom stats aggregate
NULL
#' Aggregate Values
#'
#' Applies a temporal aggregation level function to the \emph{.dateTime} column
#'  of a \code{\link{DTSg}} object and aggregates its \emph{values} column-wise
#'  to the function's temporal aggregation level utilising one or more provided
#'  summary functions. Additionally, it sets the object's \emph{aggregated}
#'  field to \code{TRUE}. See \code{\link{DTSg}} for further information.
#'
#' @param x A \code{\link{DTSg}} object (S3 method only).
#' @param funby One of the temporal aggregation level functions described in
#'  \code{\link{TALFs}} or a user defined temporal aggregation level function.
#'  See details for further information.
#' @param fun A summary \code{\link{function}} or a named \code{\link{list}} of
#'  summary functions applied column-wise to all the values of the same temporal
#'  aggregation level, for instance, \code{\link{mean}} or
#'  \code{\link{list}(min = \link{min}, max = \link{max})}. The return value(s)
#'  must be of length one.
#' @param \dots Further arguments passed on to \code{fun}.
#' @param cols A character vector specifying the columns to aggregate.
#' @param n A logical specifying if a column named \emph{.n} giving the number
#'  of values per temporal aggregation level is added. See details for further
#'  information.
#' @param ignoreDST A logical specifying if day saving time is ignored during
#'  aggregation. See details for further information.
#' @param clone A logical specifying if the object is modified in place or if a
#'  clone (copy) is made beforehand.
#'
#' @details
#' User defined temporal aggregation level functions have to return a
#'  \code{\link{POSIXct}} vector of the same length as the time series and
#'  accept two arguments: a \code{\link{POSIXct}} vector as its first and a
#'  \code{\link{list}} with helper data as its second. This \code{\link{list}}
#'  in turn contains the following named elements:
#'  \itemize{
#'    \item \emph{timezone:} Same as \emph{timezone} field. See
#'      \code{\link{DTSg}} for further information.
#'    \item \emph{ignoreDST:} Same as \code{ignoreDST} argument.
#'    \item \emph{periodicity:} Same as \emph{periodicity} field. See
#'      \code{\link{DTSg}} for further information.
#'    \item \emph{na.status:} Same as \emph{na.status} field. See
#'      \code{\link{DTSg}} for further information.
#'  }
#'
#' Depending on the number of columns to aggregate, the \emph{.n} column
#'  contains different counts:
#'  \itemize{
#'    \item One column: The counts are calculated from the value column without
#'      any missing values. This means that missing values are always stripped
#'      regardless of the value of a possible \code{na.rm} argument.
#'    \item More than one column: The counts are calculated from the
#'      \emph{.dateTime} column including all missing values.
#'  }
#'
#' \code{ignoreDST} tells a temporal aggregation level function if it is
#'  supposed to ignore day saving time while forming new timestamps. This can be
#'  a desired feature for time series strictly following the position of the sun
#'  (such as hydrological time series). Doing so ensures that diurnal variations
#'  are preserved and all intervals are of \dQuote{correct} length, however, a
#'  possible limitation might be that the day saving time shift is invariably
#'  assumed to be exactly one hour long. This feature requires that the
#'  periodicity of the time series is recognised and is supported by the
#'  following temporal aggregation level functions of the package:
#'  \itemize{
#'    \item \code{\link{byY_____}}
#'    \item \code{\link{byYQ____}}
#'    \item \code{\link{byYm____}}
#'    \item \code{\link{byYmd___}}
#'    \item \code{\link{by_Q____}}
#'    \item \code{\link{by_m____}}
#'    \item \code{\link{by___H__}}
#'  }
#'
#' @return Returns an aggregated \code{\link{DTSg}} object.
#'
#' @seealso \code{\link{DTSg}}, \code{\link{TALFs}}, \code{\link{function}},
#'  \code{\link{list}}, \code{\link{cols}}, \code{\link{POSIXct}}
#'
#' @examples
#' # new DTSg object
#' x <- DTSg$new(values = flow)
#'
#' # mean yearly river flows
#' ## R6 method
#' x$aggregate(funby = byY_____, fun = mean, na.rm = TRUE)
#'
#' ## S3 method
#' aggregate(x = x, funby = byY_____, fun = mean, na.rm = TRUE)
#'
#' # minimum and maximum river flow per quarter
#' ## R6 method
#' x$aggregate(funby = byYQ____, fun = list(min = min, max = max), na.rm = TRUE)
#'
#' ## S3 method
#' aggregate(x = x, funby = byYQ____, fun = list(min = min, max = max), na.rm = TRUE)
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
#' Alter Time Series
#'
#' Shortens, lengthens, subsets a complete range, changes the periodicity and/or
#'  the status of missing values of a \code{\link{DTSg}} object.
#'
#' @param x A \code{\link{DTSg}} object (S3 method only).
#' @param from A \code{\link{POSIXct}} date with the same time zone as the time
#'  series or a character string coercible to one. Specifies the new start of
#'  the time series.
#' @param to A \code{\link{POSIXct}} date with the same time zone as the time
#'  series or a character string coercible to one. Specifies the new end of the
#'  time series.
#' @param by Specifies the new periodicity in one of the ways the \code{by}
#'  argument of \code{\link{seq.POSIXt}} can be specified. Must be specified for
#'  time series with unrecognised periodicity. Time steps out of sync with the
#'  new periodicity are dropped.
#' @param rollback A logical specifying if a call to \code{\link{rollback}} is
#'  made when appropriate.
#' @param clone A logical specifying if the object is modified in place or if a
#'  clone (copy) is made beforehand.
#' @param na.status A character string. Either \code{"explicit"}, which makes
#'  missing timestamps according to the recognised periodicity explicit, or
#'  \code{"implicit"}, which removes timestamps with missing values on all value
#'  columns. Please note that \code{\link{DTSg}} objects work best with explicit
#'  missing values.
#' @param \dots Not used (S3 method only).
#'
#' @return Returns a \code{\link{DTSg}} object.
#'
#' @seealso \code{\link{DTSg}}, \code{\link{values}}, \code{\link{POSIXct}},
#'  \code{\link{seq.POSIXt}}, \code{\link{rollback}}
#'
#' @examples
#' # new DTSg object
#' x <- DTSg$new(values = flow)
#'
#' # subset the first two years
#' ## R6 method
#' x$alter(from = "2007-01-01", to = "2008-12-31")
#'
#' ## S3 method
#' alter(x = x, from = "2007-01-01", to = "2008-12-31")
#'
#' # change periodicity to one month
#' ## R6 method
#' x$alter(by = "1 month")
#'
#' ## S3 method
#' alter(x = x, by = "1 month")
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
#' Clone Object
#'
#' Clones (copies) a \code{\link{DTSg}} object. Merely assigning a variable
#'  representing a \code{\link{DTSg}} object to a new variable does not result
#'  in a copy of the object. Instead, both variables will reference and access
#'  the same data in the background, i.e. changing one will also affect the
#'  other. This is not an issue when calling methods with the \emph{DTSgClone}
#'  option or \code{clone} argument set to \code{TRUE}, but has to be kept in
#'  mind when setting fields, as they are always modified in place. See
#'  \code{\link{DTSg}} for further information.
#'
#' @param x A \code{\link{DTSg}} object (S3 method only).
#' @param deep A logical specifying if a deep copy is made (for consistency with
#'  \code{\link[R6]{R6Class}} the default is \code{FALSE}, but should generally
#'  be set to \code{TRUE}).
#' @param \dots Not used (S3 method only).
#'
#' @return Returns a \code{\link{DTSg}} object.
#'
#' @seealso \code{\link{DTSg}}, \code{\link[R6]{R6Class}}
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
#' Apply Function Column-wise
#'
#' Applies an arbitrary function to selected columns of a \code{\link{DTSg}}
#'  object.
#'
#' @param x A \code{\link{DTSg}} object (S3 method only).
#' @param fun A \code{\link{function}}. Its return value must be of length one.
#' @param \dots Further arguments passed on to \code{fun}.
#' @param cols A character vector specifying the columns to apply \code{fun} to.
#' @param clone A logical specifying if the object is modified in place or if a
#'  clone (copy) is made beforehand.
#' @param resultCols An optional character vector of the same length as
#'  \code{cols}. Non-existing columns specified in this argument are added and
#'  existing columns are overwritten by the return values of \code{fun}. Columns
#'  are matched element-wise between \code{resultCols} and \code{cols}.
#' @param suffix An optional character string. The return values of \code{fun}
#'  are added as new columns with names consisting of the columns specified in
#'  \code{cols} and this suffix. Existing columns are never overwritten. Only
#'  used when \code{resultCols} is not specified.
#' @param funby One of the temporal aggregation level functions described in
#'  \code{\link{TALFs}} or a user defined temporal aggregation level function.
#'  Can be used to apply functions like \code{\link{cumsum}} to a certain
#'  temporal level. See examples and \code{\link{aggregate}} for further
#'  information.
#' @param ignoreDST A logical specifying if day saving time is ignored during
#'  formation of the temporal level. See \code{\link{aggregate}} for further
#'  information.
#'
#' @details
#' In addition to the \code{\dots} argument, this method hands over a
#'  \code{\link{list}} argument with helper data called \code{.helpers} to
#'  \code{fun}. \code{.helpers} contains the following named elements:
#'  \itemize{
#'    \item \emph{.dateTime:} A \code{\link{POSIXct}} vector containing the
#'      \emph{.dateTime} column.
#'    \item \emph{periodicity:} Same as \emph{periodicity} field. See
#'      \code{\link{DTSg}} for further information.
#'    \item \emph{minLag:} A \code{\link{difftime}} object containing the
#'      minimum time difference between two subsequent timestamps.
#'    \item \emph{maxLag:} A \code{\link{difftime}} object containing the
#'      maximum time difference between two subsequent timestamps.
#'  }
#'
#' @return Returns a \code{\link{DTSg}} object.
#'
#' @seealso \code{\link{DTSg}}, \code{\link{function}}, \code{\link{cols}},
#'  \code{\link{TALFs}}, \code{\link{aggregate}}, \code{\link{list}},
#'  \code{\link{POSIXct}}, \code{\link{difftime}},
#'  \code{\link{interpolateLinear}}
#'
#' @examples
#' # new DTSg object
#' x <- DTSg$new(values = flow)
#'
#' # linear interpolation of missing values
#' ## R6 method
#' x$colapply(fun = interpolateLinear)
#'
#' ## S3 method
#' colapply(x = x, fun = interpolateLinear)
#'
#' # daily cumulative sums per month
#' ## R6 method
#' x$colapply(fun = function(x, ...) {cumsum(x)}, funby = byYm____)
#'
#' ## S3 method
#' colapply(x = x, fun = function(x, ...) {cumsum(x)}, funby = byYm____)
#'
#' # calculate moving averages with the help of 'runner'
#' if (requireNamespace("runner", quietly = TRUE)) {
#'   wrapper <- function(x, f, k, lag, ...) {
#'     runner::runner(x, f, k, lag)
#'   }
#'   wrapper2 <- function(x, f, k, lag, .helpers) {
#'     runner::runner(x, f, k, lag, .helpers[[".dateTime"]])
#'   }
#'
#'   ## R6 method
#'   x$colapply(fun = wrapper , f = mean, k = 5       , lag = -2       )
#'   x$colapply(fun = wrapper2, f = mean, k = "5 days", lag = "-2 days")
#'
#'   ## S3 method
#'   colapply(x = x, fun = wrapper , f = mean, k = 5       , lag = -2       )
#'   colapply(x = x, fun = wrapper2, f = mean, k = "5 days", lag = "-2 days")
#' }
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
#' Get Column Names
#'
#' Queries all column names of a \code{\link{DTSg}} object, those of certain
#'  \code{\link{class}}es and/or those matching a certain pattern only.
#'
#' @param x A \code{\link{DTSg}} object (S3 method only).
#' @param class An optional character vector matched to the most specific class
#'  (first element) of each column's \code{\link{class}} vector.
#' @param pattern An optional character string passed on to the \code{pattern}
#'  argument of \code{\link{grep}}.
#' @param \dots Further arguments passed on to \code{\link{grep}}. The
#'  \code{value} argument is not allowed here.
#'
#' @return Returns a character vector.
#'
#' @seealso \code{\link{DTSg}}, \code{\link{class}}, \code{\link{grep}}
#'
#' @examples
#' # new DTSg object
#' x <- DTSg$new(values = flow)
#'
#' # get names of numeric columns
#' ## R6 method
#' x$cols(class = "numeric")
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
#' Get Column Vector
#'
#' Queries the values of a column of a \code{\link{DTSg}} object.
#'
#' @param x A \code{\link{DTSg}} object (S3 method only).
#' @param col A character string specifying a column name.
#' @param \dots Not used (S3 method only).
#'
#' @return Returns a vector or a \code{\link{list}} in case of a
#'  \code{\link{list}} column.
#'
#' @seealso \code{\link{DTSg}}, \code{\link{cols}}, \code{\link{list}}
#'
#' @examples
#' # new DTSg object
#' x <- DTSg$new(values = flow)
#'
#' # get values of "flow" column
#' ## R6 method
#' x$getCol(col = "flow")
#'
#' ## S3 method
#' getCol(x = x, col = "flow")
#'
#' @aliases getCol
#'
#' @export
getCol.DTSg <- S3WrapperGenerator(expression(DTSg$public_methods$getCol))

#### merge ####
#' Merge Two DTSg Objects
#'
#' Joins two \code{\link{DTSg}} objects based on their \emph{.dateTime} column.
#'  Their time zones and \emph{aggregated} fields must be the same.
#'
#' @param x A \code{\link{DTSg}} object (S3 method only).
#' @param y A \code{\link{DTSg}} object or an object coercible to one. See
#'  \code{\link{new}} for further information.
#' @param \dots Further arguments passed on to \code{\link[data.table]{merge}}.
#'  As the \code{by}, \code{by.x} and \code{by.y} arguments can endanger the
#'  integrity of the object, they are not allowed here.
#' @param clone A logical specifying if the object is modified in place or if a
#'  clone (copy) is made beforehand.
#'
#' @return Returns a \code{\link{DTSg}} object.
#'
#' @seealso \code{\link{DTSg}}, \code{\link{new}},
#'  \code{\link[data.table]{merge}}
#'
#' @examples
#' # new DTSg object
#' x <- DTSg$new(values = flow)
#'
#' # merge with data.table
#' ## R6 method
#' x$merge(y = flow, suffixes = c("_1", "_2"))
#'
#' ## S3 method
#' merge(x = x, y = flow, suffixes = c("_1", "_2"))
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
#' List Missing Values
#'
#' Lists the missing values of selected columns of a \code{\link{DTSg}} object
#'  with recognised periodicity.
#'
#' @param x A \code{\link{DTSg}} object (S3 method only).
#' @param cols A character vector specifying the columns whose missing values
#'  shall be listed.
#' @param \dots Not used (S3 method only).
#'
#' @return Returns a \code{\link[data.table]{data.table}} with five columns:
#'  \itemize{
#'    \item \emph{.col:} the column name.
#'    \item \emph{.group:} the ID of the missing values group within each
#'     column.
#'    \item \emph{.from:} the start date of the missing values group.
#'    \item \emph{.to:} the end date of the missing values group.
#'    \item \emph{.n:} the number of missing values in the group.
#'  }
#'
#' @seealso \code{\link{DTSg}}, \code{\link{cols}},
#'  \code{\link[data.table]{data.table}}
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
  fast = FALSE,
  swallow = FALSE,
  na.status = c("explicit", "implicit", "undecided")
) {} # no R CMD check warning
setClass("DTSg", slots = c(. = "logical"))
setMethod(
  "initialize",
  "DTSg",
  function(.Object, ...) {
    DTSg$new(...)
  }
)

#### plot ####
#' Plot Time Series
#'
#' Displays an interactive plot of a \code{\link{DTSg}} object. This method
#'  requires \pkg{dygraphs} and \pkg{RColorBrewer} to be installed. Its main
#'  purpose is not to make pretty plots, but rather to offer a possibility to
#'  interactively explore time series. The title of the plot and the label of
#'  its primary axis are automatically generated out of the object's metadata
#'  (fields). See \code{\link{DTSg}} for further information.
#'
#' @param x A \code{\link{DTSg}} object (S3 method only).
#' @param from A \code{\link{POSIXct}} date with the same time zone as the time
#'  series or a character string coercible to one. The time series is plotted
#'  from this date on.
#' @param to A \code{\link{POSIXct}} date with the same time zone as the time
#'  series or a character string coercible to one. The time series is plotted
#'  up to this date.
#' @param cols A character vector specifying the columns whose values shall be
#'  plotted.
#' @param secAxisCols An optional character vector specifying the columns whose
#'  values shall be plotted on a secondary axis. Must be a subset of
#'  \code{cols}.
#' @param secAxisLabel A character string specifying the label of the optional
#'  secondary axis.
#' @param \dots Not used (S3 method only).
#'
#' @return Returns a \code{\link{DTSg}} object.
#'
#' @seealso \code{\link{DTSg}}, \code{\link[dygraphs]{dygraph}},
#'  \code{\link{POSIXct}}, \code{\link{cols}}
#'
#' @examples
#' # new DTSg object
#' x <- DTSg$new(values = flow)
#'
#' # plot time series
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
#' Print Time Series
#'
#' Prints a \code{\link{DTSg}} object.
#'
#' @param x A \code{\link{DTSg}} object (S3 method only).
#' @param \dots Not used (S3 method only).
#'
#' @return Returns a \code{\link{DTSg}} object.
#'
#' @seealso \code{\link{DTSg}}
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
#' Object Integrity
#'
#' Checks the integrity of a \code{\link{DTSg}} object and tries to
#'  automatically (re-)detect its periodicity. Normally, there is no reason for
#'  a user to call this method. The only exception is stated in
#'  \code{\link{values}}.
#'
#' @param x A \code{\link{DTSg}} object (S3 method only).
#' @param \dots Not used (S3 method only).
#'
#' @return Returns a \code{\link{DTSg}} object.
#'
#' @seealso \code{\link{DTSg}}, \code{\link{values}}
#'
#' @examples
#' # new DTSg object
#' x <- DTSg$new(values = flow)
#'
#' # check object integrity
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
#' Rolling Window Function
#'
#' Applies an arbitrary function to a rolling window of selected columns of a
#'  \code{\link{DTSg}} object with recognised periodicity.
#'
#' @param x A \code{\link{DTSg}} object (S3 method only).
#' @param fun A \code{\link{function}}. Its return value must be of length one.
#' @param \dots Further arguments passed on to \code{fun}.
#' @param cols A character vector specifying the columns whose rolling window
#'  \code{fun} shall be applied to.
#' @param before An integerish value specifying the size of the window in time
#'  steps before the \dQuote{center} of the rolling window.
#' @param after An integerish value specifying the size of the window in time
#'  steps after the \dQuote{center} of the rolling window.
#' @param weights A character string specifying a method to calculate weights
#'  for \code{fun}, for instance, \code{\link{weighted.mean}}. See details for
#'  further information.
#' @param parameters A \code{\link{list}} specifying parameters for
#'  \code{weights}. See details for further information.
#' @param clone A logical specifying if the object is modified in place or if a
#'  clone (copy) is made beforehand.
#' @param resultCols An optional character vector of the same length as
#'  \code{cols}. Non-existing columns specified in this argument are added and
#'  existing columns are overwritten by the return values of \code{fun}. Columns
#'  are matched element-wise between \code{resultCols} and \code{cols}.
#' @param suffix An optional character string. The return values of \code{fun}
#'  are added as new columns with names consisting of the columns specified in
#'  \code{cols} and this suffix. Existing columns are never overwritten. Only
#'  used when \code{resultCols} is not specified.
#' @param memoryOverCPU A logical specifying if memory usage is preferred over
#'  CPU usage for this method. The former is generally faster for smaller
#'  windows and shorter time series, the latter for bigger windows and longer
#'  time series or might even be the only way that works depending on the
#'  available hardware.
#'
#' @details
#' In addition to the \code{\dots} argument, this method hands over the weights
#'  as a numeric vector (\code{w} argument) and a \code{\link{list}} argument
#'  with helper data called \code{.helpers} to \code{fun}. \code{.helpers}
#'  contains the following named elements:
#'  \itemize{
#'    \item \emph{before:} Same as \code{before} argument.
#'    \item \emph{after:} Same as \code{after} argument.
#'    \item \emph{windowSize:} Size of the rolling window
#'      (\code{before + 1L + after}).
#'    \item \emph{centerIndex:} Index of the \dQuote{center} of the rolling
#'      window (\code{before + 1L}).
#'  }
#'
#' Currently, only one method to calculate weights is supported:
#'  \code{"inverseDistance"}. The distance \eqn{d} of the \dQuote{center} is one
#'  and each time step away from the \dQuote{center} adds one to it. So, for
#'  example, the distance of a timestamp three steps away from the
#'  \dQuote{center} is four. Additionally, the calculation of the weights
#'  accepts a power \eqn{p} parameter as a named element of a \code{\link{list}}
#'  provided through the \code{parameters} argument:
#'  \eqn{\frac{1}{d^p}}{1 / d^p}.
#'
#' @return Returns a \code{\link{DTSg}} object.
#'
#' @seealso \code{\link{DTSg}}, \code{\link{function}}, \code{\link{cols}},
#'  \code{\link{list}}
#'
#' @examples
#' # new DTSg object
#' x <- DTSg$new(values = flow)
#'
#' # calculate a moving average
#' ## R6 method
#' x$rollapply(fun = mean, na.rm = TRUE, before = 2, after = 2)
#'
#' ## S3 method
#' rollapply(x = x, fun = mean, na.rm = TRUE, before = 2, after = 2)
#'
#' @aliases rollapply
#'
#' @export
rollapply.DTSg <- S3WrapperGenerator(expression(DTSg$public_methods$rollapply))

#### setCols ####
#' @export
setCols <- function(x, ...) {
  UseMethod("setCols", x)
}
#' Set Columns' Values
#'
#' Set the values of columns, add columns to and/or remove columns from a
#'  \code{\link{DTSg}} object. The values can optionally be set for certain rows
#'  only.
#'
#' @param x A \code{\link{DTSg}} object (S3 method only).
#' @param i An integerish vector indexing rows (positive numbers pick and
#'  negative numbers omit rows) or a filter expression accepted by the \code{i}
#'  argument of \code{\link[data.table]{data.table}}. Filter expressions can
#'  contain the special symbol \code{.N}. See
#'  (\code{\link[data.table]{special-symbols}}) for further information.
#' @param cols A character vector specifying the columns whose values shall be
#'  set. The values of the \emph{.dateTime} column cannot be set.
#' @param values A vector or list-like object of replacement and/or new values
#'  accepted by the \code{value} argument of \pkg{data.table}'s
#'  \code{\link[data.table]{set}} function. \code{\link{NULL}} as a value
#'  removes a column.
#' @param clone A logical specifying if the object is modified in place or if a
#'  clone (copy) is made beforehand.
#' @param \dots Not used (S3 method only).
#'
#' @return Returns a \code{\link{DTSg}} object.
#'
#' @seealso \code{\link{DTSg}}, \code{\link[data.table]{data.table}},
#'  \code{\link[data.table]{special-symbols}}, \code{\link{cols}},
#'  \code{\link[data.table]{set}}, \code{\link{NULL}}
#'
#' @examples
#' # new DTSg object
#' x <- DTSg$new(values = flow)
#'
#' # limit river flow to 100
#' ## R6 method
#' x$setCols(i = flow > 100, cols = "flow", values = 100)
#'
#' ## S3 method
#' setCols(x = x, i = flow > 100, cols = "flow", values = 100)
#'
#' @aliases setCols
#'
#' @export
setCols.DTSg <- S3WrapperGenerator(expression(DTSg$public_methods$setCols))

#### subset ####
#' Subset Time Series
#'
#' Filter rows and/or select columns of a \code{\link{DTSg}} object.
#'
#' @param x A \code{\link{DTSg}} object (S3 method only).
#' @param i An integerish vector indexing rows (positive numbers pick and
#'  negative numbers omit rows) or a filter expression accepted by the \code{i}
#'  argument of \code{\link[data.table]{data.table}}. Filter expressions can
#'  contain the special symbol \code{.N}. See
#'  (\code{\link[data.table]{special-symbols}}) for further information.
#' @param cols A character vector specifying the columns to select. The
#'  \emph{.dateTime} column is always selected and cannot be part of it.
#' @param funby One of the temporal aggregation level functions described in
#'  \code{\link{TALFs}} or a user defined temporal aggregation level function.
#'  Can be used to, for instance, select the last two observations of a certain
#'  temporal level. See examples and \code{\link{aggregate}} for further
#'  information.
#' @param ignoreDST A logical specifying if day saving time is ignored during
#'  formation of the temporal level. See \code{\link{aggregate}} for further
#'  information.
#' @param na.status A character string. Either \code{"explicit"}, which makes
#'  missing timestamps according to the recognised periodicity explicit, or
#'  \code{"implicit"}, which removes timestamps with missing values on all value
#'  columns. Please note that filtering rows and having or making missing
#'  timestamps explicit equals to setting the values of all other timestamps to
#'  missing. Its default value is therefore \code{"implicit"}. To simply subset
#'  a complete range of a \code{\link{DTSg}} object while leaving
#'  \code{na.status} untouched, \code{\link{alter}} is probably the better
#'  choice.
#' @param clone A logical specifying if the object is modified in place or if a
#'  clone (copy) is made beforehand.
#' @param \dots Not used (S3 method only).
#'
#' @return Returns a \code{\link{DTSg}} object.
#'
#' @seealso \code{\link{DTSg}}, \code{\link[data.table]{data.table}},
#'  \code{\link[data.table]{special-symbols}}, \code{\link{cols}},
#'  \code{\link{TALFs}}, \code{\link{alter}}
#'
#' @examples
#' # new DTSg object
#' x <- DTSg$new(values = flow)
#'
#' # filter for the last six rows
#' ## R6 method
#' x$subset(i = (.N - 5):.N)
#'
#' ## S3 method
#' subset(x = x, i = (.N - 5):.N)
#'
#' # filter for the first two observations per year
#' ## R6 method
#' x$subset(i = 1:2, funby = function(x, ...) {data.table::year(x)})
#'
#' ## S3 method
#' subset(x = x, i = 1:2, funby = function(x, ...) {data.table::year(x)})
#'
#' @aliases subset
#'
#' @export
subset.DTSg <- S3WrapperGenerator(expression(DTSg$public_methods$subset))

#### summary ####
#' Time Series Summary
#'
#' Calculates summary statistics of selected columns of a \code{\link{DTSg}}
#'  object.
#'
#' @param object A \code{\link{DTSg}} object (S3 method only).
#' @param cols A character vector specifying the columns whose values shall be
#'  summarised.
#' @param \dots Further arguments passed on to \code{\link{summary.data.frame}}.
#'
#' @return Returns a \code{\link{table}}.
#'
#' @seealso \code{\link{DTSg}}, \code{\link{cols}},
#'  \code{\link{summary.data.frame}}, \code{\link{table}}
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
#' Get Values
#'
#' Queries the \emph{values} of a \code{\link{DTSg}} object.
#'
#' @param x A \code{\link{DTSg}} object (S3 method only).
#' @param reference A logical specifying if a copy of the \emph{values} or a
#'  reference to the \emph{values} is returned. See details for further
#'  information.
#' @param drop A logical specifying if the object and all references to it shall
#'  be removed from the global (and only the global) environment after
#'  successfully querying its values. This feature allows for a resource
#'  efficient destruction of a \code{\link{DTSg}} object while preserving its
#'  \emph{values.}
#' @param class A character string specifying the class of the returned
#'  \emph{values.} \code{"data.frame"} only works when either a copy of the
#'  \emph{values} is returned or the object is dropped.
#' @param \dots Not used (S3 method only).
#'
#' @details
#' A reference to the \emph{values} of a \code{\link{DTSg}} object can be used
#'  to modify them in place. This includes the \emph{.dateTime} column, which
#'  serves as the object's time index. Modifying this column can therefore
#'  endanger the object's integrity. In case needs to do so ever arise,
#'  \code{\link{refresh}} should be called immediately afterwards in order to
#'  check the object's integrity.
#'
#' @return Returns a \code{\link[data.table]{data.table}}, a reference to a
#'  \code{\link[data.table]{data.table}} or a \code{\link{data.frame}}.
#'
#' @note
#' The original name of the \emph{.dateTime} column is restored when not
#'  returned as a reference or when dropped.
#'
#' @seealso \code{\link{DTSg}}, \code{\link{refresh}},
#'  \code{\link[data.table]{data.table}}, \code{\link{data.frame}}
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
