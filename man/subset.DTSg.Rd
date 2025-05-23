% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Swrappers.R
\name{subset.DTSg}
\alias{subset.DTSg}
\alias{subset}
\title{Subset time series data}
\usage{
\method{subset}{DTSg}(
  x,
  i,
  cols = self$cols(),
  funby = NULL,
  ignoreDST = FALSE,
  na.status = "implicit",
  clone = getOption("DTSgClone"),
  multiplier = 1L,
  funbyHelpers = NULL,
  funbyApproach = self$funbyApproach,
  ...
)
}
\arguments{
\item{x}{A \code{\link{DTSg}} object (S3 method only).}

\item{i}{An integerish vector indexing rows (positive numbers pick and
negative numbers omit rows) or a filter expression accepted by the \code{i}
argument of \code{\link[data.table:data.table]{data.table::data.table}}. Filter expressions can contain the
special symbol \code{\link[data.table:special-symbols]{.N}}.}

\item{cols}{A character vector specifying the columns to select. Another
possibility is a character string containing either comma separated column
names, for example, \code{"x,y,z"}, or the start and end column separated by a
colon, for example, \code{"x:z"}. The \emph{.dateTime} column is always selected and
cannot be part of it.}

\item{funby}{One of the temporal aggregation level functions described in
\code{\link{TALFs}} or a user defined temporal aggregation level function. Can be
used to, for instance, select the last two observations of a certain
temporal level. See corresponding section and examples for further
information.}

\item{ignoreDST}{A logical specifying if day saving time shall be ignored
by \code{funby}. See corresponding section for further information.}

\item{na.status}{A character string. Either \code{"explicit"}, which makes missing
timestamps explicit according to the recognised periodicity, or
\code{"implicit"}, which removes timestamps with missing values on all value
columns. See corresponding section for further information.}

\item{clone}{A logical specifying if the object shall be modified in place or
if a deep clone (copy) shall be made beforehand.}

\item{multiplier}{A positive integerish value \dQuote{multiplying} the
temporal aggregation level of certain \code{\link{TALFs}}. See corresponding section
for further information.}

\item{funbyHelpers}{An optional \code{\link{list}} with helper data passed on to
\code{funby}. See corresponding section for further information.}

\item{funbyApproach}{A character string specifying the flavour of the applied
temporal aggregation level function. Either \code{"timechange"}, which utilises
\code{\link[timechange:time_round]{timechange::time_floor}}, or \code{"base"}, which utilises \code{\link{as.POSIXct}}, or
\code{"fasttime"}, which utilises \code{\link[fasttime:fastPOSIXct]{fasttime::fastPOSIXct}}, or \code{"RcppCCTZ"},
which utilises \code{\link[RcppCCTZ:parseDatetime]{RcppCCTZ::parseDatetime}} as the main function for
transforming timestamps.}

\item{...}{Further arguments passed on to \code{fun}.}
}
\value{
Returns a \code{\link{DTSg}} object.
}
\description{
Filters rows and/or selects columns of a \code{\link{DTSg}} object.
}
\section{Status of missing values}{

Please note that filtering rows and having or making missing timestamps
explicit equals to setting the values of all other timestamps to missing. The
default value of \code{na.status} is therefore \code{"implicit"}. To simply filter for
a consecutive range of a \code{\link{DTSg}} object while leaving the \code{na.status}
untouched, \code{\link{alter}} is probably the better choice.
}

\section{User defined TALFs, TALFs helper data and multiplier}{

User defined temporal aggregation level functions have to return a
\code{\link{POSIXct}} vector of the same length as the time series and accept two
arguments: a \code{\link{POSIXct}} vector as its first and a \code{\link{list}} with helper data
as its second. The default elements of this \code{\link{list}} are as follows:
\itemize{
\item \emph{timezone:} Same as the \code{\link[=DTSg]{timezone}} field.
\item \emph{ignoreDST:} Same as the \code{ignoreDST} argument.
\item \emph{periodicity:} Same as the \code{\link[=DTSg]{periodicity}} field.
\item \emph{na.status:} Same as the \code{\link[=DTSg]{na.status}} field.
\item \emph{multiplier:} Same as the \code{multiplier} argument.
\item \emph{funbyApproach:} Same as the \code{funbyApproach} argument.
\item \emph{assertion:} A logical specifying if the TALF is called by an assertion.
}

Any additional element specified in the \code{funbyHelpers} argument is appended
to the end of the default \code{\link{list}}. In case \code{funbyHelpers} contains an
\emph{ignoreDST, multiplier} or \emph{funbyApproach} element, it takes precedence over
the respective method argument. \emph{timezone, periodicity} and \emph{na.status}
elements are rejected, as they are always taken directly from the object.

The temporal aggregation level of certain \code{\link{TALFs}} can be adjusted with the
help of the \code{multiplier} argument. A \code{multiplier} of \code{10}, for example, makes
\code{\link{byY_____}} aggregate to decades instead of years. Another example
is a \code{multiplier} of \code{6} provided to \code{\link{by_m____}}. The function
then aggregates all months of all first and all months of all second half
years instead of all months of all years separately. This feature is
supported by the following \code{\link{TALFs}} of the package:
\itemize{
\item \code{\link{byY_____}}
\item \code{\link{byYm____}}
\item \code{\link{byYmdH__}} (UTC and equivalent as well as all Etc/GMT time zones only)
\item \code{\link{byYmdHM_}}
\item \code{\link{byYmdHMS}}
\item \code{\link{by_m____}}
\item \code{\link{by___H__}} (UTC and equivalent as well as all Etc/GMT time zones only)
\item \code{\link{by____M_}}
\item \code{\link{by_____S}}
}
}

\section{Ignore day saving time}{

\code{ignoreDST} tells a temporal aggregation level function if it is supposed to
ignore day saving time while transforming the timestamps. This can be a
desired feature for time series strictly following the position of the sun
such as hydrological time series. Doing so ensures that diurnal variations
are preserved by all means and all intervals are of the \dQuote{correct}
length, however, a possible limitation might be that when the day saving time
shift cannot be estimated, it is assumed to be one hour long and a warning is
issued. This feature requires that the periodicity of the time series has
been recognised and is supported by the following \code{\link{TALFs}} of the package:
\itemize{
\item \code{\link{byY_____}}
\item \code{\link{byYQ____}}
\item \code{\link{byYm____}}
\item \code{\link{byYmd___}}
\item \code{\link{by_Q____}}
\item \code{\link{by_m____}}
\item \code{\link{by___H__}}
}
}

\examples{
# new DTSg object
x <- DTSg$new(values = flow)

# filter for the first six observations
## R6 method
x$subset(i = 1:6)$print()

## S3 method
print(subset(x = x, i = 1:6))

# filter for the last two observations per year
## R6 method
x$subset(
  i = (.N - 1):.N,
  funby = function(x, ...) {data.table::year(x)}
)$print()

## S3 method
print(subset(
  x = x,
  i = (.N - 1):.N,
  funby = function(x, ...) {data.table::year(x)}
))

}
\seealso{
\code{\link{cols}}, \code{\link{getOption}}
}
