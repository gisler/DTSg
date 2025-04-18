% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TALFs.R
\name{TALFs}
\alias{TALFs}
\alias{byY_____}
\alias{byYQ____}
\alias{byYm____}
\alias{byYmd___}
\alias{byYmdH__}
\alias{byYmdHM_}
\alias{byYmdHMS}
\alias{by______}
\alias{by_Q____}
\alias{by_m____}
\alias{by___H__}
\alias{by____M_}
\alias{by_____S}
\title{Temporal Aggregation Level Functions (TALFs)}
\usage{
byY_____(.dateTime, .helpers)

byYQ____(.dateTime, .helpers)

byYm____(.dateTime, .helpers)

byYmd___(.dateTime, .helpers)

byYmdH__(.dateTime, .helpers)

byYmdHM_(.dateTime, .helpers)

byYmdHMS(.dateTime, .helpers)

by______(.dateTime, .helpers)

by_Q____(.dateTime, .helpers)

by_m____(.dateTime, .helpers)

by___H__(.dateTime, .helpers)

by____M_(.dateTime, .helpers)

by_____S(.dateTime, .helpers)
}
\arguments{
\item{.dateTime}{A \code{\link{POSIXct}} vector.}

\item{.helpers}{A \code{\link{list}} with helper data as handed over by methods of
\code{\link{DTSg}} objects, which support the \code{funby} argument.}
}
\value{
All functions return a \code{\link{POSIXct}} vector with timestamps
corresponding to the function's temporal aggregation level.
}
\description{
Simply hand over one of these functions to the \code{funby} argument of one of the
methods of a \code{\link{DTSg}} object, which supports it. The method then does the
rest of the work. See respective calling method for further information.
Other uses are possible, but not recommended.
}
\section{Families and flavours}{

There are two families of temporal aggregation level functions. The one
family truncates timestamps (truncating family), the other extracts a certain
part of them (extracting family). Each family comes in four flavours: the
first relies solely on base \R, the second utilises \code{\link[fasttime:fastPOSIXct]{fasttime::fastPOSIXct}}
of \pkg{fasttime}, the third \code{\link[RcppCCTZ:parseDatetime]{RcppCCTZ::parseDatetime}} of \pkg{RcppCCTZ}
and the fourth \code{\link[timechange:time_round]{timechange::time_floor}} of \pkg{timechange}.

The \pkg{timechange} flavour generally is the fastest for both families of
functions and all time zones. Second best option for the extracting family of
functions generally is the \pkg{fasttime} flavour, which, however, works with
UTC and equivalent as well as all Etc/GMT time zones only (execute
\code{grep("^(Etc/)?(UTC|UCT|Universal|Zulu)$|^(Etc/)?(GMT(\\\\+|-)?0?|Greenwich)$", OlsonNames(), ignore.case = TRUE, value = TRUE)} for a full list of supported
time zones) and is limited to timestamps between the years 1970 and 2199. For
time zones other than UTC and equivalent the \pkg{RcppCCTZ} flavour generally
is the second best option.

Use the \code{funbyApproach} argument of the respective calling method in order to
specify the utilised flavour.

The truncating family sets timestamps to the lowest possible point in time of
the corresponding temporal aggregation level:
\itemize{
\item \code{byY_____} truncates to years,    e.g. \emph{2000-11-11 11:11:11.1} becomes \emph{2000-01-01 00:00:00.0}
\item \code{byYQ____} truncates to quarters, e.g. \emph{2000-11-11 11:11:11.1} becomes \emph{2000-10-01 00:00:00.0}
\item \code{byYm____} truncates to months,   e.g. \emph{2000-11-11 11:11:11.1} becomes \emph{2000-11-01 00:00:00.0}
\item \code{byYmd___} truncates to days,     e.g. \emph{2000-11-11 11:11:11.1} becomes \emph{2000-11-11 00:00:00.0}
\item \code{byYmdH__} truncates to hours,    e.g. \emph{2000-11-11 11:11:11.1} becomes \emph{2000-11-11 11:00:00.0}
\item \code{byYmdHM_} truncates to minutes,  e.g. \emph{2000-11-11 11:11:11.1} becomes \emph{2000-11-11 11:11:00.0}
\item \code{byYmdHMS} truncates to seconds,  e.g. \emph{2000-11-11 11:11:11.1} becomes \emph{2000-11-11 11:11:11.0}
}

By convention, the extracting family sets the year to 2199 and extracts a
certain part of timestamps:
\itemize{
\item \code{by______} extracts nothing,      i.e.          all timestamps become  \emph{2199-01-01 00:00:00.0}
\item \code{by_Q____} extracts the quarters, e.g. \emph{2000-11-11 11:11:11.1} becomes \emph{2199-10-01 00:00:00.0}
\item \code{by_m____} extracts the months,   e.g. \emph{2000-11-11 11:11:11.1} becomes \emph{2199-11-01 00:00:00.0}
\item \code{by___H__} extracts the hours,    e.g. \emph{2000-11-11 11:11:11.1} becomes \emph{2199-01-01 11:00:00.0}
\item \code{by____M_} extracts the minutes,  e.g. \emph{2000-11-11 11:11:11.1} becomes \emph{2199-01-01 00:11:00.0}
\item \code{by_____S} extracts the seconds,  e.g. \emph{2000-11-11 11:11:11.1} becomes \emph{2199-01-01 00:00:11.0}
}
}

\seealso{
\code{\link{aggregate}}, \code{\link{colapply}}, \code{\link{subset}}
}
