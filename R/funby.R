#### Calls ####
byFasttimeY_____call <- quote(fasttime::fastPOSIXct(sprintf("%04d-01-01"                   , year(.dateTime)                                                                                          ), tz = "UTC"))
byFasttimeYQ____call <- quote(fasttime::fastPOSIXct(sprintf("%04d-%02d-01"                 , year(.dateTime), quarter(.dateTime) * 3L - 2L                                                            ), tz = "UTC"))
byFasttimeYm____call <- quote(fasttime::fastPOSIXct(sprintf("%04d-%02d-01"                 , year(.dateTime), month(.dateTime)                                                                        ), tz = "UTC"))
byFasttimeYmd___call <- quote(fasttime::fastPOSIXct(sprintf("%04d-%02d-%02d"               , year(.dateTime), month(.dateTime), mday(.dateTime)                                                       ), tz = "UTC"))
byFasttimeYmdH__call <- quote(fasttime::fastPOSIXct(sprintf("%04d-%02d-%02d %02d:00:00"    , year(.dateTime), month(.dateTime), mday(.dateTime), hour(.dateTime)                                      ), tz = "UTC"))
byFasttimeYmdHM_call <- quote(fasttime::fastPOSIXct(sprintf("%04d-%02d-%02d %02d:%02d:00"  , year(.dateTime), month(.dateTime), mday(.dateTime), hour(.dateTime), minute(.dateTime)                   ), tz = "UTC"))
byFasttimeYmdHMScall <- quote(fasttime::fastPOSIXct(sprintf("%04d-%02d-%02d %02d:%02d:%02d", year(.dateTime), month(.dateTime), mday(.dateTime), hour(.dateTime), minute(.dateTime), second(.dateTime)), tz = "UTC"))

byFasttime______call <- quote(fasttime::fastPOSIXct(    rep("2199-01-01",            length(.dateTime)           ), tz = "UTC"))
byFasttime_Q____call <- quote(fasttime::fastPOSIXct(sprintf("2199-%02d-01",          quarter(.dateTime) * 3L - 2L), tz = "UTC"))
byFasttime_m____call <- quote(fasttime::fastPOSIXct(sprintf("2199-%02d-01",          month(.dateTime)            ), tz = "UTC"))
byFasttime___H__call <- quote(fasttime::fastPOSIXct(sprintf("2199-01-01 %02d:00:00", hour(.dateTime)             ), tz = "UTC"))
byFasttime____M_call <- quote(fasttime::fastPOSIXct(sprintf("2199-01-01 00:%02d:00", minute(.dateTime)           ), tz = "UTC"))
byFasttime_____Scall <- quote(fasttime::fastPOSIXct(sprintf("2199-01-01 00:00:%02d", second(.dateTime)           ), tz = "UTC"))

byY_____call <- quote(as.POSIXct(sprintf("%04d-01-01"  , year(.dateTime)                              ), tz = .helpers$timezone))
byYQ____call <- quote(as.POSIXct(sprintf("%04d-%02d-01", year(.dateTime), quarter(.dateTime) * 3L - 2L), tz = .helpers$timezone))
byYm____call <- quote(as.POSIXct(sprintf("%04d-%02d-01", year(.dateTime), month(.dateTime)            ), tz = .helpers$timezone))
byYmd___call <- quote(as.POSIXct(  trunc(.dateTime     , units = "days"                               ), tz = .helpers$timezone))
byYmdH__call <- quote(as.POSIXct(  trunc(.dateTime     , units = "hours"                              ), tz = .helpers$timezone))
byYmdHM_call <- quote(as.POSIXct(  trunc(.dateTime     , units = "mins"                               ), tz = .helpers$timezone))
byYmdHMScall <- quote(as.POSIXct(  trunc(.dateTime     , units = "secs"                               ), tz = .helpers$timezone))

by______call <- quote(as.POSIXct(    rep("2199-01-01"           , length(.dateTime)           ), tz = .helpers$timezone))
by_Q____call <- quote(as.POSIXct(sprintf("2199-%02d-01"         , quarter(.dateTime) * 3L - 2L), tz = .helpers$timezone))
by_m____call <- quote(as.POSIXct(sprintf("2199-%02d-01"         , month(.dateTime)            ), tz = .helpers$timezone))
by___H__call <- quote(as.POSIXct(sprintf("2199-01-01 %02d:00:00", hour(.dateTime)             ), tz = .helpers$timezone))
by____M_call <- quote(as.POSIXct(sprintf("2199-01-01 00:%02d:00", minute(.dateTime)           ), tz = .helpers$timezone))
by_____Scall <- quote(as.POSIXct(sprintf("2199-01-01 00:00:%02d", second(.dateTime)           ), tz = .helpers$timezone))

#### Functions ####
to.UTCdateTime <- function(.dateTime, .helpers) {
  assert_is_periodicity_recognised(.helpers$periodicity)

  from <- .dateTime[1L]
  if (as.POSIXlt(from)$isdst) {
    from <- from - 3600L
  }
  .dateTime <- seq(
    as.POSIXct(as.character(from), tz = "UTC"),
    by = .helpers$periodicity,
    along.with = .dateTime
  )
  if (grepl("^\\d+ (month|year)(s?)$", .helpers$periodicity) &&
      mday(.dateTime[1L]) > 28L) {
    .dateTime <- rollback(.dateTime, .helpers$periodicity)
  }

  .dateTime
}

#' Temporal Aggregation Level Functions
#'
#' Simply specify one of these functions as \code{funby} argument of
#'  \code{\link{DTSg}} objects' \code{\link{aggregate}} method. The method does
#'  the rest of the work. See details for further information. Other uses are
#'  possible, but not recommended.
#'
#' @param .dateTime A \code{\link{POSIXct}} vector.
#' @param .helpers A list with helper data as handed over by \code{\link{DTSg}}
#'  objects' \code{\link{aggregate}} method.
#'
#' @details
#' There are two families of temporal aggregation level functions. The
#'  one family truncates timestamps (truncating family), the other extracts a
#'  certain part of them (extracting family). Each family comes in two flavours:
#'  one using \code{\link[fasttime]{fastPOSIXct}} of \pkg{fasttime}, the other
#'  solely relying on base \R. The \pkg{fasttime} versions work with UTC time
#'  series only and are limited to dates between the years 1970 and 2199, but
#'  generally are faster for the extracting family of functions.
#'
#' The truncating family sets timestamps to the lowest possible time of the
#'  corresponding temporal aggregation level:
#'  \itemize{
#'    \item \code{*Y_____} truncates to year,    e.g., \emph{2000-11-11 11:11:11.1} becomes \emph{2000-01-01 00:00:00.0}
#'    \item \code{*YQ____} truncates to quarter, e.g., \emph{2000-11-11 11:11:11.1} becomes \emph{2000-10-01 00:00:00.0}
#'    \item \code{*Ym____} truncates to month,   e.g., \emph{2000-11-11 11:11:11.1} becomes \emph{2000-11-01 00:00:00.0}
#'    \item \code{*Ymd___} truncates to day,     e.g., \emph{2000-11-11 11:11:11.1} becomes \emph{2000-11-11 00:00:00.0}
#'    \item \code{*YmdH__} truncates to hour,    e.g., \emph{2000-11-11 11:11:11.1} becomes \emph{2000-11-11 11:00:00.0}
#'    \item \code{*YmdHM_} truncates to minute,  e.g., \emph{2000-11-11 11:11:11.1} becomes \emph{2000-11-11 11:11:00.0}
#'    \item \code{*YmdHMS} truncates to second,  e.g., \emph{2000-11-11 11:11:11.1} becomes \emph{2000-11-11 11:11:11.0}
#'  }
#'
#' By convention, the extracting family sets the year to 2199 and extracts a
#'  certain part of timestamps:
#'  \itemize{
#'    \item \code{*______} extracts nothing,      i.e.,               all timestamps become  \emph{2199-01-01 00:00:00.0}
#'    \item \code{*_Q____} extracts the quarters, e.g., \emph{2000-11-11 11:11:11.1} becomes \emph{2199-10-01 00:00:00.0}
#'    \item \code{*_m____} extracts the months,   e.g., \emph{2000-11-11 11:11:11.1} becomes \emph{2199-11-01 00:00:00.0}
#'    \item \code{*___H__} extracts the hours,    e.g., \emph{2000-11-11 11:11:11.1} becomes \emph{2199-01-01 11:00:00.0}
#'    \item \code{*____M_} extracts the minutes,  e.g., \emph{2000-11-11 11:11:11.1} becomes \emph{2199-01-01 00:11:00.0}
#'    \item \code{*_____S} extracts the seconds,  e.g., \emph{2000-11-11 11:11:11.1} becomes \emph{2199-01-01 00:00:11.0}
#'  }
#'
#' @return All functions return a \code{\link{POSIXct}} vector with timestamps
#'  corresponding to the function's temporal aggregation level.
#'
#' @seealso \code{\link{DTSg}}, \code{\link{aggregate}},
#'  \code{\link[fasttime]{fastPOSIXct}}
#'
#' @name TALFs
NULL

#' @rdname TALFs
#' @export
byFasttimeY_____ <- function(.dateTime, .helpers) {
  assert_is_fasttime_ok(.dateTime, .helpers)
  eval(byFasttimeY_____call)
}
#' @rdname TALFs
#' @export
byFasttimeYQ____ <- function(.dateTime, .helpers) {
  assert_is_fasttime_ok(.dateTime, .helpers)
  eval(byFasttimeYQ____call)
}
#' @rdname TALFs
#' @export
byFasttimeYm____ <- function(.dateTime, .helpers) {
  assert_is_fasttime_ok(.dateTime, .helpers)
  eval(byFasttimeYm____call)
}
#' @rdname TALFs
#' @export
byFasttimeYmd___ <- function(.dateTime, .helpers) {
  assert_is_fasttime_ok(.dateTime, .helpers)
  eval(byFasttimeYmd___call)
}
#' @rdname TALFs
#' @export
byFasttimeYmdH__ <- function(.dateTime, .helpers) {
  assert_is_fasttime_ok(.dateTime, .helpers)
  eval(byFasttimeYmdH__call)
}
#' @rdname TALFs
#' @export
byFasttimeYmdHM_ <- function(.dateTime, .helpers) {
  assert_is_fasttime_ok(.dateTime, .helpers)
  eval(byFasttimeYmdHM_call)
}
#' @rdname TALFs
#' @export
byFasttimeYmdHMS <- function(.dateTime, .helpers) {
  assert_is_fasttime_ok(.dateTime, .helpers)
  eval(byFasttimeYmdHMScall)
}

#' @rdname TALFs
#' @export
byFasttime______ <- function(.dateTime, .helpers) {
  assert_is_fasttime_ok(.dateTime, .helpers)
  eval(byFasttime______call)
}
#' @rdname TALFs
#' @export
byFasttime_Q____ <- function(.dateTime, .helpers) {
  assert_is_fasttime_ok(.dateTime, .helpers)
  eval(byFasttime_Q____call)
}
#' @rdname TALFs
#' @export
byFasttime_m____ <- function(.dateTime, .helpers) {
  assert_is_fasttime_ok(.dateTime, .helpers)
  eval(byFasttime_m____call)
}
#' @rdname TALFs
#' @export
byFasttime___H__ <- function(.dateTime, .helpers) {
  assert_is_fasttime_ok(.dateTime, .helpers)
  eval(byFasttime___H__call)
}
#' @rdname TALFs
#' @export
byFasttime____M_ <- function(.dateTime, .helpers) {
  assert_is_fasttime_ok(.dateTime, .helpers)
  eval(byFasttime____M_call)
}
#' @rdname TALFs
#' @export
byFasttime_____S <- function(.dateTime, .helpers) {
  assert_is_fasttime_ok(.dateTime, .helpers)
  eval(byFasttime_____Scall)
}

#' @rdname TALFs
#' @export
byY_____ <- function(.dateTime, .helpers) {
  if (.helpers$ignoreDST && .helpers$timezone != "UTC") {
    .dateTime <- to.UTCdateTime(.dateTime, .helpers)
  }
  eval(byY_____call)
}
#' @rdname TALFs
#' @export
byYQ____ <- function(.dateTime, .helpers) {
  if (.helpers$ignoreDST && .helpers$timezone != "UTC") {
    .dateTime <- to.UTCdateTime(.dateTime, .helpers)
  }
  eval(byYQ____call)
}
#' @rdname TALFs
#' @export
byYm____ <- function(.dateTime, .helpers) {
  if (.helpers$ignoreDST && .helpers$timezone != "UTC") {
    .dateTime <- to.UTCdateTime(.dateTime, .helpers)
  }
  eval(byYm____call)
}
#' @rdname TALFs
#' @export
byYmd___ <- function(.dateTime, .helpers) {
  if (.helpers$ignoreDST && .helpers$timezone != "UTC") {
    .dateTime <- to.UTCdateTime(.dateTime, .helpers)
  }
  eval(byYmd___call)
}
#' @rdname TALFs
#' @export
byYmdH__ <- function(.dateTime, .helpers) {
  eval(byYmdH__call)
}
#' @rdname TALFs
#' @export
byYmdHM_ <- function(.dateTime, .helpers) {
  eval(byYmdHM_call)
}
#' @rdname TALFs
#' @export
byYmdHMS <- function(.dateTime, .helpers) {
  eval(byYmdHMScall)
}

#' @rdname TALFs
#' @export
by______ <- function(.dateTime, .helpers) {
  eval(by______call)
}
#' @rdname TALFs
#' @export
by_Q____ <- function(.dateTime, .helpers) {
  if (.helpers$ignoreDST && .helpers$timezone != "UTC") {
    .dateTime <- to.UTCdateTime(.dateTime, .helpers)
  }
  eval(by_Q____call)
}
#' @rdname TALFs
#' @export
by_m____ <- function(.dateTime, .helpers) {
  if (.helpers$ignoreDST && .helpers$timezone != "UTC") {
    .dateTime <- to.UTCdateTime(.dateTime, .helpers)
  }
  eval(by_m____call)
}
#' @rdname TALFs
#' @export
by___H__ <- function(.dateTime, .helpers) {
  if (.helpers$ignoreDST && .helpers$timezone != "UTC") {
    .dateTime <- to.UTCdateTime(.dateTime, .helpers)
  }
  eval(by___H__call)
}
#' @rdname TALFs
#' @export
by____M_ <- function(.dateTime, .helpers) {
  eval(by____M_call)
}
#' @rdname TALFs
#' @export
by_____S <- function(.dateTime, .helpers) {
  eval(by_____Scall)
}
