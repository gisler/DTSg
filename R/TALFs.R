# Calls, multiplier == 1L ####
byFasttimeY_____call <- quote(fasttime::fastPOSIXct(sprintf("%04d-01-01"                   , year(.dateTime)                                                                                            ), tz = .helpers[["timezone"]]))
byFasttimeYQ____call <- quote(fasttime::fastPOSIXct(sprintf("%04d-%02d-01"                 , year(.dateTime), quarter(.dateTime) * 3L - 2L                                                              ), tz = .helpers[["timezone"]]))
byFasttimeYm____call <- quote(fasttime::fastPOSIXct(sprintf("%04d-%02d-01"                 , year(.dateTime),   month(.dateTime)                                                                        ), tz = .helpers[["timezone"]]))
byFasttimeYmd___call <- quote(fasttime::fastPOSIXct(sprintf("%04d-%02d-%02d"               , year(.dateTime),   month(.dateTime), mday(.dateTime)                                                       ), tz = .helpers[["timezone"]]))
byFasttimeYmdH__call <- quote(fasttime::fastPOSIXct(sprintf("%04d-%02d-%02d %02d:00:00"    , year(.dateTime),   month(.dateTime), mday(.dateTime), hour(.dateTime)                                      ), tz = .helpers[["timezone"]]))
byFasttimeYmdHM_call <- quote(fasttime::fastPOSIXct(sprintf("%04d-%02d-%02d %02d:%02d:00"  , year(.dateTime),   month(.dateTime), mday(.dateTime), hour(.dateTime), minute(.dateTime)                   ), tz = .helpers[["timezone"]]))
byFasttimeYmdHMScall <- quote(fasttime::fastPOSIXct(sprintf("%04d-%02d-%02d %02d:%02d:%02d", year(.dateTime),   month(.dateTime), mday(.dateTime), hour(.dateTime), minute(.dateTime), second(.dateTime)), tz = .helpers[["timezone"]]))

byFasttime______call <- quote(rep(fasttime::fastPOSIXct("2199-01-01", tz = .helpers[["timezone"]]), length(.dateTime)))
byFasttime_Q____call <- quote(fasttime::fastPOSIXct(sprintf("2199-%02d-01"         , quarter(.dateTime) * 3L - 2L), tz = .helpers[["timezone"]]))
byFasttime_m____call <- quote(fasttime::fastPOSIXct(sprintf("2199-%02d-01"         ,   month(.dateTime)          ), tz = .helpers[["timezone"]]))
byFasttime___H__call <- quote(fasttime::fastPOSIXct(sprintf("2199-01-01 %02d:00:00",    hour(.dateTime)          ), tz = .helpers[["timezone"]]))
byFasttime____M_call <- quote(fasttime::fastPOSIXct(sprintf("2199-01-01 00:%02d:00",  minute(.dateTime)          ), tz = .helpers[["timezone"]]))
byFasttime_____Scall <- quote(fasttime::fastPOSIXct(sprintf("2199-01-01 00:00:%02d",  second(.dateTime)          ), tz = .helpers[["timezone"]]))

byRcppCCTZY_____call <- quote(RcppCCTZ::parseDatetime(sprintf("%04d-01-01"                     , year(.dateTime)                                                                                                                                                  ), fmt = "%Y-%m-%d"             , tzstr = .helpers[["timezone"]]))
byRcppCCTZYQ____call <- quote(RcppCCTZ::parseDatetime(sprintf("%04d-%02d-01"                   , year(.dateTime), quarter(.dateTime) * 3L - 2L                                                                                                                    ), fmt = "%Y-%m-%d"             , tzstr = .helpers[["timezone"]]))
byRcppCCTZYm____call <- quote(RcppCCTZ::parseDatetime(sprintf("%04d-%02d-01"                   , year(.dateTime),   month(.dateTime)                                                                                                                              ), fmt = "%Y-%m-%d"             , tzstr = .helpers[["timezone"]]))
byRcppCCTZYmd___call <- quote(RcppCCTZ::parseDatetime(sprintf("%04d-%02d-%02d"                 , year(.dateTime),   month(.dateTime), mday(.dateTime)                                                                                                             ), fmt = "%Y-%m-%d"             , tzstr = .helpers[["timezone"]]))
byRcppCCTZYmdH__call <- quote(RcppCCTZ::parseDatetime(sprintf("%04d-%02d-%02d %02d:00:00%s"    , year(.dateTime),   month(.dateTime), mday(.dateTime), hour(.dateTime)                                      , format(.dateTime, "%z", tz = .helpers[["timezone"]])), fmt = "%Y-%m-%d %H:%M:%E1S%z", tzstr = .helpers[["timezone"]]))
byRcppCCTZYmdHM_call <- quote(RcppCCTZ::parseDatetime(sprintf("%04d-%02d-%02d %02d:%02d:00%s"  , year(.dateTime),   month(.dateTime), mday(.dateTime), hour(.dateTime), minute(.dateTime)                   , format(.dateTime, "%z", tz = .helpers[["timezone"]])), fmt = "%Y-%m-%d %H:%M:%E1S%z", tzstr = .helpers[["timezone"]]))
byRcppCCTZYmdHMScall <- quote(RcppCCTZ::parseDatetime(sprintf("%04d-%02d-%02d %02d:%02d:%02d%s", year(.dateTime),   month(.dateTime), mday(.dateTime), hour(.dateTime), minute(.dateTime), second(.dateTime), format(.dateTime, "%z", tz = .helpers[["timezone"]])), fmt = "%Y-%m-%d %H:%M:%E1S%z", tzstr = .helpers[["timezone"]]))

byRcppCCTZ_Q____call <- quote(RcppCCTZ::parseDatetime(sprintf("2199-%02d-01"         , quarter(.dateTime) * 3L - 2L), fmt = "%Y-%m-%d"           , tzstr = .helpers[["timezone"]]))
byRcppCCTZ_m____call <- quote(RcppCCTZ::parseDatetime(sprintf("2199-%02d-01"         ,   month(.dateTime)          ), fmt = "%Y-%m-%d"           , tzstr = .helpers[["timezone"]]))
byRcppCCTZ___H__call <- quote(RcppCCTZ::parseDatetime(sprintf("2199-01-01 %02d:00:00",    hour(.dateTime)          ), fmt = "%Y-%m-%d %H:%M:%E1S", tzstr = .helpers[["timezone"]]))
byRcppCCTZ____M_call <- quote(RcppCCTZ::parseDatetime(sprintf("2199-01-01 00:%02d:00",  minute(.dateTime)          ), fmt = "%Y-%m-%d %H:%M:%E1S", tzstr = .helpers[["timezone"]]))
byRcppCCTZ_____Scall <- quote(RcppCCTZ::parseDatetime(sprintf("2199-01-01 00:00:%02d",  second(.dateTime)          ), fmt = "%Y-%m-%d %H:%M:%E1S", tzstr = .helpers[["timezone"]]))

byY_____call <- quote(as.POSIXct(  trunc(.dateTime     , units = "years"                              ), tz = .helpers[["timezone"]]))
byYQ____call <- quote(as.POSIXct(sprintf("%04d-%02d-01", year(.dateTime), quarter(.dateTime) * 3L - 2L), tz = .helpers[["timezone"]]))
byYm____call <- quote(as.POSIXct(  trunc(.dateTime     , units = "months"                             ), tz = .helpers[["timezone"]]))
byYmd___call <- quote(as.POSIXct(  trunc(.dateTime     , units = "days"                               ), tz = .helpers[["timezone"]]))
byYmdH__call <- quote(as.POSIXct(  trunc(.dateTime     , units = "hours"                              ), tz = .helpers[["timezone"]]))
byYmdHM_call <- quote(as.POSIXct(  trunc(.dateTime     , units = "mins"                               ), tz = .helpers[["timezone"]]))
byYmdHMScall <- quote(as.POSIXct(  trunc(.dateTime     , units = "secs"                               ), tz = .helpers[["timezone"]]))

by______call <- quote(rep(as.POSIXct("2199-01-01", tz = .helpers[["timezone"]]), length(.dateTime)))
by_Q____call <- quote(as.POSIXct(sprintf("2199-%02d-01"         , quarter(.dateTime) * 3L - 2L), tz = .helpers[["timezone"]]))
by_m____call <- quote(as.POSIXct(sprintf("2199-%02d-01"         ,   month(.dateTime)          ), tz = .helpers[["timezone"]]))
by___H__call <- quote(as.POSIXct(sprintf("2199-01-01 %02d:00:00",    hour(.dateTime)          ), tz = .helpers[["timezone"]]))
by____M_call <- quote(as.POSIXct(sprintf("2199-01-01 00:%02d:00",  minute(.dateTime)          ), tz = .helpers[["timezone"]]))
by_____Scall <- quote(as.POSIXct(sprintf("2199-01-01 00:00:%02d",  second(.dateTime)          ), tz = .helpers[["timezone"]]))

# Calls, multiplier > 1L) ####
byFasttimeMultY_____call <- quote(fasttime::fastPOSIXct(sprintf("%04d-01-01"                   , year(.dateTime)                                                                                            %/% .helpers[["multiplier"]] * .helpers[["multiplier"]]     ), tz = .helpers[["timezone"]]))
byFasttimeMultYm____call <- quote(fasttime::fastPOSIXct(sprintf("%04d-%02d-01"                 , year(.dateTime), (month(.dateTime) - 1L)                                                                   %/% .helpers[["multiplier"]] * .helpers[["multiplier"]] + 1L), tz = .helpers[["timezone"]]))
byFasttimeMultYmdH__call <- quote(fasttime::fastPOSIXct(sprintf("%04d-%02d-%02d %02d:00:00"    , year(.dateTime),  month(.dateTime), mday(.dateTime), hour(.dateTime)                                       %/% .helpers[["multiplier"]] * .helpers[["multiplier"]]     ), tz = .helpers[["timezone"]]))
byFasttimeMultYmdHM_call <- quote(fasttime::fastPOSIXct(sprintf("%04d-%02d-%02d %02d:%02d:00"  , year(.dateTime),  month(.dateTime), mday(.dateTime), hour(.dateTime), minute(.dateTime)                    %/% .helpers[["multiplier"]] * .helpers[["multiplier"]]     ), tz = .helpers[["timezone"]]))
byFasttimeMultYmdHMScall <- quote(fasttime::fastPOSIXct(sprintf("%04d-%02d-%02d %02d:%02d:%02d", year(.dateTime),  month(.dateTime), mday(.dateTime), hour(.dateTime), minute(.dateTime), second(.dateTime) %/% .helpers[["multiplier"]] * .helpers[["multiplier"]]     ), tz = .helpers[["timezone"]]))

byFasttimeMult_m____call <- quote(fasttime::fastPOSIXct(sprintf("2199-%02d-01"         , (month(.dateTime) - 1L) %/% .helpers[["multiplier"]] * .helpers[["multiplier"]] + 1L), tz = .helpers[["timezone"]]))
byFasttimeMult___H__call <- quote(fasttime::fastPOSIXct(sprintf("2199-01-01 %02d:00:00",   hour(.dateTime)       %/% .helpers[["multiplier"]] * .helpers[["multiplier"]]     ), tz = .helpers[["timezone"]]))
byFasttimeMult____M_call <- quote(fasttime::fastPOSIXct(sprintf("2199-01-01 00:%02d:00", minute(.dateTime)       %/% .helpers[["multiplier"]] * .helpers[["multiplier"]]     ), tz = .helpers[["timezone"]]))
byFasttimeMult_____Scall <- quote(fasttime::fastPOSIXct(sprintf("2199-01-01 00:00:%02d", second(.dateTime)       %/% .helpers[["multiplier"]] * .helpers[["multiplier"]]     ), tz = .helpers[["timezone"]]))

byRcppCCTZMultY_____call <- quote(RcppCCTZ::parseDatetime(sprintf("%04d-01-01"                     , year(.dateTime)                                                                                            %/% .helpers[["multiplier"]] * .helpers[["multiplier"]]                                                      ), fmt = "%Y-%m-%d"             , tzstr = .helpers[["timezone"]]))
byRcppCCTZMultYm____call <- quote(RcppCCTZ::parseDatetime(sprintf("%04d-%02d-01"                   , year(.dateTime), (month(.dateTime) - 1L)                                                                   %/% .helpers[["multiplier"]] * .helpers[["multiplier"]] + 1L                                                 ), fmt = "%Y-%m-%d"             , tzstr = .helpers[["timezone"]]))
byRcppCCTZMultYmdH__call <- quote(RcppCCTZ::parseDatetime(sprintf("%04d-%02d-%02d %02d:00:00"      , year(.dateTime),  month(.dateTime), mday(.dateTime), hour(.dateTime)                                       %/% .helpers[["multiplier"]] * .helpers[["multiplier"]]                                                      ), fmt = "%Y-%m-%d %H:%M:%E1S"  , tzstr = .helpers[["timezone"]]))
byRcppCCTZMultYmdHM_call <- quote(RcppCCTZ::parseDatetime(sprintf("%04d-%02d-%02d %02d:%02d:00%s"  , year(.dateTime),  month(.dateTime), mday(.dateTime), hour(.dateTime), minute(.dateTime)                    %/% .helpers[["multiplier"]] * .helpers[["multiplier"]], format(.dateTime, "%z", tz = .helpers[["timezone"]])), fmt = "%Y-%m-%d %H:%M:%E1S%z", tzstr = .helpers[["timezone"]]))
byRcppCCTZMultYmdHMScall <- quote(RcppCCTZ::parseDatetime(sprintf("%04d-%02d-%02d %02d:%02d:%02d%s", year(.dateTime),  month(.dateTime), mday(.dateTime), hour(.dateTime), minute(.dateTime), second(.dateTime) %/% .helpers[["multiplier"]] * .helpers[["multiplier"]], format(.dateTime, "%z", tz = .helpers[["timezone"]])), fmt = "%Y-%m-%d %H:%M:%E1S%z", tzstr = .helpers[["timezone"]]))

byRcppCCTZMult_m____call <- quote(RcppCCTZ::parseDatetime(sprintf("2199-%02d-01"         , (month(.dateTime) - 1L) %/% .helpers[["multiplier"]] * .helpers[["multiplier"]] + 1L), fmt = "%Y-%m-%d"           , tzstr = .helpers[["timezone"]]))
byRcppCCTZMult___H__call <- quote(RcppCCTZ::parseDatetime(sprintf("2199-01-01 %02d:00:00",   hour(.dateTime)       %/% .helpers[["multiplier"]] * .helpers[["multiplier"]]     ), fmt = "%Y-%m-%d %H:%M:%E1S", tzstr = .helpers[["timezone"]]))
byRcppCCTZMult____M_call <- quote(RcppCCTZ::parseDatetime(sprintf("2199-01-01 00:%02d:00", minute(.dateTime)       %/% .helpers[["multiplier"]] * .helpers[["multiplier"]]     ), fmt = "%Y-%m-%d %H:%M:%E1S", tzstr = .helpers[["timezone"]]))
byRcppCCTZMult_____Scall <- quote(RcppCCTZ::parseDatetime(sprintf("2199-01-01 00:00:%02d", second(.dateTime)       %/% .helpers[["multiplier"]] * .helpers[["multiplier"]]     ), fmt = "%Y-%m-%d %H:%M:%E1S", tzstr = .helpers[["timezone"]]))

byMultY_____call <- quote(as.POSIXct(sprintf("%04d-01-01"                     , year(.dateTime)                                                                                            %/% .helpers[["multiplier"]] * .helpers[["multiplier"]]                                                      ), tz = .helpers[["timezone"]]                                ))
byMultYm____call <- quote(as.POSIXct(sprintf("%04d-%02d-01"                   , year(.dateTime), (month(.dateTime) - 1L)                                                                   %/% .helpers[["multiplier"]] * .helpers[["multiplier"]] + 1L                                                 ), tz = .helpers[["timezone"]]                                ))
byMultYmdH__call <- quote(as.POSIXct(sprintf("%04d-%02d-%02d %02d:00:00"      , year(.dateTime),  month(.dateTime), mday(.dateTime), hour(.dateTime)                                       %/% .helpers[["multiplier"]] * .helpers[["multiplier"]]                                                      ), tz = .helpers[["timezone"]]                                ))
byMultYmdHM_call <- quote(as.POSIXct(sprintf("%04d-%02d-%02d %02d:%02d:00%s"  , year(.dateTime),  month(.dateTime), mday(.dateTime), hour(.dateTime), minute(.dateTime)                    %/% .helpers[["multiplier"]] * .helpers[["multiplier"]], format(.dateTime, "%z", tz = .helpers[["timezone"]])), tz = .helpers[["timezone"]], format = "%Y-%m-%d %H:%M:%S%z"))
byMultYmdHMScall <- quote(as.POSIXct(sprintf("%04d-%02d-%02d %02d:%02d:%02d%s", year(.dateTime),  month(.dateTime), mday(.dateTime), hour(.dateTime), minute(.dateTime), second(.dateTime) %/% .helpers[["multiplier"]] * .helpers[["multiplier"]], format(.dateTime, "%z", tz = .helpers[["timezone"]])), tz = .helpers[["timezone"]], format = "%Y-%m-%d %H:%M:%S%z"))

byMult_m____call <- quote(as.POSIXct(sprintf("2199-%02d-01"         , (month(.dateTime) - 1L) %/% .helpers[["multiplier"]] * .helpers[["multiplier"]] + 1L), tz = .helpers[["timezone"]]))
byMult___H__call <- quote(as.POSIXct(sprintf("2199-01-01 %02d:00:00",   hour(.dateTime)       %/% .helpers[["multiplier"]] * .helpers[["multiplier"]]     ), tz = .helpers[["timezone"]]))
byMult____M_call <- quote(as.POSIXct(sprintf("2199-01-01 00:%02d:00", minute(.dateTime)       %/% .helpers[["multiplier"]] * .helpers[["multiplier"]]     ), tz = .helpers[["timezone"]]))
byMult_____Scall <- quote(as.POSIXct(sprintf("2199-01-01 00:00:%02d", second(.dateTime)       %/% .helpers[["multiplier"]] * .helpers[["multiplier"]]     ), tz = .helpers[["timezone"]]))

# Functions ####
to.fakeUTCdateTime <- function(.dateTime, .helpers) {
  assertNAstatusPeriodicityOK(
    .helpers[["na.status"]],
    .helpers[["periodicity"]],
    level = "error"
  )

  from <- .dateTime[1L]

  if (as.POSIXlt(from)$isdst) {
    from <- from - 3600L
  }

  .dateTime <- seq(
    as.POSIXct(as.character(from, tz = .helpers[["timezone"]]), tz = "UTC"),
    by = .helpers[["periodicity"]],
    along.with = .dateTime
  )

  if (grepl("^\\d+ (month|year)(s?)$", .helpers[["periodicity"]]) &&
        mday(.dateTime[1L]) > 28L) {
    .dateTime <- rollback(.dateTime, .helpers[["periodicity"]])
  }

  .dateTime
}

#' Temporal Aggregation Level Functions (TALFs)
#'
#' Simply hand over one of these functions to the `funby` argument of one of the
#' methods of a [`DTSg`] object, which supports it. The method then does the
#' rest of the work. See respective calling method for further information.
#' Other uses are possible, but not recommended.
#'
#' @param .dateTime A [`POSIXct`] vector.
#' @param .helpers A [`list`] with helper data as handed over by methods of
#'   [`DTSg`] objects, which support the `funby` argument.
#'
#' @section Families and flavours:
#' There are two families of temporal aggregation level functions. The one
#' family truncates timestamps (truncating family), the other extracts a certain
#' part of them (extracting family). Each family comes in three flavours: the
#' first relies solely on base \R, the second utilises [`fasttime::fastPOSIXct`]
#' of \pkg{fasttime} and the third [`RcppCCTZ::parseDatetime`] of
#' \pkg{RcppCCTZ}.
#'
#' The \pkg{fasttime} flavour works with UTC and equivalent as well as all
#' Etc/GMT time zones only (execute
#' `grep("^(Etc/)?(UCT|UTC)$|^(Etc/)?GMT(\\+|-)?0?$", OlsonNames(), ignore.case
#' = TRUE, value = TRUE)` for a full list of supported time zones) and is
#' limited to timestamps between the years 1970 and 2199, but generally is the
#' fastest for the extracting family of functions. For time zones other than UTC
#' and equivalent the \pkg{RcppCCTZ} flavour generally is the fastest.
#'
#' Use the `funbyApproach` argument of the respective calling method in order to
#' specify the utilised flavour.
#'
#' The truncating family sets timestamps to the lowest possible point in time of
#' the corresponding temporal aggregation level:
#' - `byY_____` truncates to year,    e.g. _2000-11-11 11:11:11.1_ becomes _2000-01-01 00:00:00.0_
#' - `byYQ____` truncates to quarter, e.g. _2000-11-11 11:11:11.1_ becomes _2000-10-01 00:00:00.0_
#' - `byYm____` truncates to month,   e.g. _2000-11-11 11:11:11.1_ becomes _2000-11-01 00:00:00.0_
#' - `byYmd___` truncates to day,     e.g. _2000-11-11 11:11:11.1_ becomes _2000-11-11 00:00:00.0_
#' - `byYmdH__` truncates to hour,    e.g. _2000-11-11 11:11:11.1_ becomes _2000-11-11 11:00:00.0_
#' - `byYmdHM_` truncates to minute,  e.g. _2000-11-11 11:11:11.1_ becomes _2000-11-11 11:11:00.0_
#' - `byYmdHMS` truncates to second,  e.g. _2000-11-11 11:11:11.1_ becomes _2000-11-11 11:11:11.0_
#'
#' By convention, the extracting family sets the year to 2199 and extracts a
#' certain part of timestamps:
#' - `by______` extracts nothing,      i.e.          all timestamps become  _2199-01-01 00:00:00.0_
#' - `by_Q____` extracts the quarters, e.g. _2000-11-11 11:11:11.1_ becomes _2199-10-01 00:00:00.0_
#' - `by_m____` extracts the months,   e.g. _2000-11-11 11:11:11.1_ becomes _2199-11-01 00:00:00.0_
#' - `by___H__` extracts the hours,    e.g. _2000-11-11 11:11:11.1_ becomes _2199-01-01 11:00:00.0_
#' - `by____M_` extracts the minutes,  e.g. _2000-11-11 11:11:11.1_ becomes _2199-01-01 00:11:00.0_
#' - `by_____S` extracts the seconds,  e.g. _2000-11-11 11:11:11.1_ becomes _2199-01-01 00:00:11.0_
#'
#' Please note that the `byFasttime*` versions are deprecated.
#'
#' @return All functions return a [`POSIXct`] vector with timestamps
#'   corresponding to the function's temporal aggregation level.
#'
#' @seealso [`aggregate`], [`colapply`], [`subset`]
#'
#' @name TALFs
NULL

## Truncating family ####
#' @rdname TALFs
#' @export
byY_____ <- function(.dateTime, .helpers) {
  assertFunbyApproach(.helpers[["funbyApproach"]])
  if (.helpers[["funbyApproach"]] == "fasttime") {
    assertFasttimeOK(.dateTime, .helpers)
  }

  if (.helpers[["ignoreDST"]] && !grepl(
    "^(Etc/)?(UCT|UTC)$|^(Etc/)?GMT(\\+|-)?0?$",
    .helpers[["timezone"]],
    ignore.case = TRUE
  )) {
    .dateTime <- to.fakeUTCdateTime(.dateTime, .helpers)
  }

  if (.helpers[["multiplier"]] == 1L) {
    switch(
      .helpers[["funbyApproach"]],
      base     = eval(        byY_____call),
      fasttime = eval(byFasttimeY_____call),
      RcppCCTZ = eval(byRcppCCTZY_____call)
    )
  } else {
    switch(
      .helpers[["funbyApproach"]],
      base     = eval(        byMultY_____call),
      fasttime = eval(byFasttimeMultY_____call),
      RcppCCTZ = eval(byRcppCCTZMultY_____call)
    )
  }
}

#' @rdname TALFs
#' @export
byYQ____ <- function(.dateTime, .helpers) {
  assertFunbyApproach(.helpers[["funbyApproach"]])
  if (.helpers[["funbyApproach"]] == "fasttime") {
    assertFasttimeOK(.dateTime, .helpers)
  }

  if (.helpers[["ignoreDST"]] && !grepl(
    "^(Etc/)?(UCT|UTC)$|^(Etc/)?GMT(\\+|-)?0?$",
    .helpers[["timezone"]],
    ignore.case = TRUE
  )) {
    .dateTime <- to.fakeUTCdateTime(.dateTime, .helpers)
  }

  switch(
    .helpers[["funbyApproach"]],
    base     = eval(        byYQ____call),
    fasttime = eval(byFasttimeYQ____call),
    RcppCCTZ = eval(byRcppCCTZYQ____call)
  )
}

#' @rdname TALFs
#' @export
byYm____ <- function(.dateTime, .helpers) {
  assertFunbyApproach(.helpers[["funbyApproach"]])
  if (.helpers[["funbyApproach"]] == "fasttime") {
    assertFasttimeOK(.dateTime, .helpers)
  }

  if (.helpers[["ignoreDST"]] && !grepl(
    "^(Etc/)?(UCT|UTC)$|^(Etc/)?GMT(\\+|-)?0?$",
    .helpers[["timezone"]],
    ignore.case = TRUE
  )) {
    .dateTime <- to.fakeUTCdateTime(.dateTime, .helpers)
  }

  if (.helpers[["multiplier"]] == 1L) {
    switch(
      .helpers[["funbyApproach"]],
      base     = eval(        byYm____call),
      fasttime = eval(byFasttimeYm____call),
      RcppCCTZ = eval(byRcppCCTZYm____call)
    )
  } else {
    switch(
      .helpers[["funbyApproach"]],
      base     = eval(        byMultYm____call),
      fasttime = eval(byFasttimeMultYm____call),
      RcppCCTZ = eval(byRcppCCTZMultYm____call)
    )
  }
}

#' @rdname TALFs
#' @export
byYmd___ <- function(.dateTime, .helpers) {
  assertFunbyApproach(.helpers[["funbyApproach"]])
  if (.helpers[["funbyApproach"]] == "fasttime") {
    assertFasttimeOK(.dateTime, .helpers)
  }

  if (.helpers[["ignoreDST"]] && !grepl(
    "^(Etc/)?(UCT|UTC)$|^(Etc/)?GMT(\\+|-)?0?$",
    .helpers[["timezone"]],
    ignore.case = TRUE
  )) {
    .dateTime <- to.fakeUTCdateTime(.dateTime, .helpers)
  }

  switch(
    .helpers[["funbyApproach"]],
    base     = eval(        byYmd___call),
    fasttime = eval(byFasttimeYmd___call),
    RcppCCTZ = eval(byRcppCCTZYmd___call)
  )
}

#' @rdname TALFs
#' @export
byYmdH__ <- function(.dateTime, .helpers) {
  assertFunbyApproach(.helpers[["funbyApproach"]])
  if (.helpers[["funbyApproach"]] == "fasttime") {
    assertFasttimeOK(.dateTime, .helpers)
  }

  if (.helpers[["multiplier"]] == 1L) {
    switch(
      .helpers[["funbyApproach"]],
      base     = eval(        byYmdH__call),
      fasttime = eval(byFasttimeYmdH__call),
      RcppCCTZ = eval(byRcppCCTZYmdH__call)
    )
  } else if (!grepl(
    "^(Etc/)?(UCT|UTC)$|^(Etc/)?GMT",
    .helpers[["timezone"]],
    ignore.case = TRUE
  )) {
    stop(
      'Time zone must be "UTC" or equivalent or any Etc/GMT for this TALF ',
      "with a multiplier greater than one.",
      call. = FALSE
    )
  } else {
    switch(
      .helpers[["funbyApproach"]],
      base     = eval(        byMultYmdH__call),
      fasttime = eval(byFasttimeMultYmdH__call),
      RcppCCTZ = eval(byRcppCCTZMultYmdH__call)
    )
  }
}

#' @rdname TALFs
#' @export
byYmdHM_ <- function(.dateTime, .helpers) {
  assertFunbyApproach(.helpers[["funbyApproach"]])
  if (.helpers[["funbyApproach"]] == "fasttime") {
    assertFasttimeOK(.dateTime, .helpers)
  }

  if (.helpers[["multiplier"]] == 1L) {
    switch(
      .helpers[["funbyApproach"]],
      base     = eval(        byYmdHM_call),
      fasttime = eval(byFasttimeYmdHM_call),
      RcppCCTZ = eval(byRcppCCTZYmdHM_call)
    )
  } else {
    switch(
      .helpers[["funbyApproach"]],
      base     = eval(        byMultYmdHM_call),
      fasttime = eval(byFasttimeMultYmdHM_call),
      RcppCCTZ = eval(byRcppCCTZMultYmdHM_call)
    )
  }
}

#' @rdname TALFs
#' @export
byYmdHMS <- function(.dateTime, .helpers) {
  assertFunbyApproach(.helpers[["funbyApproach"]])
  if (.helpers[["funbyApproach"]] == "fasttime") {
    assertFasttimeOK(.dateTime, .helpers)
  }

  if (.helpers[["multiplier"]] == 1L) {
    switch(
      .helpers[["funbyApproach"]],
      base     = eval(        byYmdHMScall),
      fasttime = eval(byFasttimeYmdHMScall),
      RcppCCTZ = eval(byRcppCCTZYmdHMScall)
    )
  } else {
    switch(
      .helpers[["funbyApproach"]],
      base     = eval(        byMultYmdHMScall),
      fasttime = eval(byFasttimeMultYmdHMScall),
      RcppCCTZ = eval(byRcppCCTZMultYmdHMScall)
    )
  }
}

## Extracting family ####
#' @rdname TALFs
#' @export
by______ <- function(.dateTime, .helpers) {
  eval(by______call)
}

#' @rdname TALFs
#' @export
by_Q____ <- function(.dateTime, .helpers) {
  assertFunbyApproach(.helpers[["funbyApproach"]])
  if (.helpers[["funbyApproach"]] == "fasttime") {
    assertFasttimeOK(.dateTime, .helpers)
  }

  if (.helpers[["ignoreDST"]] && !grepl(
    "^(Etc/)?(UCT|UTC)$|^(Etc/)?GMT(\\+|-)?0?$",
    .helpers[["timezone"]],
    ignore.case = TRUE
  )) {
    .dateTime <- to.fakeUTCdateTime(.dateTime, .helpers)
  }

  switch(
    .helpers[["funbyApproach"]],
    base     = eval(        by_Q____call),
    fasttime = eval(byFasttime_Q____call),
    RcppCCTZ = eval(byRcppCCTZ_Q____call)
  )
}

#' @rdname TALFs
#' @export
by_m____ <- function(.dateTime, .helpers) {
  assertFunbyApproach(.helpers[["funbyApproach"]])
  if (.helpers[["funbyApproach"]] == "fasttime") {
    assertFasttimeOK(.dateTime, .helpers)
  }

  if (.helpers[["ignoreDST"]] && !grepl(
    "^(Etc/)?(UCT|UTC)$|^(Etc/)?GMT(\\+|-)?0?$",
    .helpers[["timezone"]],
    ignore.case = TRUE
  )) {
    .dateTime <- to.fakeUTCdateTime(.dateTime, .helpers)
  }

  if (.helpers[["multiplier"]] == 1L) {
    switch(
      .helpers[["funbyApproach"]],
      base     = eval(        by_m____call),
      fasttime = eval(byFasttime_m____call),
      RcppCCTZ = eval(byRcppCCTZ_m____call)
    )
  } else {
    switch(
      .helpers[["funbyApproach"]],
      base     = eval(        byMult_m____call),
      fasttime = eval(byFasttimeMult_m____call),
      RcppCCTZ = eval(byRcppCCTZMult_m____call)
    )
  }
}

#' @rdname TALFs
#' @export
by___H__ <- function(.dateTime, .helpers) {
  assertFunbyApproach(.helpers[["funbyApproach"]])
  if (.helpers[["funbyApproach"]] == "fasttime") {
    assertFasttimeOK(.dateTime, .helpers)
  }

  if (.helpers[["ignoreDST"]] && !grepl(
    "^(Etc/)?(UCT|UTC)$|^(Etc/)?GMT(\\+|-)?0?$",
    .helpers[["timezone"]],
    ignore.case = TRUE
  )) {
    .dateTime <- to.fakeUTCdateTime(.dateTime, .helpers)
  }

  if (.helpers[["multiplier"]] == 1L) {
    switch(
      .helpers[["funbyApproach"]],
      base     = eval(        by___H__call),
      fasttime = eval(byFasttime___H__call),
      RcppCCTZ = eval(byRcppCCTZ___H__call)
    )
  } else if (!grepl(
    "^(Etc/)?(UCT|UTC)$|^(Etc/)?GMT",
    .helpers[["timezone"]],
    ignore.case = TRUE
  )) {
    stop(
      'Time zone must be "UTC" or equivalent or any Etc/GMT for this TALF ',
      "with a multiplier greater than one.",
      call. = FALSE
    )
  } else {
    switch(
      .helpers[["funbyApproach"]],
      base     = eval(        byMult___H__call),
      fasttime = eval(byFasttimeMult___H__call),
      RcppCCTZ = eval(byRcppCCTZMult___H__call)
    )
  }
}

#' @rdname TALFs
#' @export
by____M_ <- function(.dateTime, .helpers) {
  assertFunbyApproach(.helpers[["funbyApproach"]])
  if (.helpers[["funbyApproach"]] == "fasttime") {
    assertFasttimeOK(.dateTime, .helpers)
  }

  if (.helpers[["multiplier"]] == 1L) {
    switch(
      .helpers[["funbyApproach"]],
      base     = eval(        by____M_call),
      fasttime = eval(byFasttime____M_call),
      RcppCCTZ = eval(byRcppCCTZ____M_call)
    )
  } else {
    switch(
      .helpers[["funbyApproach"]],
      base     = eval(        byMult____M_call),
      fasttime = eval(byFasttimeMult____M_call),
      RcppCCTZ = eval(byRcppCCTZMult____M_call)
    )
  }
}

#' @rdname TALFs
#' @export
by_____S <- function(.dateTime, .helpers) {
  assertFunbyApproach(.helpers[["funbyApproach"]])
  if (.helpers[["funbyApproach"]] == "fasttime") {
    assertFasttimeOK(.dateTime, .helpers)
  }

  if (.helpers[["multiplier"]] == 1L) {
    switch(
      .helpers[["funbyApproach"]],
      base     = eval(        by_____Scall),
      fasttime = eval(byFasttime_____Scall),
      RcppCCTZ = eval(byRcppCCTZ_____Scall)
    )
  } else {
    switch(
      .helpers[["funbyApproach"]],
      base     = eval(        byMult_____Scall),
      fasttime = eval(byFasttimeMult_____Scall),
      RcppCCTZ = eval(byRcppCCTZMult_____Scall)
    )
  }
}
