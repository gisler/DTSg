#### Calls (multiplier == 1L) #### # nolint
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

byY_____call <- quote(as.POSIXct(sprintf("%04d-01-01"  , year(.dateTime)                              ), tz = .helpers[["timezone"]]))
byYQ____call <- quote(as.POSIXct(sprintf("%04d-%02d-01", year(.dateTime), quarter(.dateTime) * 3L - 2L), tz = .helpers[["timezone"]]))
byYm____call <- quote(as.POSIXct(sprintf("%04d-%02d-01", year(.dateTime),   month(.dateTime)          ), tz = .helpers[["timezone"]]))
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

#### Calls (multiplier > 1L) #### # nolint
byFasttimeMultY_____call <- quote(fasttime::fastPOSIXct(sprintf("%04d-01-01"                   , year(.dateTime)                                                                                            %/% .helpers[["multiplier"]] * .helpers[["multiplier"]]     ), tz = .helpers[["timezone"]]))
byFasttimeMultYm____call <- quote(fasttime::fastPOSIXct(sprintf("%04d-%02d-01"                 , year(.dateTime), (month(.dateTime)                                                                   - 1L) %/% .helpers[["multiplier"]] * .helpers[["multiplier"]] + 1L), tz = .helpers[["timezone"]]))
byFasttimeMultYmdH__call <- quote(fasttime::fastPOSIXct(sprintf("%04d-%02d-%02d %02d:00:00"    , year(.dateTime),  month(.dateTime), mday(.dateTime), hour(.dateTime)                                       %/% .helpers[["multiplier"]] * .helpers[["multiplier"]]     ), tz = .helpers[["timezone"]]))
byFasttimeMultYmdHM_call <- quote(fasttime::fastPOSIXct(sprintf("%04d-%02d-%02d %02d:%02d:00"  , year(.dateTime),  month(.dateTime), mday(.dateTime), hour(.dateTime), minute(.dateTime)                    %/% .helpers[["multiplier"]] * .helpers[["multiplier"]]     ), tz = .helpers[["timezone"]]))
byFasttimeMultYmdHMScall <- quote(fasttime::fastPOSIXct(sprintf("%04d-%02d-%02d %02d:%02d:%02d", year(.dateTime),  month(.dateTime), mday(.dateTime), hour(.dateTime), minute(.dateTime), second(.dateTime) %/% .helpers[["multiplier"]] * .helpers[["multiplier"]]     ), tz = .helpers[["timezone"]]))

byFasttimeMult_m____call <- quote(fasttime::fastPOSIXct(sprintf("2199-%02d-01"         , (month(.dateTime) - 1L) %/% .helpers[["multiplier"]] * .helpers[["multiplier"]] + 1L), tz = .helpers[["timezone"]]))
byFasttimeMult___H__call <- quote(fasttime::fastPOSIXct(sprintf("2199-01-01 %02d:00:00",   hour(.dateTime)       %/% .helpers[["multiplier"]] * .helpers[["multiplier"]]     ), tz = .helpers[["timezone"]]))
byFasttimeMult____M_call <- quote(fasttime::fastPOSIXct(sprintf("2199-01-01 00:%02d:00", minute(.dateTime)       %/% .helpers[["multiplier"]] * .helpers[["multiplier"]]     ), tz = .helpers[["timezone"]]))
byFasttimeMult_____Scall <- quote(fasttime::fastPOSIXct(sprintf("2199-01-01 00:00:%02d", second(.dateTime)       %/% .helpers[["multiplier"]] * .helpers[["multiplier"]]     ), tz = .helpers[["timezone"]]))

byMultY_____call <- quote(as.POSIXct(sprintf("%04d-01-01"                      , year(.dateTime)                                                                                            %/% .helpers[["multiplier"]] * .helpers[["multiplier"]]                                                      ), tz = .helpers[["timezone"]]                                 ))
byMultYm____call <- quote(as.POSIXct(sprintf("%04d-%02d-01"                    , year(.dateTime), (month(.dateTime)                                                                   - 1L) %/% .helpers[["multiplier"]] * .helpers[["multiplier"]]                                                  + 1L), tz = .helpers[["timezone"]]                                 ))
byMultYmdH__call <- quote(as.POSIXct(sprintf("%04d-%02d-%02d %02d:00:00"       , year(.dateTime),  month(.dateTime), mday(.dateTime), hour(.dateTime)                                       %/% .helpers[["multiplier"]] * .helpers[["multiplier"]]                                                      ), tz = .helpers[["timezone"]]                                 ))
byMultYmdHM_call <- quote(as.POSIXct(sprintf("%04d-%02d-%02d %02d:%02d:00 %s"  , year(.dateTime),  month(.dateTime), mday(.dateTime), hour(.dateTime), minute(.dateTime)                    %/% .helpers[["multiplier"]] * .helpers[["multiplier"]], format(.dateTime, "%z", tz = .helpers[["timezone"]])), tz = .helpers[["timezone"]], format = "%Y-%m-%d %H:%M:%S %z"))
byMultYmdHMScall <- quote(as.POSIXct(sprintf("%04d-%02d-%02d %02d:%02d:%02d %s", year(.dateTime),  month(.dateTime), mday(.dateTime), hour(.dateTime), minute(.dateTime), second(.dateTime) %/% .helpers[["multiplier"]] * .helpers[["multiplier"]], format(.dateTime, "%z", tz = .helpers[["timezone"]])), tz = .helpers[["timezone"]], format = "%Y-%m-%d %H:%M:%S %z"))

byMult_m____call <- quote(as.POSIXct(sprintf("2199-%02d-01"         , (month(.dateTime) - 1L) %/% .helpers[["multiplier"]] * .helpers[["multiplier"]] + 1L), tz = .helpers[["timezone"]]))
byMult___H__call <- quote(as.POSIXct(sprintf("2199-01-01 %02d:00:00",   hour(.dateTime)       %/% .helpers[["multiplier"]] * .helpers[["multiplier"]]     ), tz = .helpers[["timezone"]]))
byMult____M_call <- quote(as.POSIXct(sprintf("2199-01-01 00:%02d:00", minute(.dateTime)       %/% .helpers[["multiplier"]] * .helpers[["multiplier"]]     ), tz = .helpers[["timezone"]]))
byMult_____Scall <- quote(as.POSIXct(sprintf("2199-01-01 00:00:%02d", second(.dateTime)       %/% .helpers[["multiplier"]] * .helpers[["multiplier"]]     ), tz = .helpers[["timezone"]]))

#### Functions ####
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
#' methods of [`DTSg`] objects, which support it. The method then does the rest
#' of the work. See corresponding section and the respective method for further
#' information. Other uses are possible, but not recommended.
#'
#' @param .dateTime A [`POSIXct`] vector.
#' @param .helpers A [`list`] with helper data as handed over by methods of
#'   [`DTSg`] objects, which support the `funby` argument.
#'
#' @section Families:
#' There are two families of temporal aggregation level functions. The one
#' family truncates timestamps (truncating family), the other extracts a certain
#' part of them (extracting family). Each family comes in two flavours: one
#' using [`fasttime::fastPOSIXct`] of \pkg{fasttime}, the other solely relying
#' on base \R. The \pkg{fasttime} versions work with UTC and equivalent as well
#' as all Etc/GMT time zones only (execute
#' `grep("^(Etc/)?(UCT|UTC)$|^(Etc/)?GMT(\\+|-)?0?$", OlsonNames(), ignore.case
#' = TRUE, value = TRUE)` for a full list of supported time zones) and are
#' limited to timestamps between the years 1970 and 2199, but generally are
#' faster for the extracting family of functions.
#'
#' The truncating family sets timestamps to the lowest possible point in time of
#' the corresponding temporal aggregation level:
#' - `*Y_____` truncates to year,    e.g. _2000-11-11 11:11:11.1_ becomes _2000-01-01 00:00:00.0_
#' - `*YQ____` truncates to quarter, e.g. _2000-11-11 11:11:11.1_ becomes _2000-10-01 00:00:00.0_
#' - `*Ym____` truncates to month,   e.g. _2000-11-11 11:11:11.1_ becomes _2000-11-01 00:00:00.0_
#' - `*Ymd___` truncates to day,     e.g. _2000-11-11 11:11:11.1_ becomes _2000-11-11 00:00:00.0_
#' - `*YmdH__` truncates to hour,    e.g. _2000-11-11 11:11:11.1_ becomes _2000-11-11 11:00:00.0_
#' - `*YmdHM_` truncates to minute,  e.g. _2000-11-11 11:11:11.1_ becomes _2000-11-11 11:11:00.0_
#' - `*YmdHMS` truncates to second,  e.g. _2000-11-11 11:11:11.1_ becomes _2000-11-11 11:11:11.0_
#'
#' By convention, the extracting family sets the year to 2199 and extracts a
#' certain part of timestamps:
#' - `*______` extracts nothing,      i.e.          all timestamps become  _2199-01-01 00:00:00.0_
#' - `*_Q____` extracts the quarters, e.g. _2000-11-11 11:11:11.1_ becomes _2199-10-01 00:00:00.0_
#' - `*_m____` extracts the months,   e.g. _2000-11-11 11:11:11.1_ becomes _2199-11-01 00:00:00.0_
#' - `*___H__` extracts the hours,    e.g. _2000-11-11 11:11:11.1_ becomes _2199-01-01 11:00:00.0_
#' - `*____M_` extracts the minutes,  e.g. _2000-11-11 11:11:11.1_ becomes _2199-01-01 00:11:00.0_
#' - `*_____S` extracts the seconds,  e.g. _2000-11-11 11:11:11.1_ becomes _2199-01-01 00:00:11.0_
#'
#' @return All functions return a [`POSIXct`] vector with timestamps
#'   corresponding to the function's temporal aggregation level.
#'
#' @seealso [`aggregate`], [`colapply`], [`subset`]
#'
#' @name TALFs
NULL

#' @rdname TALFs
#' @export
byFasttimeY_____ <- function(.dateTime, .helpers) {
  assertFasttimeOK(.dateTime, .helpers)
  if (.helpers[["multiplier"]] == 1L) {
    eval(byFasttimeY_____call)
  } else {
    eval(byFasttimeMultY_____call)
  }
}
#' @rdname TALFs
#' @export
byFasttimeYQ____ <- function(.dateTime, .helpers) {
  assertFasttimeOK(.dateTime, .helpers)
  eval(byFasttimeYQ____call)
}
#' @rdname TALFs
#' @export
byFasttimeYm____ <- function(.dateTime, .helpers) {
  assertFasttimeOK(.dateTime, .helpers)
  if (.helpers[["multiplier"]] == 1L) {
    eval(byFasttimeYm____call)
  } else {
    eval(byFasttimeMultYm____call)
  }
}
#' @rdname TALFs
#' @export
byFasttimeYmd___ <- function(.dateTime, .helpers) {
  assertFasttimeOK(.dateTime, .helpers)
  eval(byFasttimeYmd___call)
}
#' @rdname TALFs
#' @export
byFasttimeYmdH__ <- function(.dateTime, .helpers) {
  assertFasttimeOK(.dateTime, .helpers)
  if (.helpers[["multiplier"]] == 1L) {
    eval(byFasttimeYmdH__call)
  } else {
    eval(byFasttimeMultYmdH__call)
  }
}
#' @rdname TALFs
#' @export
byFasttimeYmdHM_ <- function(.dateTime, .helpers) {
  assertFasttimeOK(.dateTime, .helpers)
  if (.helpers[["multiplier"]] == 1L) {
    eval(byFasttimeYmdHM_call)
  } else {
    eval(byFasttimeMultYmdHM_call)
  }
}
#' @rdname TALFs
#' @export
byFasttimeYmdHMS <- function(.dateTime, .helpers) {
  assertFasttimeOK(.dateTime, .helpers)
  if (.helpers[["multiplier"]] == 1L) {
    eval(byFasttimeYmdHMScall)
  } else {
    eval(byFasttimeMultYmdHMScall)
  }
}

#' @rdname TALFs
#' @export
byFasttime______ <- function(.dateTime, .helpers) {
  assertFasttimeOK(.dateTime, .helpers)
  eval(byFasttime______call)
}
#' @rdname TALFs
#' @export
byFasttime_Q____ <- function(.dateTime, .helpers) {
  assertFasttimeOK(.dateTime, .helpers)
  eval(byFasttime_Q____call)
}
#' @rdname TALFs
#' @export
byFasttime_m____ <- function(.dateTime, .helpers) {
  assertFasttimeOK(.dateTime, .helpers)
  if (.helpers[["multiplier"]] == 1L) {
    eval(byFasttime_m____call)
  } else {
    eval(byFasttimeMult_m____call)
  }
}
#' @rdname TALFs
#' @export
byFasttime___H__ <- function(.dateTime, .helpers) {
  assertFasttimeOK(.dateTime, .helpers)
  if (.helpers[["multiplier"]] == 1L) {
    eval(byFasttime___H__call)
  } else {
    eval(byFasttimeMult___H__call)
  }
}
#' @rdname TALFs
#' @export
byFasttime____M_ <- function(.dateTime, .helpers) {
  assertFasttimeOK(.dateTime, .helpers)
  if (.helpers[["multiplier"]] == 1L) {
    eval(byFasttime____M_call)
  } else {
    eval(byFasttimeMult____M_call)
  }
}
#' @rdname TALFs
#' @export
byFasttime_____S <- function(.dateTime, .helpers) {
  assertFasttimeOK(.dateTime, .helpers)
  if (.helpers[["multiplier"]] == 1L) {
    eval(byFasttime_____Scall)
  } else {
    eval(byFasttimeMult_____Scall)
  }
}

#' @rdname TALFs
#' @export
byY_____ <- function(.dateTime, .helpers) {
  if (.helpers[["ignoreDST"]] && !grepl(
    "^(Etc/)?(UCT|UTC)$|^(Etc/)?GMT(\\+|-)?0?$",
    .helpers[["timezone"]],
    ignore.case = TRUE
  )) {
    .dateTime <- to.fakeUTCdateTime(.dateTime, .helpers)
  }
  if (.helpers[["multiplier"]] == 1L) {
    eval(byY_____call)
  } else {
    eval(byMultY_____call)
  }
}
#' @rdname TALFs
#' @export
byYQ____ <- function(.dateTime, .helpers) {
  if (.helpers[["ignoreDST"]] && !grepl(
    "^(Etc/)?(UCT|UTC)$|^(Etc/)?GMT(\\+|-)?0?$",
    .helpers[["timezone"]],
    ignore.case = TRUE
  )) {
    .dateTime <- to.fakeUTCdateTime(.dateTime, .helpers)
  }
  eval(byYQ____call)
}
#' @rdname TALFs
#' @export
byYm____ <- function(.dateTime, .helpers) {
  if (.helpers[["ignoreDST"]] && !grepl(
    "^(Etc/)?(UCT|UTC)$|^(Etc/)?GMT(\\+|-)?0?$",
    .helpers[["timezone"]],
    ignore.case = TRUE
  )) {
    .dateTime <- to.fakeUTCdateTime(.dateTime, .helpers)
  }
  if (.helpers[["multiplier"]] == 1L) {
    eval(byYm____call)
  } else {
    eval(byMultYm____call)
  }
}
#' @rdname TALFs
#' @export
byYmd___ <- function(.dateTime, .helpers) {
  if (.helpers[["ignoreDST"]] && !grepl(
    "^(Etc/)?(UCT|UTC)$|^(Etc/)?GMT(\\+|-)?0?$",
    .helpers[["timezone"]],
    ignore.case = TRUE
  )) {
    .dateTime <- to.fakeUTCdateTime(.dateTime, .helpers)
  }
  eval(byYmd___call)
}
#' @rdname TALFs
#' @export
byYmdH__ <- function(.dateTime, .helpers) {
  if (.helpers[["multiplier"]] == 1L) {
    eval(byYmdH__call)
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
    eval(byMultYmdH__call)
  }
}
#' @rdname TALFs
#' @export
byYmdHM_ <- function(.dateTime, .helpers) {
  if (.helpers[["multiplier"]] == 1L) {
    eval(byYmdHM_call)
  } else {
    eval(byMultYmdHM_call)
  }
}
#' @rdname TALFs
#' @export
byYmdHMS <- function(.dateTime, .helpers) {
  if (.helpers[["multiplier"]] == 1L) {
    eval(byYmdHMScall)
  } else {
    eval(byMultYmdHMScall)
  }
}

#' @rdname TALFs
#' @export
by______ <- function(.dateTime, .helpers) {
  eval(by______call)
}
#' @rdname TALFs
#' @export
by_Q____ <- function(.dateTime, .helpers) {
  if (.helpers[["ignoreDST"]] && !grepl(
    "^(Etc/)?(UCT|UTC)$|^(Etc/)?GMT(\\+|-)?0?$",
    .helpers[["timezone"]],
    ignore.case = TRUE
  )) {
    .dateTime <- to.fakeUTCdateTime(.dateTime, .helpers)
  }
  eval(by_Q____call)
}
#' @rdname TALFs
#' @export
by_m____ <- function(.dateTime, .helpers) {
  if (.helpers[["ignoreDST"]] && !grepl(
    "^(Etc/)?(UCT|UTC)$|^(Etc/)?GMT(\\+|-)?0?$",
    .helpers[["timezone"]],
    ignore.case = TRUE
  )) {
    .dateTime <- to.fakeUTCdateTime(.dateTime, .helpers)
  }
  if (.helpers[["multiplier"]] == 1L) {
    eval(by_m____call)
  } else {
    eval(byMult_m____call)
  }
}
#' @rdname TALFs
#' @export
by___H__ <- function(.dateTime, .helpers) {
  if (.helpers[["ignoreDST"]] && !grepl(
    "^(Etc/)?(UCT|UTC)$|^(Etc/)?GMT(\\+|-)?0?$",
    .helpers[["timezone"]],
    ignore.case = TRUE
  )) {
    .dateTime <- to.fakeUTCdateTime(.dateTime, .helpers)
  }
  if (.helpers[["multiplier"]] == 1L) {
    eval(by___H__call)
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
    eval(byMult___H__call)
  }
}
#' @rdname TALFs
#' @export
by____M_ <- function(.dateTime, .helpers) {
  if (.helpers[["multiplier"]] == 1L) {
    eval(by____M_call)
  } else {
    eval(byMult____M_call)
  }
}
#' @rdname TALFs
#' @export
by_____S <- function(.dateTime, .helpers) {
  if (.helpers[["multiplier"]] == 1L) {
    eval(by_____Scall)
  } else {
    eval(byMult_____Scall)
  }
}
