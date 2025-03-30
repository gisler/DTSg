# timechange approach ####
byTimechangeExpr <- expression(
  byY_____ = time_floor(.dateTimeForced, sprintf("%s year"  , .helpers[["multiplier"]])),
  byYm____ = time_floor(.dateTimeForced, sprintf("%s month" , .helpers[["multiplier"]])),
  byYmd___ = time_floor(.dateTimeForced, sprintf("%s day"   , .helpers[["multiplier"]])),
  byYmdH__ = time_floor(.dateTimeForced, sprintf("%s hour"  , .helpers[["multiplier"]])),
  byYmdHM_ = time_floor(.dateTimeForced, sprintf("%s minute", .helpers[["multiplier"]])),
  byYmdHMS = time_floor(.dateTimeForced, sprintf("%s second", .helpers[["multiplier"]])),

  by_m____ = time_update(.dateTimeForced, year = 2199L,             mday = 1L, hour = 0L, minute = 0L, second = 0L,  month = (month(.dateTime) - 1L) %/% .helpers[["multiplier"]] * .helpers[["multiplier"]] + 1L),
  by___H__ = time_update(.dateTimeForced, year = 2199L, month = 1L, mday = 1L,            minute = 0L, second = 0L,   hour =   hour(.dateTime)       %/% .helpers[["multiplier"]] * .helpers[["multiplier"]]     ),
  by____M_ = time_update(.dateTimeForced, year = 2199L, month = 1L, mday = 1L, hour = 0L,              second = 0L, minute = minute(.dateTime)       %/% .helpers[["multiplier"]] * .helpers[["multiplier"]]     ),
  by_____S = time_update(.dateTimeForced, year = 2199L, month = 1L, mday = 1L, hour = 0L, minute = 0L,              second = second(.dateTime)       %/% .helpers[["multiplier"]] * .helpers[["multiplier"]]     )
)

timechangeApproach <- function(.dateTime, .helpers, by) {
  if (by %chin% c("byYQ____", "by_Q____")) {
    by <- if (by == "byYQ____") "byYm____" else "by_m____"
    .helpers[["multiplier"]] <- 3L
  }

  if (attr(.dateTime, "tzone") != .helpers[["timezone"]]) {
    .dateTimeForced <- time_force_tz(.dateTime, tz = .helpers[["timezone"]])
  } else {
    .dateTimeForced <- .dateTime
  }

  eval(byTimechangeExpr[[by]])
}

# Nested list of expressions ####
byExpr <- list(
  single = list(
    base = expression(
      byY_____ = as.POSIXct(  trunc(.dateTime     , units = "years"                              ), tz = .helpers[["timezone"]]),
      byYQ____ = as.POSIXct(sprintf("%04d-%02d-01", year(.dateTime), quarter(.dateTime) * 3L - 2L), tz = .helpers[["timezone"]]),
      byYm____ = as.POSIXct(  trunc(.dateTime     , units = "months"                             ), tz = .helpers[["timezone"]]),
      byYmd___ = as.POSIXct(  trunc(.dateTime     , units = "days"                               ), tz = .helpers[["timezone"]]),
      byYmdH__ = as.POSIXct(  trunc(.dateTime     , units = "hours"                              ), tz = .helpers[["timezone"]]),
      byYmdHM_ = as.POSIXct(  trunc(.dateTime     , units = "mins"                               ), tz = .helpers[["timezone"]]),
      byYmdHMS = as.POSIXct(  trunc(.dateTime     , units = "secs"                               ), tz = .helpers[["timezone"]]),

      by______ = rep(as.POSIXct("2199-01-01", tz = .helpers[["timezone"]]), length(.dateTime)),
      by_Q____ = as.POSIXct(sprintf("2199-%02d-01"         , quarter(.dateTime) * 3L - 2L), tz = .helpers[["timezone"]]),
      by_m____ = as.POSIXct(sprintf("2199-%02d-01"         ,   month(.dateTime)          ), tz = .helpers[["timezone"]]),
      by___H__ = as.POSIXct(sprintf("2199-01-01 %02d:00:00",    hour(.dateTime)          ), tz = .helpers[["timezone"]]),
      by____M_ = as.POSIXct(sprintf("2199-01-01 00:%02d:00",  minute(.dateTime)          ), tz = .helpers[["timezone"]]),
      by_____S = as.POSIXct(sprintf("2199-01-01 00:00:%02d",  second(.dateTime)          ), tz = .helpers[["timezone"]])
    ),
    fasttime = expression(
      byY_____ = fasttime::fastPOSIXct(sprintf("%04d-01-01"                   , year(.dateTime)                                                                                            ), tz = .helpers[["timezone"]]),
      byYQ____ = fasttime::fastPOSIXct(sprintf("%04d-%02d-01"                 , year(.dateTime), quarter(.dateTime) * 3L - 2L                                                              ), tz = .helpers[["timezone"]]),
      byYm____ = fasttime::fastPOSIXct(sprintf("%04d-%02d-01"                 , year(.dateTime),   month(.dateTime)                                                                        ), tz = .helpers[["timezone"]]),
      byYmd___ = fasttime::fastPOSIXct(sprintf("%04d-%02d-%02d"               , year(.dateTime),   month(.dateTime), mday(.dateTime)                                                       ), tz = .helpers[["timezone"]]),
      byYmdH__ = fasttime::fastPOSIXct(sprintf("%04d-%02d-%02d %02d:00:00"    , year(.dateTime),   month(.dateTime), mday(.dateTime), hour(.dateTime)                                      ), tz = .helpers[["timezone"]]),
      byYmdHM_ = fasttime::fastPOSIXct(sprintf("%04d-%02d-%02d %02d:%02d:00"  , year(.dateTime),   month(.dateTime), mday(.dateTime), hour(.dateTime), minute(.dateTime)                   ), tz = .helpers[["timezone"]]),
      byYmdHMS = fasttime::fastPOSIXct(sprintf("%04d-%02d-%02d %02d:%02d:%02d", year(.dateTime),   month(.dateTime), mday(.dateTime), hour(.dateTime), minute(.dateTime), second(.dateTime)), tz = .helpers[["timezone"]]),

      by_Q____ = fasttime::fastPOSIXct(sprintf("2199-%02d-01"         , quarter(.dateTime) * 3L - 2L), tz = .helpers[["timezone"]]),
      by_m____ = fasttime::fastPOSIXct(sprintf("2199-%02d-01"         ,   month(.dateTime)          ), tz = .helpers[["timezone"]]),
      by___H__ = fasttime::fastPOSIXct(sprintf("2199-01-01 %02d:00:00",    hour(.dateTime)          ), tz = .helpers[["timezone"]]),
      by____M_ = fasttime::fastPOSIXct(sprintf("2199-01-01 00:%02d:00",  minute(.dateTime)          ), tz = .helpers[["timezone"]]),
      by_____S = fasttime::fastPOSIXct(sprintf("2199-01-01 00:00:%02d",  second(.dateTime)          ), tz = .helpers[["timezone"]])
    ),
    RcppCCTZ = expression(
      byY_____ = RcppCCTZ::parseDatetime(sprintf("%04d-01-01"                     , year(.dateTime)                                                                                                                                                    ), fmt = "%Y-%m-%d"             , tzstr = .helpers[["timezone"]]),
      byYQ____ = RcppCCTZ::parseDatetime(sprintf("%04d-%02d-01"                   , year(.dateTime), quarter(.dateTime) * 3L - 2L                                                                                                                      ), fmt = "%Y-%m-%d"             , tzstr = .helpers[["timezone"]]),
      byYm____ = RcppCCTZ::parseDatetime(sprintf("%04d-%02d-01"                   , year(.dateTime),   month(.dateTime)                                                                                                                                ), fmt = "%Y-%m-%d"             , tzstr = .helpers[["timezone"]]),
      byYmd___ = RcppCCTZ::parseDatetime(sprintf("%04d-%02d-%02d"                 , year(.dateTime),   month(.dateTime), mday(.dateTime)                                                                                                               ), fmt = "%Y-%m-%d"             , tzstr = .helpers[["timezone"]]),
      byYmdH__ = RcppCCTZ::parseDatetime(sprintf("%04d-%02d-%02d %02d:00:00%s"    , year(.dateTime),   month(.dateTime), mday(.dateTime), hour(.dateTime)                                      , strftime(.dateTime, "%z", tz = .helpers[["timezone"]])), fmt = "%Y-%m-%d %H:%M:%E1S%z", tzstr = .helpers[["timezone"]]),
      byYmdHM_ = RcppCCTZ::parseDatetime(sprintf("%04d-%02d-%02d %02d:%02d:00%s"  , year(.dateTime),   month(.dateTime), mday(.dateTime), hour(.dateTime), minute(.dateTime)                   , strftime(.dateTime, "%z", tz = .helpers[["timezone"]])), fmt = "%Y-%m-%d %H:%M:%E1S%z", tzstr = .helpers[["timezone"]]),
      byYmdHMS = RcppCCTZ::parseDatetime(sprintf("%04d-%02d-%02d %02d:%02d:%02d%s", year(.dateTime),   month(.dateTime), mday(.dateTime), hour(.dateTime), minute(.dateTime), second(.dateTime), strftime(.dateTime, "%z", tz = .helpers[["timezone"]])), fmt = "%Y-%m-%d %H:%M:%E1S%z", tzstr = .helpers[["timezone"]]),

      by_Q____ = RcppCCTZ::parseDatetime(sprintf("2199-%02d-01"         , quarter(.dateTime) * 3L - 2L), fmt = "%Y-%m-%d"           , tzstr = .helpers[["timezone"]]),
      by_m____ = RcppCCTZ::parseDatetime(sprintf("2199-%02d-01"         ,   month(.dateTime)          ), fmt = "%Y-%m-%d"           , tzstr = .helpers[["timezone"]]),
      by___H__ = RcppCCTZ::parseDatetime(sprintf("2199-01-01 %02d:00:00",    hour(.dateTime)          ), fmt = "%Y-%m-%d %H:%M:%E1S", tzstr = .helpers[["timezone"]]),
      by____M_ = RcppCCTZ::parseDatetime(sprintf("2199-01-01 00:%02d:00",  minute(.dateTime)          ), fmt = "%Y-%m-%d %H:%M:%E1S", tzstr = .helpers[["timezone"]]),
      by_____S = RcppCCTZ::parseDatetime(sprintf("2199-01-01 00:00:%02d",  second(.dateTime)          ), fmt = "%Y-%m-%d %H:%M:%E1S", tzstr = .helpers[["timezone"]])
    )
  ),
  multi = list(
    base = expression(
      byY_____ = as.POSIXct(sprintf("%04d-01-01"                     , year(.dateTime)                                                                                            %/% .helpers[["multiplier"]] * .helpers[["multiplier"]]                                                        ), tz = .helpers[["timezone"]]                                ),
      byYm____ = as.POSIXct(sprintf("%04d-%02d-01"                   , year(.dateTime), (month(.dateTime) - 1L)                                                                   %/% .helpers[["multiplier"]] * .helpers[["multiplier"]] + 1L                                                   ), tz = .helpers[["timezone"]]                                ),
      byYmdH__ = as.POSIXct(sprintf("%04d-%02d-%02d %02d:00:00"      , year(.dateTime),  month(.dateTime), mday(.dateTime), hour(.dateTime)                                       %/% .helpers[["multiplier"]] * .helpers[["multiplier"]]                                                        ), tz = .helpers[["timezone"]]                                ),
      byYmdHM_ = as.POSIXct(sprintf("%04d-%02d-%02d %02d:%02d:00%s"  , year(.dateTime),  month(.dateTime), mday(.dateTime), hour(.dateTime), minute(.dateTime)                    %/% .helpers[["multiplier"]] * .helpers[["multiplier"]], strftime(.dateTime, "%z", tz = .helpers[["timezone"]])), tz = .helpers[["timezone"]], format = "%Y-%m-%d %H:%M:%S%z"),
      byYmdHMS = as.POSIXct(sprintf("%04d-%02d-%02d %02d:%02d:%02d%s", year(.dateTime),  month(.dateTime), mday(.dateTime), hour(.dateTime), minute(.dateTime), second(.dateTime) %/% .helpers[["multiplier"]] * .helpers[["multiplier"]], strftime(.dateTime, "%z", tz = .helpers[["timezone"]])), tz = .helpers[["timezone"]], format = "%Y-%m-%d %H:%M:%S%z"),

      by_m____ = as.POSIXct(sprintf("2199-%02d-01"         , (month(.dateTime) - 1L) %/% .helpers[["multiplier"]] * .helpers[["multiplier"]] + 1L), tz = .helpers[["timezone"]]),
      by___H__ = as.POSIXct(sprintf("2199-01-01 %02d:00:00",   hour(.dateTime)       %/% .helpers[["multiplier"]] * .helpers[["multiplier"]]     ), tz = .helpers[["timezone"]]),
      by____M_ = as.POSIXct(sprintf("2199-01-01 00:%02d:00", minute(.dateTime)       %/% .helpers[["multiplier"]] * .helpers[["multiplier"]]     ), tz = .helpers[["timezone"]]),
      by_____S = as.POSIXct(sprintf("2199-01-01 00:00:%02d", second(.dateTime)       %/% .helpers[["multiplier"]] * .helpers[["multiplier"]]     ), tz = .helpers[["timezone"]])
    ),
    fasttime = expression(
      byY_____ = fasttime::fastPOSIXct(sprintf("%04d-01-01"                   , year(.dateTime)                                                                                            %/% .helpers[["multiplier"]] * .helpers[["multiplier"]]     ), tz = .helpers[["timezone"]]),
      byYm____ = fasttime::fastPOSIXct(sprintf("%04d-%02d-01"                 , year(.dateTime), (month(.dateTime) - 1L)                                                                   %/% .helpers[["multiplier"]] * .helpers[["multiplier"]] + 1L), tz = .helpers[["timezone"]]),
      byYmdH__ = fasttime::fastPOSIXct(sprintf("%04d-%02d-%02d %02d:00:00"    , year(.dateTime),  month(.dateTime), mday(.dateTime), hour(.dateTime)                                       %/% .helpers[["multiplier"]] * .helpers[["multiplier"]]     ), tz = .helpers[["timezone"]]),
      byYmdHM_ = fasttime::fastPOSIXct(sprintf("%04d-%02d-%02d %02d:%02d:00"  , year(.dateTime),  month(.dateTime), mday(.dateTime), hour(.dateTime), minute(.dateTime)                    %/% .helpers[["multiplier"]] * .helpers[["multiplier"]]     ), tz = .helpers[["timezone"]]),
      byYmdHMS = fasttime::fastPOSIXct(sprintf("%04d-%02d-%02d %02d:%02d:%02d", year(.dateTime),  month(.dateTime), mday(.dateTime), hour(.dateTime), minute(.dateTime), second(.dateTime) %/% .helpers[["multiplier"]] * .helpers[["multiplier"]]     ), tz = .helpers[["timezone"]]),

      by_m____ = fasttime::fastPOSIXct(sprintf("2199-%02d-01"         , (month(.dateTime) - 1L) %/% .helpers[["multiplier"]] * .helpers[["multiplier"]] + 1L), tz = .helpers[["timezone"]]),
      by___H__ = fasttime::fastPOSIXct(sprintf("2199-01-01 %02d:00:00",   hour(.dateTime)       %/% .helpers[["multiplier"]] * .helpers[["multiplier"]]     ), tz = .helpers[["timezone"]]),
      by____M_ = fasttime::fastPOSIXct(sprintf("2199-01-01 00:%02d:00", minute(.dateTime)       %/% .helpers[["multiplier"]] * .helpers[["multiplier"]]     ), tz = .helpers[["timezone"]]),
      by_____S = fasttime::fastPOSIXct(sprintf("2199-01-01 00:00:%02d", second(.dateTime)       %/% .helpers[["multiplier"]] * .helpers[["multiplier"]]     ), tz = .helpers[["timezone"]])
    ),
    RcppCCTZ = expression(
      byY_____ = RcppCCTZ::parseDatetime(sprintf("%04d-01-01"                     , year(.dateTime)                                                                                            %/% .helpers[["multiplier"]] * .helpers[["multiplier"]]                                                        ), fmt = "%Y-%m-%d"             , tzstr = .helpers[["timezone"]]),
      byYm____ = RcppCCTZ::parseDatetime(sprintf("%04d-%02d-01"                   , year(.dateTime), (month(.dateTime) - 1L)                                                                   %/% .helpers[["multiplier"]] * .helpers[["multiplier"]] + 1L                                                   ), fmt = "%Y-%m-%d"             , tzstr = .helpers[["timezone"]]),
      byYmdH__ = RcppCCTZ::parseDatetime(sprintf("%04d-%02d-%02d %02d:00:00"      , year(.dateTime),  month(.dateTime), mday(.dateTime), hour(.dateTime)                                       %/% .helpers[["multiplier"]] * .helpers[["multiplier"]]                                                        ), fmt = "%Y-%m-%d %H:%M:%E1S"  , tzstr = .helpers[["timezone"]]),
      byYmdHM_ = RcppCCTZ::parseDatetime(sprintf("%04d-%02d-%02d %02d:%02d:00%s"  , year(.dateTime),  month(.dateTime), mday(.dateTime), hour(.dateTime), minute(.dateTime)                    %/% .helpers[["multiplier"]] * .helpers[["multiplier"]], strftime(.dateTime, "%z", tz = .helpers[["timezone"]])), fmt = "%Y-%m-%d %H:%M:%E1S%z", tzstr = .helpers[["timezone"]]),
      byYmdHMS = RcppCCTZ::parseDatetime(sprintf("%04d-%02d-%02d %02d:%02d:%02d%s", year(.dateTime),  month(.dateTime), mday(.dateTime), hour(.dateTime), minute(.dateTime), second(.dateTime) %/% .helpers[["multiplier"]] * .helpers[["multiplier"]], strftime(.dateTime, "%z", tz = .helpers[["timezone"]])), fmt = "%Y-%m-%d %H:%M:%E1S%z", tzstr = .helpers[["timezone"]]),

      by_m____ = RcppCCTZ::parseDatetime(sprintf("2199-%02d-01"         , (month(.dateTime) - 1L) %/% .helpers[["multiplier"]] * .helpers[["multiplier"]] + 1L), fmt = "%Y-%m-%d"           , tzstr = .helpers[["timezone"]]),
      by___H__ = RcppCCTZ::parseDatetime(sprintf("2199-01-01 %02d:00:00",   hour(.dateTime)       %/% .helpers[["multiplier"]] * .helpers[["multiplier"]]     ), fmt = "%Y-%m-%d %H:%M:%E1S", tzstr = .helpers[["timezone"]]),
      by____M_ = RcppCCTZ::parseDatetime(sprintf("2199-01-01 00:%02d:00", minute(.dateTime)       %/% .helpers[["multiplier"]] * .helpers[["multiplier"]]     ), fmt = "%Y-%m-%d %H:%M:%E1S", tzstr = .helpers[["timezone"]]),
      by_____S = RcppCCTZ::parseDatetime(sprintf("2199-01-01 00:00:%02d", second(.dateTime)       %/% .helpers[["multiplier"]] * .helpers[["multiplier"]]     ), fmt = "%Y-%m-%d %H:%M:%E1S", tzstr = .helpers[["timezone"]])
    ),
    timechange = expression(
      byY_____ = timechangeApproach(.dateTime, .helpers, "byY_____"),
      byYQ____ = timechangeApproach(.dateTime, .helpers, "byYQ____"),
      byYm____ = timechangeApproach(.dateTime, .helpers, "byYm____"),
      byYmd___ = timechangeApproach(.dateTime, .helpers, "byYmd___"),
      byYmdH__ = timechangeApproach(.dateTime, .helpers, "byYmdH__"),
      byYmdHM_ = timechangeApproach(.dateTime, .helpers, "byYmdHM_"),
      byYmdHMS = timechangeApproach(.dateTime, .helpers, "byYmdHMS"),

      by_Q____ = timechangeApproach(.dateTime, .helpers, "by_Q____"),
      by_m____ = timechangeApproach(.dateTime, .helpers, "by_m____"),
      by___H__ = timechangeApproach(.dateTime, .helpers, "by___H__"),
      by____M_ = timechangeApproach(.dateTime, .helpers, "by____M_"),
      by_____S = timechangeApproach(.dateTime, .helpers, "by_____S")
    )
  )
)
byExpr[["single"]][["timechange"]] <- byExpr[["multi"]][["timechange"]]

# Functions ####
toFakeUTCdateTime <- function(.dateTime, .helpers) {
  assertNAstatusPeriodicityOK(
    .helpers[["na.status"]],
    .helpers[["periodicity"]],
    level = "error"
  )

  from <- .dateTime[1L]

  if (as.POSIXlt(from)$isdst) {
    ts <- seq(
      from,
      as.POSIXct("1916-01-01", tz = .helpers[["timezone"]]),
      by = "-1 DSTday"
    )

    i <- first(which(as.POSIXlt(ts)$isdst == 0L))
    lagDSTday <- diff(ts[c(i, i - 1L)])

    if (is.na(lagDSTday)) {
      lagDST <- 3600L

      warning(
        "The day saving time shift cannot be estimated and is assumed to be ",
        "one hour long."
      )
    } else {
      lagDST <- as.difftime(
        1L,
        units = "days",
        tz = .helpers[["timezone"]]
      ) - lagDSTday
    }

    from <- from - lagDST
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
#' part of them (extracting family). Each family comes in four flavours: the
#' first relies solely on base \R, the second utilises [`fasttime::fastPOSIXct`]
#' of \pkg{fasttime}, the third [`RcppCCTZ::parseDatetime`] of \pkg{RcppCCTZ}
#' and the fourth [`timechange::time_floor`] of \pkg{timechange}.
#'
#' The \pkg{timechange} flavour generally is the fastest for both families of
#' functions and all time zones. Second best option for the extracting family of
#' functions generally is the \pkg{fasttime} flavour, which, however, works with
#' UTC and equivalent as well as all Etc/GMT time zones only (execute
#' `grep("^(Etc/)?(UTC|UCT|Universal|Zulu)$|^(Etc/)?(GMT(\\+|-)?0?|Greenwich)$",
#' OlsonNames(), ignore.case = TRUE, value = TRUE)` for a full list of supported
#' time zones) and is limited to timestamps between the years 1970 and 2199. For
#' time zones other than UTC and equivalent the \pkg{RcppCCTZ} flavour generally
#' is the second best option.
#'
#' Use the `funbyApproach` argument of the respective calling method in order to
#' specify the utilised flavour.
#'
#' The truncating family sets timestamps to the lowest possible point in time of
#' the corresponding temporal aggregation level:
#' - `byY_____` truncates to years,    e.g. _2000-11-11 11:11:11.1_ becomes _2000-01-01 00:00:00.0_
#' - `byYQ____` truncates to quarters, e.g. _2000-11-11 11:11:11.1_ becomes _2000-10-01 00:00:00.0_
#' - `byYm____` truncates to months,   e.g. _2000-11-11 11:11:11.1_ becomes _2000-11-01 00:00:00.0_
#' - `byYmd___` truncates to days,     e.g. _2000-11-11 11:11:11.1_ becomes _2000-11-11 00:00:00.0_
#' - `byYmdH__` truncates to hours,    e.g. _2000-11-11 11:11:11.1_ becomes _2000-11-11 11:00:00.0_
#' - `byYmdHM_` truncates to minutes,  e.g. _2000-11-11 11:11:11.1_ becomes _2000-11-11 11:11:00.0_
#' - `byYmdHMS` truncates to seconds,  e.g. _2000-11-11 11:11:11.1_ becomes _2000-11-11 11:11:11.0_
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
  assertFunbyApproach(.dateTime, .helpers)

  if (.helpers[["ignoreDST"]] && !testSupportedTZ(.helpers[["timezone"]])) {
    .dateTime <- toFakeUTCdateTime(.dateTime, .helpers)
  }

  singleOrMulti <- if (.helpers[["multiplier"]] == 1L) "single" else "multi"

  eval(byExpr[[singleOrMulti]][[.helpers[["funbyApproach"]]]][["byY_____"]])
}

#' @rdname TALFs
#' @export
byYQ____ <- function(.dateTime, .helpers) {
  assertFunbyApproach(.dateTime, .helpers)

  if (.helpers[["ignoreDST"]] && !testSupportedTZ(.helpers[["timezone"]])) {
    .dateTime <- toFakeUTCdateTime(.dateTime, .helpers)
  }

  eval(byExpr[["single"]][[.helpers[["funbyApproach"]]]][["byYQ____"]])
}

#' @rdname TALFs
#' @export
byYm____ <- function(.dateTime, .helpers) {
  assertFunbyApproach(.dateTime, .helpers)

  if (.helpers[["ignoreDST"]] && !testSupportedTZ(.helpers[["timezone"]])) {
    .dateTime <- toFakeUTCdateTime(.dateTime, .helpers)
  }

  singleOrMulti <- if (.helpers[["multiplier"]] == 1L) "single" else "multi"

  eval(byExpr[[singleOrMulti]][[.helpers[["funbyApproach"]]]][["byYm____"]])
}

#' @rdname TALFs
#' @export
byYmd___ <- function(.dateTime, .helpers) {
  assertFunbyApproach(.dateTime, .helpers)

  if (.helpers[["ignoreDST"]] && !testSupportedTZ(.helpers[["timezone"]])) {
    .dateTime <- toFakeUTCdateTime(.dateTime, .helpers)
  }

  eval(byExpr[["single"]][[.helpers[["funbyApproach"]]]][["byYmd___"]])
}

#' @rdname TALFs
#' @export
byYmdH__ <- function(.dateTime, .helpers) {
  assertFunbyApproach(.dateTime, .helpers)

  singleOrMulti <- if (.helpers[["multiplier"]] == 1L) "single" else "multi"
  if (singleOrMulti == "multi" && !testSupportedTZ(
    .helpers[["timezone"]],
    anyGMT = TRUE
  )) {
    stop(
      'Time zone must be "UTC" or equivalent or any GMT for this TALF with a ',
      "multiplier greater than one."
    )
  }

  eval(byExpr[[singleOrMulti]][[.helpers[["funbyApproach"]]]][["byYmdH__"]])
}

#' @rdname TALFs
#' @export
byYmdHM_ <- function(.dateTime, .helpers) {
  assertFunbyApproach(.dateTime, .helpers)

  singleOrMulti <- if (.helpers[["multiplier"]] == 1L) "single" else "multi"

  eval(byExpr[[singleOrMulti]][[.helpers[["funbyApproach"]]]][["byYmdHM_"]])
}

#' @rdname TALFs
#' @export
byYmdHMS <- function(.dateTime, .helpers) {
  assertFunbyApproach(.dateTime, .helpers)

  singleOrMulti <- if (.helpers[["multiplier"]] == 1L) "single" else "multi"

  eval(byExpr[[singleOrMulti]][[.helpers[["funbyApproach"]]]][["byYmdHMS"]])
}

## Extracting family ####
#' @rdname TALFs
#' @export
by______ <- function(.dateTime, .helpers) {
  eval(byExpr[["single"]][["base"]][["by______"]])
}

#' @rdname TALFs
#' @export
by_Q____ <- function(.dateTime, .helpers) {
  assertFunbyApproach(.dateTime, .helpers)

  if (.helpers[["ignoreDST"]] && !testSupportedTZ(.helpers[["timezone"]])) {
    .dateTime <- toFakeUTCdateTime(.dateTime, .helpers)
  }

  eval(byExpr[["single"]][[.helpers[["funbyApproach"]]]][["by_Q____"]])
}

#' @rdname TALFs
#' @export
by_m____ <- function(.dateTime, .helpers) {
  assertFunbyApproach(.dateTime, .helpers)

  if (.helpers[["ignoreDST"]] && !testSupportedTZ(.helpers[["timezone"]])) {
    .dateTime <- toFakeUTCdateTime(.dateTime, .helpers)
  }

  singleOrMulti <- if (.helpers[["multiplier"]] == 1L) "single" else "multi"

  eval(byExpr[[singleOrMulti]][[.helpers[["funbyApproach"]]]][["by_m____"]])
}

#' @rdname TALFs
#' @export
by___H__ <- function(.dateTime, .helpers) {
  assertFunbyApproach(.dateTime, .helpers)

  if (.helpers[["ignoreDST"]] && !testSupportedTZ(.helpers[["timezone"]])) {
    .dateTime <- toFakeUTCdateTime(.dateTime, .helpers)
  }

  singleOrMulti <- if (.helpers[["multiplier"]] == 1L) "single" else "multi"
  if (singleOrMulti == "multi" && !testSupportedTZ(
    .helpers[["timezone"]],
    anyGMT = TRUE
  )) {
    stop(
      'Time zone must be "UTC" or equivalent or any GMT for this TALF with a ',
      "multiplier greater than one."
    )
  }

  eval(byExpr[[singleOrMulti]][[.helpers[["funbyApproach"]]]][["by___H__"]])
}

#' @rdname TALFs
#' @export
by____M_ <- function(.dateTime, .helpers) {
  assertFunbyApproach(.dateTime, .helpers)

  singleOrMulti <- if (.helpers[["multiplier"]] == 1L) "single" else "multi"

  eval(byExpr[[singleOrMulti]][[.helpers[["funbyApproach"]]]][["by____M_"]])
}

#' @rdname TALFs
#' @export
by_____S <- function(.dateTime, .helpers) {
  assertFunbyApproach(.dateTime, .helpers)

  singleOrMulti <- if (.helpers[["multiplier"]] == 1L) "single" else "multi"

  eval(byExpr[[singleOrMulti]][[.helpers[["funbyApproach"]]]][["by_____S"]])
}
