# no R CMD check notes
.dateTime <- NULL
.colAfter <- NULL
.colBefore <- NULL
.dateTimeAfter <- NULL
.dateTimeBefore <- NULL
.fill <- NULL
.divisor <- NULL

#' Linear interpolation
#'
#' @description
#' Linearly interpolates missing values of a numeric vector. For use with the
#' [`colapply`] method of [`DTSg`] objects. Other uses are possible, but not
#' recommended.
#'
#' This [`function`] mainly serves as an example for writing user defined
#' [`function`]s utilising one of the [`list`]s with helper data handed over by
#' some of the methods of [`DTSg`] objects.
#'
#' @param .col A numeric vector.
#' @param roll A positive numeric specifying the maximum size of gaps whose
#'   missing values shall be interpolated. For time series with unrecognised
#'   periodicity it is interpreted in seconds and for time series with
#'   recognised periodicity it is multiplied with the maximum time difference
#'   between two subsequent time steps in seconds. Thus, for regular time series
#'   it is the number of time steps and for irregular it is an approximation of
#'   it.
#' @param rollends A logical specifying if missing values at the start and end
#'   of the time series shall be filled. See [`data.table::data.table`] for
#'   further information.
#' @param .helpers A [`list`] with helper data as handed over by [`colapply`].
#'   See [`colapply`] for further information.
#'
#' @return Returns a numeric vector.
#'
#' @examples
#' # new DTSg object
#' x <- DTSg$new(values = flow)
#'
#' # linear interpolation of missing values
#' ## R6 method
#' x$colapply(fun = interpolateLinear)$print()
#'
#' ## S3 method
#' print(colapply(x = x, fun = interpolateLinear))
#'
#' @export
interpolateLinear <- function(.col, roll = Inf, rollends = TRUE, .helpers) { # nolint
  qassert(.col, "n+")
  qassert(roll, "N1(0,]")

  if (.helpers[["periodicity"]] != "unrecognised") {
    roll <- roll * as.numeric(.helpers[["maxLag"]], units = "secs")
  }

  DT <- data.table(.dateTime = .helpers[[".dateTime"]], key = ".dateTime")
  values <- DT[, list(.dateTime = .dateTime[!is.na(.col)], .col = .col[!is.na(.col)])]

  DT <- values[
    DT,
    c(".dateTime", sprintf("x.%s", c(".dateTime", ".col"))),
    with = FALSE,
    roll = roll,
    rollends = rollends
  ]
  setnames(DT, c(2L, 3L), c(".dateTimeBefore", ".colBefore"))

  DT <- values[
    DT,
    c(names(DT), sprintf("x.%s", c(".dateTime", ".col"))),
    with = FALSE,
    roll = -roll,
    rollends = rollends
  ]
  setnames(DT, c(4L, 5L), c(".dateTimeAfter", ".colAfter"))

  DT[, .fill := sum(.colBefore), by = rleid(.col)]

  DT[, .divisor := as.numeric(.dateTimeAfter - .dateTimeBefore, units = "secs")]
  DT[!is.na(.fill), .col := .colBefore]
  DT[
    .divisor > 0 & !is.na(.fill),
    .col := .colBefore +
      as.numeric(.dateTime - .dateTimeBefore, units = "secs") *
      (.colAfter - .colBefore) / .divisor
  ]

  DT[[".col"]]
}

#' Rollback of months
#'
#' Generating regular sequences of time with the help of [`seq.POSIXt`] can have
#' undesirable effects. This function \dQuote{first advances the month without
#' changing the day: if this results in an invalid day of the month, it is
#' counted forward into the next month}. Monthly or yearly sequences starting at
#' the end of a month with 30 or 31 days (or 29 in case of a leap year)
#' therefore do not always fall on the end of shorter months. `rollback` fixes
#' this by counting the days of affected months backwards again.
#'
#' @param .dateTime A [`POSIXct`] vector.
#' @param periodicity A character string specifying a multiple of month(s) or
#'   year(s). See [`seq.POSIXt`] for further information.
#'
#' @return Returns a [`POSIXct`] vector.
#'
#' @examples
#' # rollback monthly time series
#' by <- "1 month"
#' rollback(
#'   .dateTime = seq(
#'     from = as.POSIXct("2000-01-31", tz = "UTC"),
#'     to = as.POSIXct("2000-12-31", tz = "UTC"),
#'     by = by
#'   ),
#'   periodicity = by
#' )
#'
#' @export
rollback <- function(.dateTime, periodicity) {
  qassert(.dateTime, "P+")
  if (!grepl("^\\d+ (month|year)(s?)$", qassert(periodicity, "S1"))) {
    stop("Periodicity must be a multiple of month(s) or year(s).")
  }

  periodicity <- unlist(strsplit(periodicity, " ", fixed = TRUE))
  periodicity[2L] <- sub("s$", "", periodicity[2L])
  clause <- switch(
    periodicity[2L],
    month = expression(any(lags > periodicity[1L])),
    year = expression(any(lags > 0))
  )

  rollToEndOfPreviousMonth <- function(lags, .dateTime, clause, periodicity) {
    if (eval(clause)) {
      .dateTime <- .dateTime + diff(seq(.dateTime, by = "-1 DSTday", length.out = 2L))
    }

    .dateTime
  }

  repeat {
    lags <- diff(month(.dateTime))
    if (!eval(clause)) {
      break
    }

    .dateTime <- structure(
      c(
        .dateTime[1L],
        do.call(
          c,
          Map(
            rollToEndOfPreviousMonth,
            lags = lags,
            .dateTime = .dateTime[-1L],
            MoreArgs = list(clause = clause, periodicity = periodicity)
          )
        )
      ),
      tzone = attr(.dateTime, "tzone")
    )
  }

  .dateTime
}
