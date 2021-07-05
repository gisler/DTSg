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
    stop("Periodicity must be a multiple of month(s) or year(s).", call. = FALSE)
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
