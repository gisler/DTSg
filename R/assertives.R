assertFasttimeOK <- function(.dateTime, .helpers) {
  if (!requireNamespace("fasttime", quietly = TRUE)) {
    stop('Package "fasttime" must be installed for this TALF.', call. = FALSE)
  }
  if (year(.dateTime[1L]) < 1970L || year(last(.dateTime)) > 2199L) {
    stop(
      "Dates must be between the years 1970 and 2199 for this TALF.",
      call. = FALSE
    )
  }
  if (.helpers$timezone != "UTC") {
    stop('Time zone must be "UTC" for this TALF.', call. = FALSE)
  }

  TRUE
}

assertRecognisedPeriodicity <- function(periodicity) {
  if (periodicity == "unrecognised") {
    stop(
      paste(
        "This functionality does not work with time series of unrecognised periodicity.",
        'Please call "alter()" with specified "by" argument first.',
        sep = "\n"
      ),
      call. = FALSE
    )
  }

  periodicity
}

assertNoBeginningDot <- function(x) {
  if (any(grepl("^\\.", x))) {
    stop(
      sprintf('"%s" must not begin with a ".".', deparse(substitute(x))),
      call. = FALSE
    )
  }

  x
}
