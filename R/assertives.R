assertNAstatusPeriodicityOK <- function(
  na.status,
  periodicity,
  level = c("error", "warning")
) {
  level <- match.arg(level)

  msg <- paste(
    "This functionality may only give complete and correct results for time series with explicitly missing values and recognised periodicity.",
    'Consider calling "alter()" with "na.status = \'explicit\'" and/or specified "by" argument first.',
    sep = "\n"
  )
  if (na.status != "explicit" || periodicity == "unrecognised") {
    if (level == "error") {
      stop(msg, call. = FALSE)
    } else {
      warning(msg, call. = FALSE)
    }
  }

  TRUE
}

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

assertNoBeginningDot <- function(x) {
  if (any(grepl("^\\.", x))) {
    stop(
      sprintf('"%s" must not begin with a ".".', deparse(substitute(x))),
      call. = FALSE
    )
  }

  x
}
