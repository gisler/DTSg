assertFunbyApproach <- function(.dateTime, .helpers) {
  funbyApproach <- match.arg(
    .helpers[["funbyApproach"]],
    c("timechange", "base", "fasttime", "RcppCCTZ")
  )

  if (funbyApproach == "fasttime") {
    if (!requireNamespace("fasttime", quietly = TRUE)) {
      stop('Package "fasttime" must be installed for this approach.')
    }

    if (year(.dateTime[1L]) < 1970L || year(last(.dateTime)) > 2199L) {
      stop("Timestamps must be between the years 1970 and 2199 for this approach.")
    }

    if (!testSupportedTZ(.helpers[["timezone"]])) {
      stop('Time zone must be "UTC" or equivalent for this approach.')
    }
  } else if (funbyApproach == "RcppCCTZ" &&
               !requireNamespace("RcppCCTZ", quietly = TRUE)) {
    stop('Package "RcppCCTZ" must be installed for this approach.')
  }

  invisible(funbyApproach)
}

assertFilter <- function(x, limit) {
  if (!testMultiClass(x, c("integer", "numeric")) && !is.expression(x)) {
    stop('"i" must be a numeric vector or an expression.')
  } else if (testMultiClass(x, c("integer", "numeric"))) {
    assertIntegerish(
      x,
      lower = -limit,
      upper = limit,
      any.missing = FALSE,
      unique = TRUE,
      .var.name = "i"
    )
  }

  invisible(x)
}

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
    if (level == "error") { # nolint
      stop(msg)
    } else {
      warning(msg)
    }
  }

  invisible(TRUE)
}

assertNoStartingDot <- function(x) {
  if (any(startsWith(x, "."))) {
    stop(sprintf('"%s" must not start with a ".".', deparse1(substitute(x))))
  }

  invisible(x)
}

testSupportedTZ <- function(tz, anyGMT = FALSE) {
  if (anyGMT) {
    pattern <- "^(Etc/)?(UTC|UCT|Universal|Zulu)$|^(Etc/)?(GMT(\\+|-)?\\d?\\d?|Greenwich)$"
  } else {
    pattern <- "^(Etc/)?(UTC|UCT|Universal|Zulu)$|^(Etc/)?(GMT(\\+|-)?0?|Greenwich)$"
  }

  grepl(pattern, tz, ignore.case = TRUE)
}
