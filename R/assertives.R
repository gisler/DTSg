assertFunbyApproach <- function(funbyApproach, .funbyApproaches) {
  funbyApproach <- match.arg(funbyApproach, .funbyApproaches)

  if (funbyApproach == "fasttime" &&
      !requireNamespace("fasttime", quietly = TRUE)) {
    stop(
      'Package "fasttime" must be installed for this approach.',
      call. = FALSE
    )
  } else if (funbyApproach == "RcppCCTZ" &&
             !requireNamespace("RcppCCTZ", quietly = TRUE)) {
    stop(
      'Package "RcppCCTZ" must be installed for this approach.',
      call. = FALSE
    )
  }

  invisible(funbyApproach)
}

assertFasttimeOK <- function(.dateTime, .helpers) {
  if (year(.dateTime[1L]) < 1970L || year(last(.dateTime)) > 2199L) {
    stop(
      "Timestamps must be between the years 1970 and 2199 for this approach.",
      call. = FALSE
    )
  }
  if (!grepl(
    "^(Etc/)?(UCT|UTC)$|^(Etc/)?GMT(\\+|-)?0?$",
    .helpers[["timezone"]],
    ignore.case = TRUE
  )) {
    stop(
      'Time zone must be "UTC" or equivalent for this approach.',
      call. = FALSE
    )
  }

  invisible(TRUE)
}

assertFilter <- function(x, limit) {
  if (!testMultiClass(x, c("integer", "numeric")) && !is.expression(x)) {
    stop('"i" must be a numeric vector or an expression.', call. = FALSE)
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
    if (level == "error") {
      stop(msg, call. = FALSE)
    } else {
      warning(msg, call. = FALSE)
    }
  }

  invisible(TRUE)
}

assertNoStartingDot <- function(x) {
  if (any(startsWith(x, "."))) {
    stop(
      sprintf('"%s" must not start with a ".".', deparse(substitute(x))),
      call. = FALSE
    )
  }

  invisible(x)
}
