assert_is_fasttime_ok <- function(.dateTime, .helpers) {
  if (!requireNamespace("fasttime", quietly = TRUE)) {
    stop(sprintf("Package %s must be installed for this function.", deparse("fasttime")), call. = FALSE)
  }
  if (year(.dateTime[1L]) < 1970L || year(last(.dateTime)) > 2199L) {
    stop("Dates must be between the years 1970 and 2199 for this function.", call. = FALSE)
  }
  if (.helpers$timezone != "UTC") {
    stop('Time zone must be "UTC" for this function.', call. = FALSE)
  }
}

assert_is_length_cols_greater_than_or_equal_to_one <- function(cols) {
  if (length(cols) < 1L) {
    stop('Length of "cols" must be greater than or equal to one.', call. = FALSE)
  }
}

assert_is_periodicity_recognised <- function(periodicity) {
  if (periodicity == "unrecognised") {
    stop(
      paste(
        "This functionality doesn't work with time series of unrecognised periodicity.",
        'Please call "alter()" with specified "by" argument first.',
        sep = "\n"
      ),
      call. = FALSE
    )
  }
}
