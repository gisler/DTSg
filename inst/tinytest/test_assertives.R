#### assertFunbyApproach ####
expect_error(
  DTSg:::assertFunbyApproach(, list(funbyApproach = "special")),
  pattern = "^'arg' should be one of ",
  info = "unsupported approach returns error"
)

expect_error(
  DTSg:::assertFunbyApproach(seq(
    as.POSIXct("1960-01-01"),
    as.POSIXct("2209-12-31"),
    "1 year"
  ), list(funbyApproach = "fasttime", timezone = "UTC")),
  pattern = "Timestamps must be between the years 1970 and 2199 for this approach.",
  fixed = TRUE,
  info = 'unsuitable ".dateTime" returns error'
)

expect_error(
  DTSg:::assertFunbyApproach(seq(
    as.POSIXct("1970-01-01"),
    as.POSIXct("2199-12-31"),
    "1 year"
  ), list(funbyApproach = "fasttime", timezone = "Europe/Vienna")),
  pattern = 'Time zone must be "UTC" or equivalent for this approach.',
  fixed = TRUE,
  info = "unsuitable time zone returns error"
)

#### assertFilter ####
expect_error(
  DTSg:::assertFilter(TRUE, Inf),
  pattern = '"i" must be a numeric vector or an expression.',
  fixed = TRUE,
  info = "logical returns error"
)

#### assertNAstatusPeriodicityOK ####
expect_error(
  DTSg:::assertNAstatusPeriodicityOK("explicit", "unrecognised"),
  pattern = "^This functionality may only give complete and correct results ",
  info = 'unrecognised "periodicity" returns error'
)

expect_warning(
  DTSg:::assertNAstatusPeriodicityOK("explicit", "unrecognised", "warning"),
  pattern = "^This functionality may only give complete and correct results ",
  info = 'unrecognised "periodicity" returns warning'
)

expect_error(
  DTSg:::assertNAstatusPeriodicityOK("implicit", .difftime(0, units = "secs")),
  pattern = "^This functionality may only give complete and correct results ",
  info = 'implicit "na.status" returns error'
)

expect_warning(
  DTSg:::assertNAstatusPeriodicityOK("undecided", .difftime(0, units = "secs"), "warning"),
  pattern = "^This functionality may only give complete and correct results ",
  info = 'undecided "na.status" returns warning'
)

#### assertNoStartingDot ####
expect_error(
  DTSg:::assertNoStartingDot(c("col1", ".col2", "col3")),
  pattern = 'c("col1", ".col2", "col3")" must not start with a ".".',
  fixed = TRUE,
  info = "column name with starting dot returns error"
)
