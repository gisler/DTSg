#### assertNAstatusPeriodicityOK ####
expect_error(
  assertNAstatusPeriodicityOK("implicit", .difftime(0, units = "secs")),
  info = 'implicit "na.status" returns error'
)

# expect_warning(
#   assertNAstatusPeriodicityOK("undecided", .difftime(0, units = "secs"), "warning"),
#   info = 'undecided "na.status" returns warning'
# )

expect_error(
  assertNAstatusPeriodicityOK("explicit", "unrecognised"),
  info = 'unrecognised "periodicity" returns error'
)

# expect_warning(
#   assertNAstatusPeriodicityOK("explicit", "unrecognised", "warning"),
#   info = 'unrecognised "periodicity" returns warning'
# )

#### assertFasttimeOK ####
expect_error(
  assertFasttimeOK(
    seq(
      as.POSIXct("1960-01-01"),
      as.POSIXct("2209-12-31"),
      "1 year"
    ),
    list(timezone = "UTC")
  ),
  info = 'unsuitable ".dateTime" returns error'
)

expect_error(
  assertFasttimeOK(
    seq(
      as.POSIXct("1970-01-01"),
      as.POSIXct("2199-12-31"),
      "1 year"
    ),
    list(timezone = "Europe/Vienna")
  ),
  info = 'unsuitable "timezone" returns error'
)

#### assertNoBeginningDot ####
expect_error(
  assertNoBeginningDot(c("col1", ".col2", ".col3")),
  info = "column names with beginning dot return error"
)
