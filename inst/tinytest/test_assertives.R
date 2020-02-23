#### assertFasttimeOk ####
expect_error(
  assertFasttimeOk(
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
  assertFasttimeOk(
    seq(
      as.POSIXct("1970-01-01"),
      as.POSIXct("2199-12-31"),
      "1 year"
    ),
    list(timezone = "Europe/Vienna")
  ),
  info = 'unsuitable "timezone" returns error'
)

#### assertRecognisedPeriodicity ####
expect_error(
  assertRecognisedPeriodicity("unrecognised"),
  info = 'unrecognised "periodicity" returns error'
)

#### assertNoBeginningDot ####
expect_error(
  assertNoBeginningDot(c("col1", ".col2", ".col3")),
  info = "column names with beginning dot return error"
)
