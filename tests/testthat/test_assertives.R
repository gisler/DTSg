#### assertFasttimeOk ####
context("assertFasttimeOk")

test_that(
  'unsuitable ".dateTime" returns error',
  expect_error(
    assertFasttimeOk(
      seq(
        as.POSIXct("1960-01-01"),
        as.POSIXct("2209-12-31"),
        "1 year"
      ),
      list(timezone = "UTC")
    )
  )
)

test_that(
  'unsuitable "timezone" returns error',
  expect_error(
    assertFasttimeOk(
      seq(
        as.POSIXct("1970-01-01"),
        as.POSIXct("2199-12-31"),
        "1 year"
      ),
      list(timezone = "Europe/Vienna")
    )
  )
)

#### assertRecognisedPeriodicity ####
context("assertRecognisedPeriodicity")

test_that(
  'unrecognised "periodicity" returns error',
  expect_error(
    assertRecognisedPeriodicity("unrecognised")
  )
)

#### assertNoBeginningDot ####
context("assertNoBeginningDot")

test_that(
  'column names with beginning dot return error',
  expect_error(
    assertNoBeginningDot(c("col1", ".col2", ".col3"))
  )
)
