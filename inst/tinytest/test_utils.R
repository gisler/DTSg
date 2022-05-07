source("data.R") # nolint

#### interpolateLinear ####
expect_identical(
  DTSg$new(DT4)$colapply(interpolateLinear)$values(TRUE)[["col2"]],
  c(1:5, rep(6, 3)),
  info = '"interpolateLinear" works correctly'
)

expect_identical(
  DTSg$new(DT4)$colapply(interpolateLinear, roll = 2)$values(TRUE)[["col2"]],
  c(1, rep(NA, 3), 5, rep(6, 3)),
  info = '"interpolateLinear" with "roll" > gap / 2 works correctly'
)

expect_identical(
  DTSg$new(DT4)$colapply(interpolateLinear, roll = 1)$values(TRUE)[["col2"]],
  c(1, rep(NA, 3), 5, 6, NA, NA),
  info = '"interpolateLinear" with "roll" < gap / 2 works correctly'
)

#### rollback ####
expect_error(
  rollback(DT1[["date"]], "30 mins"),
  info = 'wrong "periodicity" returns error'
)

expect_identical(
  rollback(seq(
    as.POSIXct("2000-01-31", tz = "Europe/Vienna"),
    as.POSIXct("2000-07-01", tz = "Europe/Vienna"),
    "1 month"
  ), "1 month"),
  as.POSIXct(
    c("2000-01-31", "2000-02-29", "2000-03-31", "2000-04-30", "2000-05-31", "2000-06-30"),
    tz = "Europe/Vienna"
  ),
  info = "rolling back months works correctly"
)

expect_identical(
  rollback(seq(
    as.POSIXct("2000-02-29", tz = "Europe/Vienna"),
    as.POSIXct("2010-03-01", tz = "Europe/Vienna"),
    "2 years"
  ), "2 years"),
  as.POSIXct(
    c("2000-02-29", "2002-02-28", "2004-02-29", "2006-02-28", "2008-02-29", "2010-02-28"),
    tz = "Europe/Vienna"
  ),
  info = "rolling back years works correctly"
)
