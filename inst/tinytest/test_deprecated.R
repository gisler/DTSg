source("data.R") # nolint

options(DTSgDeprecatedWarnings = FALSE)

#### fasttime functions (UTC, multiplier == 1L) ####
if (requireNamespace("fasttime", quietly = TRUE)) {
  expect_identical(
    DTSg$new(UTChourlyData)$aggregate(byFasttimeY_____, sum)$values(TRUE)[["value"]],
    UTChourlyData[, .(value = sum(value)), by = "year"][["value"]],
    info = '"byFasttimeY_____" works as expected (UTC, multiplier == 1L)'
  )

  expect_identical(
    DTSg$new(UTChourlyData)$aggregate(byFasttimeYQ____, sum)$values(TRUE)[["value"]],
    UTChourlyData[, .(value = sum(value)), by = "quarter"][["value"]],
    info = '"byFasttimeYQ____" works as expected (UTC, multiplier == 1L)'
  )

  expect_identical(
    DTSg$new(UTChourlyData)$aggregate(byFasttimeYm____, sum)$values(TRUE)[["value"]],
    UTChourlyData[, .(value = sum(value)), by = "month"][["value"]],
    info = '"byFasttimeYm____" works as expected (UTC, multiplier == 1L)'
  )

  expect_identical(
    DTSg$new(UTChourlyData)$aggregate(byFasttimeYmd___, sum)$values(TRUE)[["value"]],
    UTChourlyData[, .(value = sum(value)), by = "day"][["value"]],
    info = '"byFasttimeYmd___" works as expected (UTC, multiplier == 1L)'
  )

  expect_identical(
    DTSg$new(UTCfractionalSecondData)$aggregate(byFasttimeYmdH__, sum)$values(TRUE)[["value"]],
    UTCfractionalSecondData[, .(value = sum(value)), by = "hour"][["value"]],
    info = '"byFasttimeYmdH__" works as expected (UTC, multiplier == 1L)'
  )

  expect_identical(
    DTSg$new(UTCfractionalSecondData)$aggregate(byFasttimeYmdHM_, sum)$values(TRUE)[["value"]],
    UTCfractionalSecondData[, .(value = sum(value)), by = "minute"][["value"]],
    info = '"byFasttimeYmdHM_" works as expected (UTC, multiplier == 1L)'
  )

  expect_identical(
    DTSg$new(UTCfractionalSecondData)$aggregate(byFasttimeYmdHMS, sum)$values(TRUE)[["value"]],
    UTCfractionalSecondData[, .(value = sum(value)), by = "second"][["value"]],
    info = '"byFasttimeYmdHMS" works as expected (UTC, multiplier == 1L)'
  )

  expect_identical(
    DTSg$new(UTChourlyData)$aggregate(byFasttime______, sum)$values(TRUE)[["value"]],
    UTChourlyData[, .(value = sum(value))][["value"]],
    info = '"byFasttime______" works as expected (UTC, multiplier == 1L)'
  )

  expect_identical(
    DTSg$new(UTChourlyData)$aggregate(byFasttime_Q____, sum)$values(TRUE)[["value"]],
    UTChourlyData[, .(value = sum(value)), keyby = "quarter"][["value"]],
    info = '"byFasttime_Q____" works as expected (UTC, multiplier == 1L)'
  )

  expect_identical(
    DTSg$new(UTChourlyData)$aggregate(byFasttime_m____, sum)$values(TRUE)[["value"]],
    UTChourlyData[, .(value = sum(value)), keyby = "month"][["value"]],
    info = '"byFasttime_m____" works as expected (UTC, multiplier == 1L)'
  )

  expect_identical(
    DTSg$new(UTCfractionalSecondData)$aggregate(byFasttime___H__, sum)$values(TRUE)[["value"]],
    UTCfractionalSecondData[, .(value = sum(value)), by = "hour"][["value"]],
    info = '"byFasttime___H__" works as expected (UTC, multiplier == 1L)'
  )

  expect_identical(
    DTSg$new(UTCfractionalSecondData)$aggregate(byFasttime____M_, sum)$values(TRUE)[["value"]],
    UTCfractionalSecondData[, .(value = sum(value)), by = "hourMinute"][["value"]],
    info = '"byFasttime____M_" works as expected (UTC, multiplier == 1L)'
  )

  expect_identical(
    DTSg$new(UTCfractionalSecondData)$aggregate(byFasttime_____S, sum)$values(TRUE)[["value"]],
    UTCfractionalSecondData[, .(value = sum(value)), by = "minuteSecond"][["value"]],
    info = '"byFasttime_____S" works as expected (UTC, multiplier == 1L)'
  )
}

#### fasttime functions (UTC, multiplier > 1L) ####
if (requireNamespace("fasttime", quietly = TRUE)) {
  expect_identical(
    DTSg$new(UTChourlyData)$aggregate(byFasttimeY_____, sum, multiplier = 2L)$values(TRUE)[["value"]],
    UTChourlyData[, .(value = sum(value)), by = .(year %/% 2L * 2L)][["value"]],
    info = '"byFasttimeY_____" works as expected (UTC, multiplier > 1L)'
  )

  expect_identical(
    DTSg$new(UTChourlyData)$aggregate(byFasttimeYm____, sum, multiplier = 4L)$values(TRUE)[["value"]],
    UTChourlyData[, .(value = sum(value)), by = .((yearMonth - 1L) %/% 4L * 4L + 1L)][["value"]],
    info = '"byFasttimeYm____" works as expected (UTC, multiplier > 1L)'
  )

  expect_identical(
    DTSg$new(UTCfractionalSecondData)$aggregate(byFasttimeYmdH__, sum, multiplier = 2L)$values(TRUE)[["value"]],
    UTCfractionalSecondData[, .(value = sum(value)), by = .(hour %/% 2L * 2L)][["value"]],
    info = '"byFasttimeYmdH__" works as expected (UTC, multiplier > 1L)'
  )

  expect_identical(
    DTSg$new(UTCfractionalSecondData)$aggregate(byFasttimeYmdHM_, sum, multiplier = 12L)$values(TRUE)[["value"]],
    UTCfractionalSecondData[, .(value = sum(value)), by = .(minute %/% 12L * 12L)][["value"]],
    info = '"byFasttimeYmdHM_" works as expected (UTC, multiplier > 1L)'
  )

  expect_identical(
    DTSg$new(UTCfractionalSecondData)$aggregate(byFasttimeYmdHMS, sum, multiplier = 30L)$values(TRUE)[["value"]],
    UTCfractionalSecondData[, .(value = sum(value)), by = .(second %/% 30L * 30L)][["value"]],
    info = '"byFasttimeYmdHMS" works as expected (UTC, multiplier > 1L)'
  )

  expect_identical(
    DTSg$new(UTChourlyData)$aggregate(byFasttime_m____, sum, multiplier = 3L)$values(TRUE)[["value"]],
    UTChourlyData[, .(value = sum(value)), keyby = .((month - 1L) %/% 3L * 3L + 1L)][["value"]],
    info = '"byFasttime_m____" works as expected (UTC, multiplier > 1L)'
  )

  expect_identical(
    DTSg$new(UTCfractionalSecondData)$aggregate(byFasttime___H__, sum, multiplier = 2L)$values(TRUE)[["value"]],
    UTCfractionalSecondData[, .(value = sum(value)), by = .(hour %/% 2L * 2L)][["value"]],
    info = '"byFasttime___H__" works as expected (UTC, multiplier > 1L)'
  )

  expect_identical(
    DTSg$new(UTCfractionalSecondData)$aggregate(byFasttime____M_, sum, multiplier = 20L)$values(TRUE)[["value"]],
    UTCfractionalSecondData[, .(value = sum(value)), by = .(hourMinute %/% 20L * 20L)][["value"]],
    info = '"byFasttime____M_" works as expected (UTC, multiplier > 1L)'
  )

  expect_identical(
    DTSg$new(UTCfractionalSecondData)$aggregate(byFasttime_____S, sum, multiplier = 15L)$values(TRUE)[["value"]],
    UTCfractionalSecondData[, .(value = sum(value)), by = .(minuteSecond %/% 15L * 15L)][["value"]],
    info = '"byFasttime_____S" works as expected (UTC, multiplier > 1L)'
  )
}

options(DTSgDeprecatedWarnings = TRUE)
