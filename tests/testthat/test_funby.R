#### fasttime functions (UTC) ####
context("fasttime functions (UTC)")

test_that(
  '"byFasttimeY_____" works as expected (UTC)',
  {
    skip_if_not_installed("fasttime")
    expect_equal(
      DTSg$new(UTChourlyData)$aggregate(byFasttimeY_____, sum)$values(TRUE)[["value"]],
      UTChourlyData[, .(value = sum(value)), by = "year"][["value"]]
    )
  }
)

test_that(
  '"byFasttimeYQ____" works as expected (UTC)',
  {
    skip_if_not_installed("fasttime")
    expect_equal(
      DTSg$new(UTChourlyData)$aggregate(byFasttimeYQ____, sum)$values(TRUE)[["value"]],
      UTChourlyData[, .(value = sum(value)), by = "quarter"][["value"]]
    )
  }
)

test_that(
  '"byFasttimeYm____" works as expected (UTC)',
  {
    skip_if_not_installed("fasttime")
    expect_equal(
      DTSg$new(UTChourlyData)$aggregate(byFasttimeYm____, sum)$values(TRUE)[["value"]],
      UTChourlyData[, .(value = sum(value)), by = "month"][["value"]]
    )
  }
)

test_that(
  '"byFasttimeYmd___" works as expected (UTC)',
  {
    skip_if_not_installed("fasttime")
    expect_equal(
      DTSg$new(UTChourlyData)$aggregate(byFasttimeYmd___, sum)$values(TRUE)[["value"]],
      UTChourlyData[, .(value = sum(value)), by = "day"][["value"]]
    )
  }
)

test_that(
  '"byFasttimeYmdH__" works as expected (UTC)',
  {
    skip_if_not_installed("fasttime")
    expect_equal(
      DTSg$new(UTCfractionalSecondData)$aggregate(byFasttimeYmdH__, sum)$values(TRUE)[["value"]],
      UTCfractionalSecondData[, .(value = sum(value)), by = "hour"][["value"]]
    )
  }
)

test_that(
  '"byFasttimeYmdHM_" works as expected (UTC)',
  {
    skip_if_not_installed("fasttime")
    expect_equal(
      DTSg$new(UTCfractionalSecondData)$aggregate(byFasttimeYmdHM_, sum)$values(TRUE)[["value"]],
      UTCfractionalSecondData[, .(value = sum(value)), by = "minute"][["value"]]
    )
  }
)

test_that(
  '"byFasttimeYmdHMS" works as expected (UTC)',
  {
    skip_if_not_installed("fasttime")
    expect_equal(
      DTSg$new(UTCfractionalSecondData)$aggregate(byFasttimeYmdHMS, sum)$values(TRUE)[["value"]],
      UTCfractionalSecondData[, .(value = sum(value)), by = "second"][["value"]]
    )
  }
)

test_that(
  '"byFasttime______" works as expected (UTC)',
  {
    skip_if_not_installed("fasttime")
    expect_equal(
      DTSg$new(UTChourlyData)$aggregate(byFasttime______, sum)$values(TRUE)[["value"]],
      UTChourlyData[, .(value = sum(value))][["value"]]
    )
  }
)

test_that(
  '"byFasttime_Q____" works as expected (UTC)',
  {
    skip_if_not_installed("fasttime")
    expect_equal(
      DTSg$new(UTChourlyData)$aggregate(byFasttime_Q____, sum)$values(TRUE)[["value"]],
      UTChourlyData[, .(value = sum(value)), keyby = "quarter"][["value"]]
    )
  }
)

test_that(
  '"byFasttime_m____" works as expected (UTC)',
  {
    skip_if_not_installed("fasttime")
    expect_equal(
      DTSg$new(UTChourlyData)$aggregate(byFasttime_m____, sum)$values(TRUE)[["value"]],
      UTChourlyData[, .(value = sum(value)), keyby = "month"][["value"]]
    )
  }
)

test_that(
  '"byFasttime___H__" works as expected (UTC)',
  {
    skip_if_not_installed("fasttime")
    expect_equal(
      DTSg$new(UTCfractionalSecondData)$aggregate(byFasttime___H__, sum)$values(TRUE)[["value"]],
      UTCfractionalSecondData[, .(value = sum(value)), by = "hour"][["value"]]
    )
  }
)

test_that(
  '"byFasttime____M_" works as expected (UTC)',
  {
    skip_if_not_installed("fasttime")
    expect_equal(
      DTSg$new(UTCfractionalSecondData)$aggregate(byFasttime____M_, sum)$values(TRUE)[["value"]],
      UTCfractionalSecondData[, .(value = sum(value)), by = "hourMinute"][["value"]]
    )
  }
)

test_that(
  '"byFasttime_____S" works as expected (UTC)',
  {
    skip_if_not_installed("fasttime")
    expect_equal(
      DTSg$new(UTCfractionalSecondData)$aggregate(byFasttime_____S, sum)$values(TRUE)[["value"]],
      UTCfractionalSecondData[, .(value = sum(value)), by = "minuteSecond"][["value"]]
    )
  }
)

#### base functions (UTC) ####
context("base functions (UTC)")

test_that(
  '"byY_____" works as expected (UTC)',
  expect_equal(
    DTSg$new(UTChourlyData)$aggregate(byY_____, sum)$values(TRUE)[["value"]],
    UTChourlyData[, .(value = sum(value)), by = "year"][["value"]]
  )
)

test_that(
  '"byYQ____" works as expected (UTC)',
  expect_equal(
    DTSg$new(UTChourlyData)$aggregate(byYQ____, sum)$values(TRUE)[["value"]],
    UTChourlyData[, .(value = sum(value)), by = "quarter"][["value"]]
  )
)

test_that(
  '"byYm____" works as expected (UTC)',
  expect_equal(
    DTSg$new(UTChourlyData)$aggregate(byYm____, sum)$values(TRUE)[["value"]],
    UTChourlyData[, .(value = sum(value)), by = "month"][["value"]]
  )
)

test_that(
  '"byYmd___" works as expected (UTC)',
  expect_equal(
    DTSg$new(UTChourlyData)$aggregate(byYmd___, sum)$values(TRUE)[["value"]],
    UTChourlyData[, .(value = sum(value)), by = "day"][["value"]]
  )
)

test_that(
  '"byYmdH__" works as expected (UTC)',
  expect_equal(
    DTSg$new(UTCfractionalSecondData)$aggregate(byYmdH__, sum)$values(TRUE)[["value"]],
    UTCfractionalSecondData[, .(value = sum(value)), by = "hour"][["value"]]
  )
)

test_that(
  '"byYmdHM_" works as expected (UTC)',
  expect_equal(
    DTSg$new(UTCfractionalSecondData)$aggregate(byYmdHM_, sum)$values(TRUE)[["value"]],
    UTCfractionalSecondData[, .(value = sum(value)), by = "minute"][["value"]]
  )
)

test_that(
  '"byYmdHMS" works as expected (UTC)',
  expect_equal(
    DTSg$new(UTCfractionalSecondData)$aggregate(byYmdHMS, sum)$values(TRUE)[["value"]],
    UTCfractionalSecondData[, .(value = sum(value)), by = "second"][["value"]]
  )
)

test_that(
  '"by______" works as expected (UTC)',
  expect_equal(
    DTSg$new(UTChourlyData)$aggregate(by______, sum)$values(TRUE)[["value"]],
    UTChourlyData[, .(value = sum(value))][["value"]]
  )
)

test_that(
  '"by_Q____" works as expected (UTC)',
  expect_equal(
    DTSg$new(UTChourlyData)$aggregate(by_Q____, sum)$values(TRUE)[["value"]],
    UTChourlyData[, .(value = sum(value)), keyby = "quarter"][["value"]]
  )
)

test_that(
  '"by_m____" works as expected (UTC)',
  expect_equal(
    DTSg$new(UTChourlyData)$aggregate(by_m____, sum)$values(TRUE)[["value"]],
    UTChourlyData[, .(value = sum(value)), keyby = "month"][["value"]]
  )
)

test_that(
  '"by___H__" works as expected (UTC)',
  expect_equal(
    DTSg$new(UTCfractionalSecondData)$aggregate(by___H__, sum)$values(TRUE)[["value"]],
    UTCfractionalSecondData[, .(value = sum(value)), by = "hour"][["value"]]
  )
)

test_that(
  '"by____M_" works as expected (UTC)',
  expect_equal(
    DTSg$new(UTCfractionalSecondData)$aggregate(by____M_, sum)$values(TRUE)[["value"]],
    UTCfractionalSecondData[, .(value = sum(value)), by = "hourMinute"][["value"]]
  )
)

test_that(
  '"by_____S" works as expected (UTC)',
  expect_equal(
    DTSg$new(UTCfractionalSecondData)$aggregate(by_____S, sum)$values(TRUE)[["value"]],
    UTCfractionalSecondData[, .(value = sum(value)), by = "minuteSecond"][["value"]]
  )
)

#### base functions (CET1) ####
context("base functions (CET1)")

test_that(
  '"byY_____" works as expected (CET1)',
  expect_equal(
    DTSg$new(CEThourlyData)$aggregate(byY_____, sum, ignoreDST = TRUE)$values(TRUE)[["value"]],
    CEThourlyData[, .(value = sum(value)), by = "year"][["value"]]
  )
)

test_that(
  '"byYQ____" works as expected (CET1)',
  expect_equal(
    DTSg$new(CEThourlyData)$aggregate(byYQ____, sum, ignoreDST = TRUE)$values(TRUE)[["value"]],
    CEThourlyData[, .(value = sum(value)), by = "quarter"][["value"]]
  )
)

test_that(
  '"byYm____" works as expected (CET1)',
  expect_equal(
    DTSg$new(CEThourlyData)$aggregate(byYm____, sum, ignoreDST = TRUE)$values(TRUE)[["value"]],
    CEThourlyData[, .(value = sum(value)), by = "month"][["value"]]
  )
)

test_that(
  '"byYmd___" works as expected (CET1)',
  expect_equal(
    DTSg$new(CEThourlyData)$aggregate(byYmd___, sum, ignoreDST = TRUE)$values(TRUE)[["value"]],
    CEThourlyData[, .(value = sum(value)), by = "day"][["value"]]
  )
)

test_that(
  '"byYmdH__" works as expected (CET1)',
  expect_equal(
    DTSg$new(CETfractionalSecondData1)$aggregate(byYmdH__, sum)$values(TRUE)[["value"]],
    CETfractionalSecondData1[, .(value = sum(value)), by = "hour"][["value"]]
  )
)

test_that(
  '"byYmdHM_" works as expected (CET1)',
  expect_equal(
    DTSg$new(CETfractionalSecondData1)$aggregate(byYmdHM_, sum)$values(TRUE)[["value"]],
    CETfractionalSecondData1[, .(value = sum(value)), by = "minute"][["value"]]
  )
)

test_that(
  '"byYmdHMS" works as expected (CET1)',
  expect_equal(
    DTSg$new(CETfractionalSecondData1)$aggregate(byYmdHMS, sum)$values(TRUE)[["value"]],
    CETfractionalSecondData1[, .(value = sum(value)), by = "second"][["value"]]
  )
)

test_that(
  '"by______" works as expected (CET1)',
  expect_equal(
    DTSg$new(CEThourlyData)$aggregate(by______, sum)$values(TRUE)[["value"]],
    CEThourlyData[, .(value = sum(value))][["value"]]
  )
)

test_that(
  '"by_Q____" works as expected (CET1)',
  expect_equal(
    DTSg$new(CEThourlyData)$aggregate(by_Q____, sum, ignoreDST = TRUE)$values(TRUE)[["value"]],
    CEThourlyData[, .(value = sum(value)), keyby = "quarter"][["value"]]
  )
)

test_that(
  '"by_m____" works as expected (CET1)',
  expect_equal(
    DTSg$new(CEThourlyData)$aggregate(by_m____, sum, ignoreDST = TRUE)$values(TRUE)[["value"]],
    CEThourlyData[, .(value = sum(value)), keyby = "month"][["value"]]
  )
)

test_that(
  '"by___H__" works as expected (CET1)',
  expect_equal(
    DTSg$new(CETfractionalSecondData1)$aggregate(by___H__, sum, ignoreDST = TRUE)$values(TRUE)[["value"]],
    CETfractionalSecondData1[, .(value = sum(value)), by = "hour"][["value"]]
  )
)

test_that(
  '"by____M_" works as expected (CET1)',
  expect_equal(
    DTSg$new(CETfractionalSecondData1)$aggregate(by____M_, sum)$values(TRUE)[["value"]],
    CETfractionalSecondData1[, .(value = sum(value)), by = "hourMinute"][["value"]]
  )
)

test_that(
  '"by_____S" works as expected (CET1)',
  expect_equal(
    DTSg$new(CETfractionalSecondData1)$aggregate(by_____S, sum)$values(TRUE)[["value"]],
    CETfractionalSecondData1[, .(value = sum(value)), by = "minuteSecond"][["value"]]
  )
)

#### base functions (CET2) ####
context("base functions (CET2)")

test_that(
  '"byYmdH__" works as expected (CET2)',
  expect_equal(
    DTSg$new(CETfractionalSecondData2)$aggregate(byYmdH__, sum)$values(TRUE)[["value"]],
    CETfractionalSecondData2[, .(value = sum(value)), by = "hour"][["value"]]
  )
)

test_that(
  '"byYmdHM_" works as expected (CET2)',
  expect_equal(
    DTSg$new(CETfractionalSecondData2)$aggregate(byYmdHM_, sum)$values(TRUE)[["value"]],
    CETfractionalSecondData2[, .(value = sum(value)), by = "minute"][["value"]]
  )
)

test_that(
  '"byYmdHMS" works as expected (CET2)',
  expect_equal(
    DTSg$new(CETfractionalSecondData2)$aggregate(byYmdHMS, sum)$values(TRUE)[["value"]],
    CETfractionalSecondData2[, .(value = sum(value)), by = "second"][["value"]]
  )
)

test_that(
  '"by___H__" works as expected (CET2)',
  expect_equal(
    DTSg$new(CETfractionalSecondData2)$aggregate(by___H__, sum, ignoreDST = TRUE)$values(TRUE)[["value"]],
    CETfractionalSecondData2[, .(value = sum(value)), by = "hour"][["value"]]
  )
)

test_that(
  '"by____M_" works as expected (CET2)',
  expect_equal(
    DTSg$new(CETfractionalSecondData2)$aggregate(by____M_, sum)$values(TRUE)[["value"]],
    CETfractionalSecondData2[, .(value = sum(value)), by = "hourMinute"][["value"]]
  )
)

test_that(
  '"by_____S" works as expected (CET2)',
  expect_equal(
    DTSg$new(CETfractionalSecondData2)$aggregate(by_____S, sum)$values(TRUE)[["value"]],
    CETfractionalSecondData2[, .(value = sum(value)), by = "minuteSecond"][["value"]]
  )
)
