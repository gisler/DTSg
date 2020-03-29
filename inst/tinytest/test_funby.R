source("data.R")

#### fasttime functions (UTC) ####
if (requireNamespace("fasttime", quietly = TRUE)) {
  expect_equal(
    DTSg$new(UTChourlyData)$aggregate(byFasttimeY_____, sum)$values(TRUE)[["value"]],
    UTChourlyData[, .(value = sum(value)), by = "year"][["value"]],
    info = '"byFasttimeY_____" works as expected (UTC)'
  )

  expect_equal(
    DTSg$new(UTChourlyData)$aggregate(byFasttimeYQ____, sum)$values(TRUE)[["value"]],
    UTChourlyData[, .(value = sum(value)), by = "quarter"][["value"]],
    info = '"byFasttimeYQ____" works as expected (UTC)'
  )

  expect_equal(
    DTSg$new(UTChourlyData)$aggregate(byFasttimeYm____, sum)$values(TRUE)[["value"]],
    UTChourlyData[, .(value = sum(value)), by = "month"][["value"]],
    info = '"byFasttimeYm____" works as expected (UTC)'
  )

  expect_equal(
    DTSg$new(UTChourlyData)$aggregate(byFasttimeYmd___, sum)$values(TRUE)[["value"]],
    UTChourlyData[, .(value = sum(value)), by = "day"][["value"]],
    info = '"byFasttimeYmd___" works as expected (UTC)'
  )

  expect_equal(
    DTSg$new(UTCfractionalSecondData)$aggregate(byFasttimeYmdH__, sum)$values(TRUE)[["value"]],
    UTCfractionalSecondData[, .(value = sum(value)), by = "hour"][["value"]],
    info = '"byFasttimeYmdH__" works as expected (UTC)'
  )

  expect_equal(
    DTSg$new(UTCfractionalSecondData)$aggregate(byFasttimeYmdHM_, sum)$values(TRUE)[["value"]],
    UTCfractionalSecondData[, .(value = sum(value)), by = "minute"][["value"]],
    info = '"byFasttimeYmdHM_" works as expected (UTC)'
  )

  expect_equal(
    DTSg$new(UTCfractionalSecondData)$aggregate(byFasttimeYmdHMS, sum)$values(TRUE)[["value"]],
    UTCfractionalSecondData[, .(value = sum(value)), by = "second"][["value"]],
    info = '"byFasttimeYmdHMS" works as expected (UTC)'
  )

  expect_equal(
    DTSg$new(UTChourlyData)$aggregate(byFasttime______, sum)$values(TRUE)[["value"]],
    UTChourlyData[, .(value = sum(value))][["value"]],
    info = '"byFasttime______" works as expected (UTC)'
  )

  expect_equal(
    DTSg$new(UTChourlyData)$aggregate(byFasttime_Q____, sum)$values(TRUE)[["value"]],
    UTChourlyData[, .(value = sum(value)), keyby = "quarter"][["value"]],
    info = '"byFasttime_Q____" works as expected (UTC)'
  )

  expect_equal(
    DTSg$new(UTChourlyData)$aggregate(byFasttime_m____, sum)$values(TRUE)[["value"]],
    UTChourlyData[, .(value = sum(value)), keyby = "month"][["value"]],
    info = '"byFasttime_m____" works as expected (UTC)'
  )

  expect_equal(
    DTSg$new(UTCfractionalSecondData)$aggregate(byFasttime___H__, sum)$values(TRUE)[["value"]],
    UTCfractionalSecondData[, .(value = sum(value)), by = "hour"][["value"]],
    info = '"byFasttime___H__" works as expected (UTC)'
  )

  expect_equal(
    DTSg$new(UTCfractionalSecondData)$aggregate(byFasttime____M_, sum)$values(TRUE)[["value"]],
    UTCfractionalSecondData[, .(value = sum(value)), by = "hourMinute"][["value"]],
    info = '"byFasttime____M_" works as expected (UTC)'
  )

  expect_equal(
    DTSg$new(UTCfractionalSecondData)$aggregate(byFasttime_____S, sum)$values(TRUE)[["value"]],
    UTCfractionalSecondData[, .(value = sum(value)), by = "minuteSecond"][["value"]],
    info = '"byFasttime_____S" works as expected (UTC)'
  )
}

#### base functions (UTC) ####
expect_equal(
  DTSg$new(UTChourlyData)$aggregate(byY_____, sum)$values(TRUE)[["value"]],
  UTChourlyData[, .(value = sum(value)), by = "year"][["value"]],
  info = '"byY_____" works as expected (UTC)'
)

expect_equal(
  DTSg$new(UTChourlyData)$aggregate(byYQ____, sum)$values(TRUE)[["value"]],
  UTChourlyData[, .(value = sum(value)), by = "quarter"][["value"]],
  info = '"byYQ____" works as expected (UTC)'
)

expect_equal(
  DTSg$new(UTChourlyData)$aggregate(byYm____, sum)$values(TRUE)[["value"]],
  UTChourlyData[, .(value = sum(value)), by = "month"][["value"]],
  info = '"byYm____" works as expected (UTC)'
)

expect_equal(
  DTSg$new(UTChourlyData)$aggregate(byYmd___, sum)$values(TRUE)[["value"]],
  UTChourlyData[, .(value = sum(value)), by = "day"][["value"]],
  info = '"byYmd___" works as expected (UTC)'
)

expect_equal(
  DTSg$new(UTCfractionalSecondData)$aggregate(byYmdH__, sum)$values(TRUE)[["value"]],
  UTCfractionalSecondData[, .(value = sum(value)), by = "hour"][["value"]],
  info = '"byYmdH__" works as expected (UTC)'
)

expect_equal(
  DTSg$new(UTCfractionalSecondData)$aggregate(byYmdHM_, sum)$values(TRUE)[["value"]],
  UTCfractionalSecondData[, .(value = sum(value)), by = "minute"][["value"]],
  info = '"byYmdHM_" works as expected (UTC)'
)

expect_equal(
  DTSg$new(UTCfractionalSecondData)$aggregate(byYmdHMS, sum)$values(TRUE)[["value"]],
  UTCfractionalSecondData[, .(value = sum(value)), by = "second"][["value"]],
  info = '"byYmdHMS" works as expected (UTC)'
)

expect_equal(
  DTSg$new(UTChourlyData)$aggregate(by______, sum)$values(TRUE)[["value"]],
  UTChourlyData[, .(value = sum(value))][["value"]],
  info = '"by______" works as expected (UTC)'
)

expect_equal(
  DTSg$new(UTChourlyData)$aggregate(by_Q____, sum)$values(TRUE)[["value"]],
  UTChourlyData[, .(value = sum(value)), keyby = "quarter"][["value"]],
  info = '"by_Q____" works as expected (UTC)'
)

expect_equal(
  DTSg$new(UTChourlyData)$aggregate(by_m____, sum)$values(TRUE)[["value"]],
  UTChourlyData[, .(value = sum(value)), keyby = "month"][["value"]],
  info = '"by_m____" works as expected (UTC)'
)

expect_equal(
  DTSg$new(UTCfractionalSecondData)$aggregate(by___H__, sum)$values(TRUE)[["value"]],
  UTCfractionalSecondData[, .(value = sum(value)), by = "hour"][["value"]],
  info = '"by___H__" works as expected (UTC)'
)

expect_equal(
  DTSg$new(UTCfractionalSecondData)$aggregate(by____M_, sum)$values(TRUE)[["value"]],
  UTCfractionalSecondData[, .(value = sum(value)), by = "hourMinute"][["value"]],
  info = '"by____M_" works as expected (UTC)'
)

expect_equal(
  DTSg$new(UTCfractionalSecondData)$aggregate(by_____S, sum)$values(TRUE)[["value"]],
  UTCfractionalSecondData[, .(value = sum(value)), by = "minuteSecond"][["value"]],
  info = '"by_____S" works as expected (UTC)'
)

#### base functions (CETtoFromDST) ####
expect_equal(
  DTSg$new(CEThourlyData)$aggregate(byY_____, sum, ignoreDST = TRUE)$values(TRUE)[["value"]],
  CEThourlyData[, .(value = sum(value)), by = "year"][["value"]],
  info = '"byY_____" works as expected (CETtoFromDST)'
)

expect_equal(
  DTSg$new(CEThourlyData)$aggregate(byYQ____, sum, ignoreDST = TRUE)$values(TRUE)[["value"]],
  CEThourlyData[, .(value = sum(value)), by = "quarter"][["value"]],
  info = '"byYQ____" works as expected (CETtoFromDST)'
)

expect_equal(
  DTSg$new(CEThourlyData)$aggregate(byYm____, sum, ignoreDST = TRUE)$values(TRUE)[["value"]],
  CEThourlyData[, .(value = sum(value)), by = "month"][["value"]],
  info = '"byYm____" works as expected (CETtoFromDST)'
)

expect_equal(
  DTSg$new(CEThourlyData)$aggregate(byYmd___, sum, ignoreDST = TRUE)$values(TRUE)[["value"]],
  CEThourlyData[, .(value = sum(value)), by = "day"][["value"]],
  info = '"byYmd___" works as expected (CETtoFromDST)'
)

expect_equal(
  DTSg$new(CETtoDSTfractionalSecondData)$aggregate(byYmdH__, sum)$values(TRUE)[["value"]],
  CETtoDSTfractionalSecondData[, .(value = sum(value)), by = "hour"][["value"]],
  info = '"byYmdH__" works as expected (CETtoDST)'
)

expect_equal(
  DTSg$new(CETtoDSTfractionalSecondData)$aggregate(byYmdHM_, sum)$values(TRUE)[["value"]],
  CETtoDSTfractionalSecondData[, .(value = sum(value)), by = "minute"][["value"]],
  info = '"byYmdHM_" works as expected (CETtoDST)'
)

expect_equal(
  DTSg$new(CETtoDSTfractionalSecondData)$aggregate(byYmdHMS, sum)$values(TRUE)[["value"]],
  CETtoDSTfractionalSecondData[, .(value = sum(value)), by = "second"][["value"]],
  info = '"byYmdHMS" works as expected (CETtoDST)'
)

expect_equal(
  DTSg$new(CEThourlyData)$aggregate(by______, sum)$values(TRUE)[["value"]],
  CEThourlyData[, .(value = sum(value))][["value"]],
  info = '"by______" works as expected (CETtoFromDST)'
)

expect_equal(
  DTSg$new(CEThourlyData)$aggregate(by_Q____, sum, ignoreDST = TRUE)$values(TRUE)[["value"]],
  CEThourlyData[, .(value = sum(value)), keyby = "quarter"][["value"]],
  info = '"by_Q____" works as expected (CETtoFromDST)'
)

expect_equal(
  DTSg$new(CEThourlyData)$aggregate(by_m____, sum, ignoreDST = TRUE)$values(TRUE)[["value"]],
  CEThourlyData[, .(value = sum(value)), keyby = "month"][["value"]],
  info = '"by_m____" works as expected (CETtoFromDST)'
)

expect_equal(
  DTSg$new(CETtoDSTfractionalSecondData)$aggregate(by___H__, sum, ignoreDST = TRUE)$values(TRUE)[["value"]],
  CETtoDSTfractionalSecondData[, .(value = sum(value)), by = "hour"][["value"]],
  info = '"by___H__" works as expected (CETtoDST)'
)

expect_equal(
  DTSg$new(CETtoDSTfractionalSecondData)$aggregate(by____M_, sum)$values(TRUE)[["value"]],
  CETtoDSTfractionalSecondData[, .(value = sum(value)), by = "hourMinute"][["value"]],
  info = '"by____M_" works as expected (CETtoDST)'
)

expect_equal(
  DTSg$new(CETtoDSTfractionalSecondData)$aggregate(by_____S, sum)$values(TRUE)[["value"]],
  CETtoDSTfractionalSecondData[, .(value = sum(value)), by = "minuteSecond"][["value"]],
  info = '"by_____S" works as expected (CETtoDST)'
)

#### base functions (CETfromDST) ####
expect_equal(
  DTSg$new(CETfromDSTfractionalSecondData)$aggregate(byYmdH__, sum)$values(TRUE)[["value"]],
  CETfromDSTfractionalSecondData[, .(value = sum(value)), by = "hour"][["value"]],
  info = '"byYmdH__" works as expected (CETfromDST)'
)

expect_equal(
  DTSg$new(CETfromDSTfractionalSecondData)$aggregate(byYmdHM_, sum)$values(TRUE)[["value"]],
  CETfromDSTfractionalSecondData[, .(value = sum(value)), by = "minute"][["value"]],
  info = '"byYmdHM_" works as expected (CETfromDST)'
)

expect_equal(
  DTSg$new(CETfromDSTfractionalSecondData)$aggregate(byYmdHMS, sum)$values(TRUE)[["value"]],
  CETfromDSTfractionalSecondData[, .(value = sum(value)), by = "second"][["value"]],
  info = '"byYmdHMS" works as expected (CETfromDST)'
)

expect_equal(
  DTSg$new(CETfromDSTfractionalSecondData)$aggregate(by___H__, sum, ignoreDST = TRUE)$values(TRUE)[["value"]],
  CETfromDSTfractionalSecondData[, .(value = sum(value)), by = "hour"][["value"]],
  info = '"by___H__" works as expected (CETfromDST)'
)

expect_equal(
  DTSg$new(CETfromDSTfractionalSecondData)$aggregate(by____M_, sum)$values(TRUE)[["value"]],
  CETfromDSTfractionalSecondData[, .(value = sum(value)), by = "hourMinute"][["value"]],
  info = '"by____M_" works as expected (CETfromDST)'
)

expect_equal(
  DTSg$new(CETfromDSTfractionalSecondData)$aggregate(by_____S, sum)$values(TRUE)[["value"]],
  CETfromDSTfractionalSecondData[, .(value = sum(value)), by = "minuteSecond"][["value"]],
  info = '"by_____S" works as expected (CETfromDST)'
)