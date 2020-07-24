source("data.R")

#### fasttime functions (UTC) ####
if (requireNamespace("fasttime", quietly = TRUE)) {
  expect_identical(
    DTSg$new(UTChourlyData)$aggregate(byFasttimeY_____, length)$values(TRUE)[["value"]],
    UTChourlyData[, .(value = length(value)), by = "year"][["value"]],
    info = '"byFasttimeY_____" works as expected (UTC)'
  )

  expect_identical(
    DTSg$new(UTChourlyData)$aggregate(byFasttimeYQ____, length)$values(TRUE)[["value"]],
    UTChourlyData[, .(value = length(value)), by = "quarter"][["value"]],
    info = '"byFasttimeYQ____" works as expected (UTC)'
  )

  expect_identical(
    DTSg$new(UTChourlyData)$aggregate(byFasttimeYm____, length)$values(TRUE)[["value"]],
    UTChourlyData[, .(value = length(value)), by = "month"][["value"]],
    info = '"byFasttimeYm____" works as expected (UTC)'
  )

  expect_identical(
    DTSg$new(UTChourlyData)$aggregate(byFasttimeYmd___, length)$values(TRUE)[["value"]],
    UTChourlyData[, .(value = length(value)), by = "day"][["value"]],
    info = '"byFasttimeYmd___" works as expected (UTC)'
  )

  expect_identical(
    DTSg$new(UTCfractionalSecondData)$aggregate(byFasttimeYmdH__, length)$values(TRUE)[["value"]],
    UTCfractionalSecondData[, .(value = length(value)), by = "hour"][["value"]],
    info = '"byFasttimeYmdH__" works as expected (UTC)'
  )

  expect_identical(
    DTSg$new(UTCfractionalSecondData)$aggregate(byFasttimeYmdHM_, length)$values(TRUE)[["value"]],
    UTCfractionalSecondData[, .(value = length(value)), by = "minute"][["value"]],
    info = '"byFasttimeYmdHM_" works as expected (UTC)'
  )

  expect_identical(
    DTSg$new(UTCfractionalSecondData)$aggregate(byFasttimeYmdHMS, length)$values(TRUE)[["value"]],
    UTCfractionalSecondData[, .(value = length(value)), by = "second"][["value"]],
    info = '"byFasttimeYmdHMS" works as expected (UTC)'
  )

  expect_identical(
    DTSg$new(UTChourlyData)$aggregate(byFasttime______, length)$values(TRUE)[["value"]],
    UTChourlyData[, .(value = length(value))][["value"]],
    info = '"byFasttime______" works as expected (UTC)'
  )

  expect_identical(
    DTSg$new(UTChourlyData)$aggregate(byFasttime_Q____, length)$values(TRUE)[["value"]],
    UTChourlyData[, .(value = length(value)), keyby = "quarter"][["value"]],
    info = '"byFasttime_Q____" works as expected (UTC)'
  )

  expect_identical(
    DTSg$new(UTChourlyData)$aggregate(byFasttime_m____, length)$values(TRUE)[["value"]],
    UTChourlyData[, .(value = length(value)), keyby = "month"][["value"]],
    info = '"byFasttime_m____" works as expected (UTC)'
  )

  expect_identical(
    DTSg$new(UTCfractionalSecondData)$aggregate(byFasttime___H__, length)$values(TRUE)[["value"]],
    UTCfractionalSecondData[, .(value = length(value)), by = "hour"][["value"]],
    info = '"byFasttime___H__" works as expected (UTC)'
  )

  expect_identical(
    DTSg$new(UTCfractionalSecondData)$aggregate(byFasttime____M_, length)$values(TRUE)[["value"]],
    UTCfractionalSecondData[, .(value = length(value)), by = "hourMinute"][["value"]],
    info = '"byFasttime____M_" works as expected (UTC)'
  )

  expect_identical(
    DTSg$new(UTCfractionalSecondData)$aggregate(byFasttime_____S, length)$values(TRUE)[["value"]],
    UTCfractionalSecondData[, .(value = length(value)), by = "minuteSecond"][["value"]],
    info = '"byFasttime_____S" works as expected (UTC)'
  )
}

#### base functions (UTC) ####
expect_identical(
  DTSg$new(UTChourlyData)$aggregate(byY_____, length)$values(TRUE)[["value"]],
  UTChourlyData[, .(value = length(value)), by = "year"][["value"]],
  info = '"byY_____" works as expected (UTC)'
)

expect_identical(
  DTSg$new(UTChourlyData)$aggregate(byYQ____, length)$values(TRUE)[["value"]],
  UTChourlyData[, .(value = length(value)), by = "quarter"][["value"]],
  info = '"byYQ____" works as expected (UTC)'
)

expect_identical(
  DTSg$new(UTChourlyData)$aggregate(byYm____, length)$values(TRUE)[["value"]],
  UTChourlyData[, .(value = length(value)), by = "month"][["value"]],
  info = '"byYm____" works as expected (UTC)'
)

expect_identical(
  DTSg$new(UTChourlyData)$aggregate(byYmd___, length)$values(TRUE)[["value"]],
  UTChourlyData[, .(value = length(value)), by = "day"][["value"]],
  info = '"byYmd___" works as expected (UTC)'
)

expect_identical(
  DTSg$new(UTCfractionalSecondData)$aggregate(byYmdH__, length)$values(TRUE)[["value"]],
  UTCfractionalSecondData[, .(value = length(value)), by = "hour"][["value"]],
  info = '"byYmdH__" works as expected (UTC)'
)

expect_identical(
  DTSg$new(UTCfractionalSecondData)$aggregate(byYmdHM_, length)$values(TRUE)[["value"]],
  UTCfractionalSecondData[, .(value = length(value)), by = "minute"][["value"]],
  info = '"byYmdHM_" works as expected (UTC)'
)

expect_identical(
  DTSg$new(UTCfractionalSecondData)$aggregate(byYmdHMS, length)$values(TRUE)[["value"]],
  UTCfractionalSecondData[, .(value = length(value)), by = "second"][["value"]],
  info = '"byYmdHMS" works as expected (UTC)'
)

expect_identical(
  DTSg$new(UTChourlyData)$aggregate(by______, length)$values(TRUE)[["value"]],
  UTChourlyData[, .(value = length(value))][["value"]],
  info = '"by______" works as expected (UTC)'
)

expect_identical(
  DTSg$new(UTChourlyData)$aggregate(by_Q____, length)$values(TRUE)[["value"]],
  UTChourlyData[, .(value = length(value)), keyby = "quarter"][["value"]],
  info = '"by_Q____" works as expected (UTC)'
)

expect_identical(
  DTSg$new(UTChourlyData)$aggregate(by_m____, length)$values(TRUE)[["value"]],
  UTChourlyData[, .(value = length(value)), keyby = "month"][["value"]],
  info = '"by_m____" works as expected (UTC)'
)

expect_identical(
  DTSg$new(UTCfractionalSecondData)$aggregate(by___H__, length)$values(TRUE)[["value"]],
  UTCfractionalSecondData[, .(value = length(value)), by = "hour"][["value"]],
  info = '"by___H__" works as expected (UTC)'
)

expect_identical(
  DTSg$new(UTCfractionalSecondData)$aggregate(by____M_, length)$values(TRUE)[["value"]],
  UTCfractionalSecondData[, .(value = length(value)), by = "hourMinute"][["value"]],
  info = '"by____M_" works as expected (UTC)'
)

expect_identical(
  DTSg$new(UTCfractionalSecondData)$aggregate(by_____S, length)$values(TRUE)[["value"]],
  UTCfractionalSecondData[, .(value = length(value)), by = "minuteSecond"][["value"]],
  info = '"by_____S" works as expected (UTC)'
)

#### base functions (CETtoFromDST) ####
expect_identical(
  DTSg$new(CEThourlyData)$aggregate(byY_____, length, ignoreDST = TRUE)$values(TRUE)[["value"]],
  CEThourlyData[, .(value = length(value)), by = "year"][["value"]],
  info = '"byY_____" works as expected (CETtoFromDST)'
)

expect_identical(
  DTSg$new(CEThourlyData)$aggregate(byYQ____, length, ignoreDST = TRUE)$values(TRUE)[["value"]],
  CEThourlyData[, .(value = length(value)), by = "quarter"][["value"]],
  info = '"byYQ____" works as expected (CETtoFromDST)'
)

expect_identical(
  DTSg$new(CEThourlyData)$aggregate(byYm____, length, ignoreDST = TRUE)$values(TRUE)[["value"]],
  CEThourlyData[, .(value = length(value)), by = "month"][["value"]],
  info = '"byYm____" works as expected (CETtoFromDST)'
)

expect_identical(
  DTSg$new(CEThourlyData)$aggregate(byYmd___, length, ignoreDST = TRUE)$values(TRUE)[["value"]],
  CEThourlyData[, .(value = length(value)), by = "day"][["value"]],
  info = '"byYmd___" works as expected (CETtoFromDST)'
)

expect_identical(
  DTSg$new(CETtoDSTfractionalSecondData)$aggregate(byYmdH__, length)$values(TRUE)[["value"]],
  CETtoDSTfractionalSecondData[, .(value = length(value)), by = "hour"][["value"]],
  info = '"byYmdH__" works as expected (CETtoDST)'
)

expect_identical(
  DTSg$new(CETtoDSTfractionalSecondData)$aggregate(byYmdHM_, length)$values(TRUE)[["value"]],
  CETtoDSTfractionalSecondData[, .(value = length(value)), by = "minute"][["value"]],
  info = '"byYmdHM_" works as expected (CETtoDST)'
)

expect_identical(
  DTSg$new(CETtoDSTfractionalSecondData)$aggregate(byYmdHMS, length)$values(TRUE)[["value"]],
  CETtoDSTfractionalSecondData[, .(value = length(value)), by = "second"][["value"]],
  info = '"byYmdHMS" works as expected (CETtoDST)'
)

expect_identical(
  DTSg$new(CEThourlyData)$aggregate(by______, length)$values(TRUE)[["value"]],
  CEThourlyData[, .(value = length(value))][["value"]],
  info = '"by______" works as expected (CETtoFromDST)'
)

expect_identical(
  DTSg$new(CEThourlyData)$aggregate(by_Q____, length, ignoreDST = TRUE)$values(TRUE)[["value"]],
  CEThourlyData[, .(value = length(value)), keyby = "quarter"][["value"]],
  info = '"by_Q____" works as expected (CETtoFromDST)'
)

expect_identical(
  DTSg$new(CEThourlyData)$aggregate(by_m____, length, ignoreDST = TRUE)$values(TRUE)[["value"]],
  CEThourlyData[, .(value = length(value)), keyby = "month"][["value"]],
  info = '"by_m____" works as expected (CETtoFromDST)'
)

expect_identical(
  DTSg$new(CETtoDSTfractionalSecondData)$aggregate(by___H__, length, ignoreDST = TRUE)$values(TRUE)[["value"]],
  CETtoDSTfractionalSecondData[, .(value = length(value)), by = "hour"][["value"]],
  info = '"by___H__" works as expected (CETtoDST)'
)

expect_identical(
  DTSg$new(CETtoDSTfractionalSecondData)$aggregate(by____M_, length)$values(TRUE)[["value"]],
  CETtoDSTfractionalSecondData[, .(value = length(value)), by = "hourMinute"][["value"]],
  info = '"by____M_" works as expected (CETtoDST)'
)

expect_identical(
  DTSg$new(CETtoDSTfractionalSecondData)$aggregate(by_____S, length)$values(TRUE)[["value"]],
  CETtoDSTfractionalSecondData[, .(value = length(value)), by = "minuteSecond"][["value"]],
  info = '"by_____S" works as expected (CETtoDST)'
)

#### base functions (CETfromDST) ####
expect_identical(
  DTSg$new(CETfromDSTfractionalSecondData)$aggregate(byYmdH__, length)$values(TRUE)[["value"]],
  CETfromDSTfractionalSecondData[, .(value = length(value)), by = "hour"][["value"]],
  info = '"byYmdH__" works as expected (CETfromDST)'
)

expect_identical(
  DTSg$new(CETfromDSTfractionalSecondData)$aggregate(byYmdHM_, length)$values(TRUE)[["value"]],
  CETfromDSTfractionalSecondData[, .(value = length(value)), by = "minute"][["value"]],
  info = '"byYmdHM_" works as expected (CETfromDST)'
)

expect_identical(
  DTSg$new(CETfromDSTfractionalSecondData)$aggregate(byYmdHMS, length)$values(TRUE)[["value"]],
  CETfromDSTfractionalSecondData[, .(value = length(value)), by = "second"][["value"]],
  info = '"byYmdHMS" works as expected (CETfromDST)'
)

expect_identical(
  DTSg$new(CETfromDSTfractionalSecondData)$aggregate(by___H__, length, ignoreDST = TRUE)$values(TRUE)[["value"]],
  CETfromDSTfractionalSecondData[, .(value = length(value)), by = "hour"][["value"]],
  info = '"by___H__" works as expected (CETfromDST)'
)

expect_identical(
  DTSg$new(CETfromDSTfractionalSecondData)$aggregate(by____M_, length)$values(TRUE)[["value"]],
  CETfromDSTfractionalSecondData[, .(value = length(value)), by = "hourMinute"][["value"]],
  info = '"by____M_" works as expected (CETfromDST)'
)

expect_identical(
  DTSg$new(CETfromDSTfractionalSecondData)$aggregate(by_____S, length)$values(TRUE)[["value"]],
  CETfromDSTfractionalSecondData[, .(value = length(value)), by = "minuteSecond"][["value"]],
  info = '"by_____S" works as expected (CETfromDST)'
)
