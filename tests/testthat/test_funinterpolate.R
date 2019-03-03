context("funinterpolate")

test_that(
  '"interpolateLinear" works correctly',
  expect_identical(
    DTSg$new(DT1)$colapply(interpolateLinear, cols = "col2")$values()[["col2"]],
    seq(1, 15, by = 2)
  )
)
