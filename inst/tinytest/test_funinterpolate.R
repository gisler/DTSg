source("data.R")

#### interpolateLinear ####
expect_identical(
  DTSg$new(DT1)$colapply(interpolateLinear, cols = "col2")$values(TRUE)[["col2"]],
  seq(1, 15, by = 2),
  info = '"interpolateLinear" works correctly'
)
