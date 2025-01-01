expect_error(
  S3WrapperGenerator(DTSg$private_fields$.na.status),
  pattern = '"R6Method" must contain a public method of an "R6ClassGenerator".',
  fixed = TRUE,
  info = "not a function or expression returns error"
)

expect_error(
  S3WrapperGenerator(DTSg$private_methods$funApply),
  pattern = '"R6Method" must contain a public method of an "R6ClassGenerator".',
  fixed = TRUE,
  info = "not a public method returns error"
)
