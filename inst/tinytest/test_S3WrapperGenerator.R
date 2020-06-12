expect_error(
  S3WrapperGenerator(DTSg$private_fields$.na.status),
  info = "not a function or expression returns error"
)

expect_error(
  S3WrapperGenerator(DTSg$private_methods$aggregate),
  info = "not a public method returns error"
)

expect_error(
  S3WrapperGenerator(expression(noDTSg$public_methods$aggregate)),
  info = 'not an "R6ClassGenerator" returns error'
)
