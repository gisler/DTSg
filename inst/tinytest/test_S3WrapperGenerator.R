expect_error(
  S3WrapperGenerator(DTSg$public_methods$aggregate),
  info = "not an expression object returns error"
)

expect_error(
  S3WrapperGenerator(expression(DTSg$private_methods$aggregate)),
  info = "not a public method returns error"
)

expect_error(
  S3WrapperGenerator(expression(noDTSg$public_methods$aggregate)),
  info = 'not an "R6ClassGenerator" returns error'
)
