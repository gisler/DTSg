#### S3WrapperGenerator ####
context("S3WrapperGenerator")

test_that(
  "not an expression object returns error",
  expect_error(
    S3WrapperGenerator(DTSg$public_methods$aggregate)
  )
)

test_that(
  "not a public method returns error",
  expect_error(
    S3WrapperGenerator(expression(DTSg$private_methods$aggregate))
  )
)

test_that(
  'not an "R6ClassGenerator" returns error',
  expect_error(
    S3WrapperGenerator(expression(noDTSg$public_methods$aggregate))
  )
)
