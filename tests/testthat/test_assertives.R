#### assert_are_newCols_and_cols_not_intersecting_sets ####
context("assert_are_newCols_and_cols_not_intersecting_sets")

test_that(
  'intersecting "newCols" and "cols" returns error',
  expect_error(
    assert_are_newCols_and_cols_not_intersecting_sets(
      c("col1", "col2"),
      c("col2", "col3")
    )
  )
)

#### assert_is_fasttime_ok ####
context("assert_is_fasttime_ok")

test_that(
  'unsuitable ".dateTime" returns error',
  expect_error(
    assert_is_fasttime_ok(
      seq(
        as.POSIXct("1960-01-01"),
        as.POSIXct("2209-12-31"),
        "1 year"
      ),
      list(timezone = "UTC")
    )
  )
)

test_that(
  'unsuitable "timezone" returns error',
  expect_error(
    assert_is_fasttime_ok(
      seq(
        as.POSIXct("1970-01-01"),
        as.POSIXct("2199-12-31"),
        "1 year"
      ),
      list(timezone = "Europe/Vienna")
    )
  )
)

#### assert_is_length_cols_greater_than_or_equal_to_one ####
context("assert_is_length_cols_greater_than_or_equal_to_one")

test_that(
  'empty "cols" returns error',
  expect_error(
    assert_is_length_cols_greater_than_or_equal_to_one(character())
  )
)

#### assert_is_periodicity_recognised ####
context("assert_is_periodicity_recognised")

test_that(
  'unrecognised "periodicity" returns error',
  expect_error(
    assert_is_periodicity_recognised("unrecognised")
  )
)

#### assert_all_have_no_beginning_dot ####
context("assert_all_have_no_beginning_dot")

test_that(
  'column name with beginning dot returns error',
  expect_error(
    assert_all_have_no_beginning_dot(c("col1", ".col2", "col3"))
  )
)
