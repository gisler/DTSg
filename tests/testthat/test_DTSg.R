#### aggregate method ####
context("aggregate method")

test_that(
  'wrong "funby" value class returns error',
  expect_error(
    DTSg$new(DT1)$aggregate(function(.dateTime, .helpers) {"timestamp"}, mean)
  )
)

test_that(
  "values are aggregated correctly",
  expect_identical(
    DTSg$new(DT1)$aggregate(byYmdH__, mean)$values(),
    data.table::data.table(
      date = seq(
        as.POSIXct("2000-10-29 01:00:00", tz = "Europe/Vienna"),
        as.POSIXct("2000-10-29 03:30:00", tz = "Europe/Vienna"),
        "1 hour"
      ),
      col1 = c( 2, 6, 10, 14),
      col2 = c(NA, 6, 10, 14),
      key = "date"
    )
  )
)

test_that(
  '"..." passes on arguments correctly',
  expect_identical(
    DTSg$new(DT1)$aggregate(byYmdH__, mean, na.rm = TRUE)$values()[["col2"]],
    c(1, 6, 10, 14)
  )
)

test_that(
  '"aggregated" field is set correctly',
  expect_true(
    DTSg$new(DT1)$aggregate(byYmdH__, mean)$aggregated
  )
)

test_that(
  ".n is correct (single column)",
  expect_identical(
    DTSg$new(DT1)$aggregate(byYmdH__, mean, cols = "col2", n = TRUE)$values()[[".n"]],
    c(1L, 2L, 2L, 2L)
  )
)

test_that(
  ".n is correct (multiple columns)",
  expect_identical(
    DTSg$new(DT1)$aggregate(byYmdH__, mean, n = TRUE)$values()[[".n"]],
    rep(2L, 4L)
  )
)

#### alter method ####
context("alter method")

test_that(
  '"unrecognised" periodicity returns error',
  expect_error(
    DTSg$new(DT1)$alter(by = "unrecognised")
  )
)

test_that(
  "values are altered correctly",
  expect_identical(
    DTSg$new(DT1[-3L, ])$alter("2000-10-29 01:00:00", "2000-10-29 03:00:00", "1 hour")$values(),
    data.table::data.table(
      date = seq(
        as.POSIXct("2000-10-29 01:00:00", tz = "Europe/Vienna"),
        as.POSIXct("2000-10-29 03:00:00", tz = "Europe/Vienna"),
        "1 hour"
      ),
      col1 = c(  1, NA,   9,  13),
      col2 = c(  1, NA,   9,  13),
      col3 = c("A", NA, "E", "G"),
      key = "date"
    )
  )
)

test_that(
  'call to "rollback" is made',
  expect_identical(
    DTSg$new(DT1[-3L, ])$alter("2000-01-31", "2000-07-01", "1 month")$values()[["date"]],
    as.POSIXct(
      c("2000-01-31", "2000-02-29", "2000-03-31", "2000-04-30", "2000-05-31", "2000-06-30"),
      tz = "Europe/Vienna"
    )
  )
)

#### clone method ####
context("clone method")

test_that(
  "deep clone is created by default",
  expect_identical(
    {
      TS <- DTSg$new(DT1)
      TS$aggregate(byYmdH__, mean)
      TS$alter(by = "1 hour")
      TS$colapply(function(x, ...) {as.factor(x)})
      TS$merge(DT2)
      TS$rollapply(weighted.mean, na.rm = TRUE)
      TS
    },
    TS
  )
)

#### colapply method ####
context("colapply method")

test_that(
  '"fun" is applied correctly',
  expect_identical(
    DTSg$new(DT1)$colapply(function(x, ...) {as.factor(x)}, cols = c("col1", "col2"))$cols("factor"),
    c("col1", "col2")
  )
)

test_that(
  '"..." passes on arguments correctly',
  expect_identical(
    DTSg$new(DT1[-1L, ])$colapply(
      interpolateLinear,
      rollends = FALSE,
      cols = "col2"
    )$values()[["col2"]],
    c(NA, seq(5, 15, by = 2))
  )
)

#### cols method ####
context("cols method")

test_that(
  "all column names are returned",
  expect_identical(
    DTSg$new(DT1)$cols(),
    c("col1", "col2", "col3")
  )
)

test_that(
  "character column name is returned",
  expect_identical(
    DTSg$new(DT1)$cols("character"),
    "col3"
  )
)

test_that(
  "no column name is returned",
  expect_identical(
    DTSg$new(DT1)$cols("integer"),
    character()
  )
)

#### initialize method ####
context("initialize method")

test_that(
  "data.table with a single column only returns error",
  expect_error(
    DTSg$new(DT1[, .(date)])
  )
)

test_that(
  'data.table with a "." in a column name returns error',
  expect_error(
    DTSg$new(DT1[, .(date, .col1 = col1)])
  )
)

test_that(
  "data.table with duplicated column names returns error",
  expect_error(
    DTSg$new(DT1[, .(date, col1, col1)])
  )
)

test_that(
  '"ID" field is set correctly',
  expect_identical(
    DTSg$new(DT1, ID = "ID")$ID,
    "ID"
  )
)

test_that(
  '"parameter" field is set correctly',
  expect_identical(
    DTSg$new(DT1, parameter = "parameter")$parameter,
    "parameter"
  )
)

test_that(
  '"unit" field is set correctly',
  expect_identical(
    DTSg$new(DT1, unit = "unit")$unit,
    "unit"
  )
)

test_that(
  '"variant" field is set correctly',
  expect_identical(
    DTSg$new(DT1, variant = "variant")$variant,
    "variant"
  )
)

test_that(
  '"aggregated" field is set correctly',
  expect_true(
    DTSg$new(DT1, aggregated = TRUE)$aggregated
  )
)

test_that(
  '"fast" field is set correctly',
  expect_true(
    DTSg$new(DT1, fast = TRUE)$fast
  )
)

#### merge method ####
context("merge method")

test_that(
  "use of arguments not allowed returns error",
  expect_error(
    DTSg$new(DT1[, .(date, col1)])$merge(DT2, by = "date")
  )
)

test_that(
  "values are merged correctly",
  expect_identical(
    DTSg$new(DT1[, .(date, col1)])$merge(DT2)$values(),
    merge(DT1[, .(date, col1)], DT2, by = "date")
  )
)

test_that(
  '"..." passes on arguments correctly',
  expect_identical(
    DTSg$new(DT1[, .(date, col1)])$merge(DT2, all = TRUE)$values(),
    merge(DT1[, .(date, col1)], DT2, by = "date", all = TRUE)
  )
)

#### nas method ####
context("nas method")

test_that(
  "missing values detected correctly",
  expect_identical(
    DTSg$new(DT2)$nas(),
    data.table::data.table(
      .col = c("col1", "col2"),
      .group = c(1L, 1L),
      .from = as.POSIXct(c("2000-10-29 01:00:00", "2000-10-29 01:00:00"), tz = "Europe/Vienna"),
      .to = as.POSIXct(c("2000-10-29 01:30:00", "2000-10-29 02:00:00"), tz = "Europe/Vienna"),
      .n = c(2L, 3L)
    )
  )
)

test_that(
  "no missing values return empty data.table",
  expect_identical(
    DTSg$new(DT2)$nas(cols = "col3"),
    data.table::data.table(
      .col = character(),
      .group = integer(),
      .from = .POSIXct(numeric(), tz = "Europe/Vienna"),
      .to = .POSIXct(numeric(), tz = "Europe/Vienna"),
      .n = integer()
    )
  )
)

#### refresh method ####
context("refresh method")

test_that(
  "coercing .dateTime column returns warning",
  expect_warning(
    DTSg$new(data.table::data.table(date = as.character(DT2[["date"]]), col1 = DT2[["col1"]]))
  )
)

test_that(
  "failing to coerce .dateTime column returns error",
  expect_error(
    DTSg$new(data.table::data.table(date = "timestamp", col1 = DT2[["col1"]]))
  )
)

test_that(
  "data.table with missing timestamps returns error",
  expect_error(
    DTSg$new(data.table::data.table(
      date = c(DT1[["date"]][1:2], NA, DT1[["date"]][4:8]),
      col1 = DT1[["col1"]]
    ))
  )
)

test_that(
  "data.table with duplicated timestamps returns error",
  expect_error(
    DTSg$new(DT1[c(1L, 1L), ])
  )
)

for (by in c(
  sprintf("%s DSTdays", c(1:15, 21L, 28L, 30L)),
  sprintf("%s months", c(1:4, 6L)),
  sprintf("%s years", c(1:3, 5:6))
)) {
  test_that(
    "irregular periodicity is recognised correctly",
    expect_identical(
      {
        date <- seq(
          as.POSIXct("2000-01-31", tz = "Europe/Vienna"),
          as.POSIXct("2012-12-31", tz = "Europe/Vienna"),
          by
        )
        if (grepl("^\\d+ (month|year)(s?)$", by)) {
          date <- rollback(date, by)
        }
        DTSg$new(data.table::data.table(
          date = date,
          col1 = 1L
        ))$periodicity
      },
      by
    )
  )
}

test_that(
  '"unrecognised" periodicity is recognised correctly',
  expect_identical(
    DTSg$new(data.table::data.table(
      date = c(
        seq(
          as.POSIXct("2000-01-01", tz = "Europe/Vienna"),
          as.POSIXct("2000-11-01", tz = "Europe/Vienna"),
          "5 months"
        ),
        seq(
          as.POSIXct("2001-06-01", tz = "Europe/Vienna"),
          as.POSIXct("2002-01-01", tz = "Europe/Vienna"),
          "7 months"
        )
      ),
      col1 = LETTERS[1:5]
    ))$periodicity,
    "unrecognised"
  )
)

test_that(
  '"periodicity" field is set correctly',
  expect_identical(
    DTSg$new(DT1)$periodicity,
    difftime(DT1[["date"]][2L], DT1[["date"]][1L])
  )
)

test_that(
  '"regular" field is set correctly',
  expect_true(
    DTSg$new(DT1)$regular
  )
)

test_that(
  '"timezone" field is set correctly',
  expect_identical(
    DTSg$new(DT1)$timezone,
    "Europe/Vienna"
  )
)

test_that(
  '"periodicity" field is read-only',
  expect_error(
    DTSg$new(DT1)$periodicity <- "1 month"
  )
)

test_that(
  '"regular" field is read-only',
  expect_error(
    DTSg$new(DT1)$regular <- FALSE
  )
)

test_that(
  '"timezone" field is read-only',
  expect_error(
    DTSg$new(DT1)$timezone <- "Europe/Vienna"
  )
)

#### rollapply method ####
context("rollapply method")

test_that(
  '"fun" is applied correctly',
  expect_identical(
    DTSg$new(DT1)$rollapply(function(x, ...) {sum(x)}, before = 2L, after = 1L)$values()[["col1"]],
    c(NA, NA, 16, 24, 32, 40, 48, NA)
  )
)

test_that(
  '"..." passes on arguments correctly',
  expect_identical(
    DTSg$new(DT1)$rollapply(mean, na.rm = TRUE, before = 2L, after = 1L)$values()[["col1"]],
    c(2, 3, 4, 6, 8, 10, 12, 13)
  )
)

test_that(
  "window of size one returns identity",
  expect_identical(
    DTSg$new(DT1)$rollapply(mean, na.rm = TRUE, before = 0L, after = 0L)$values()[["col1"]],
    DT1[["col1"]]
  )
)

test_that(
  '"inverseDistance" weights are correct (power is one)',
  expect_identical(
    DTSg$new(DT1)$rollapply(
      weighted.mean,
      na.rm = TRUE,
      before = 2L,
      after = 1L
    )$values()[["col1"]][3L],
    {
      weights <- 1 / c(rev(seq_len(2) + 1), 1, seq_len(1) + 1)^1
      weights <- weights / sum(weights)
      weighted.mean(DT1[["col1"]][1:4], weights, na.rm = TRUE)
    }
  )
)

test_that(
  '"inverseDistance" weights are correct (power is two)',
  expect_identical(
    DTSg$new(DT1)$rollapply(
      weighted.mean,
      na.rm = TRUE,
      before = 2L,
      after = 1L,
      parameters = list(power = 2)
    )$values()[["col1"]][3L],
    {
      weights <- 1 / c(rev(seq_len(2) + 1), 1, seq_len(1) + 1)^2
      weights <- weights / sum(weights)
      weighted.mean(DT1[["col1"]][1:4], weights, na.rm = TRUE)
    }
  )
)

#### summary method ####
context("summary method")

test_that(
  "values are summarised correctly",
  expect_identical(
    DTSg$new(DT1)$summary(),
    summary(DT1[, -1L, with = FALSE])
  )
)

test_that(
  '"..." passes on arguments correctly',
  expect_identical(
    DTSg$new(DT1)$summary(digits = 0L)[1L, 1L],
    "Min.   : 1  "
  )
)

#### values method ####
context("values method")

test_that(
  "values are copied",
  expect_identical(
    {
      TS <- DTSg$new(DT1)
      TS$values()[, col1 := NULL]
      TS$values()[["col1"]]
    },
    DT1[["col1"]]
  )
)

test_that(
  "reference to values is returned",
  expect_identical(
    {
      TS <- DTSg$new(DT1)
      TS$values(TRUE)[, col1 := NULL]
      TS$values()[["col1"]]
    },
    NULL
  )
)

test_that(
  "name of .dateTime column is not restored",
  expect_identical(
    names(DTSg$new(DT1)$values(TRUE))[1L],
    ".dateTime"
  )
)

test_that(
  "name of .dateTime column is restored",
  expect_identical(
    names(DTSg$new(DT1)$values())[1L],
    "date"
  )
)
