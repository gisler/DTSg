source("data.R")

#### aggregate method ####
expect_identical(
  DTSg$new(DT1)$aggregate(byYmdH__, mean)$values(),
  data.table(
    date = seq(
      as.POSIXct("2000-10-29 01:00:00", tz = "Europe/Vienna"),
      as.POSIXct("2000-10-29 03:30:00", tz = "Europe/Vienna"),
      "1 hour"
    ),
    col1 = c( 2, 6, 10, 14),
    col2 = c(NA, 6, 10, 14),
    key = "date"
  ),
  info = "values are aggregated correctly (multiple columns and single function)"
)

expect_identical(
  DTSg$new(DT1)$aggregate(byYmdH__, mean, cols = "col2", n = TRUE)$values()[[".n"]],
  c(1L, 2L, 2L, 2L),
  info = ".n is correct (single column and function)"
)

expect_identical(
  DTSg$new(DT1)$aggregate(byYmdH__, list(mean = mean, sum = sum), n = TRUE)$values(),
  data.table(
    date = seq(
      as.POSIXct("2000-10-29 01:00:00", tz = "Europe/Vienna"),
      as.POSIXct("2000-10-29 03:30:00", tz = "Europe/Vienna"),
      "1 hour"
    ),
    col1.mean = c( 2, 6 , 10, 14),
    col1.sum  = c( 4, 12, 20, 28),
    col2.mean = c(NA, 6 , 10, 14),
    col2.sum  = c(NA, 12, 20, 28),
    .n = rep(2L, 4),
    key = "date"
  ),
  info = "values are aggregated correctly and .n is correct (multiple columns and functions)"
)

expect_identical(
  DTSg$new(DT1)$aggregate(byYmdH__, mean, na.rm = TRUE)$values()[["col2"]],
  c(1, 6, 10, 14),
  info = '"..." passes on arguments correctly (single function)'
)

expect_identical(
  DTSg$new(DT1)$aggregate(byYmdH__, list(mean = mean, sum = sum), na.rm = TRUE)$values()[["col2.sum"]],
  c(1, 12, 20, 28),
  info = '"..." passes on arguments correctly (multiple functions)'
)

expect_true(
  DTSg$new(DT1)$aggregate(byYmdH__, mean)$aggregated,
  info = '"aggregated" field is set correctly'
)

#### alter method ####
expect_error(
  DTSg$new(DT1)$alter(by = "unrecognised"),
  info = "unrecognised periodicity returns error"
)

expect_identical(
  DTSg$new(DT1[-3L, ])$alter("2000-10-29 01:00:00", "2000-10-29 03:00:00", "1 hour")$values(),
  data.table(
    date = seq(
      as.POSIXct("2000-10-29 01:00:00", tz = "Europe/Vienna"),
      as.POSIXct("2000-10-29 03:00:00", tz = "Europe/Vienna"),
      "1 hour"
    ),
    col1 = c(  1, NA,   9,  13),
    col2 = c(  1, NA,   9,  13),
    col3 = c("A", NA, "E", "G"),
    key = "date"
  ),
  info = "values are altered correctly"
)

expect_identical(
  DTSg$new(DT1[-3L, ])$alter("2000-01-31", "2000-07-01", "1 month")$values()[["date"]],
  as.POSIXct(
    c("2000-01-31", "2000-02-29", "2000-03-31", "2000-04-30", "2000-05-31", "2000-06-30"),
    tz = "Europe/Vienna"
  ),
  info = 'call to "rollback" is made'
)

#### clone method ####
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
  TS,
  info = "deep clone is created by default"
)

#### colapply method ####
expect_identical(
  DTSg$new(DT1)$colapply(
    function(x, ...) {as.factor(x)},
    cols = c("col1", "col2")
  )$cols("factor"),
  c("col1", "col2"),
  info = '"fun" is applied correctly'
)

expect_identical(
  DTSg$new(DT1[-1L, ])$colapply(
    interpolateLinear,
    rollends = FALSE,
    cols = "col2"
  )$values()[["col2"]],
  c(NA, seq(5, 15, by = 2)),
  info = '"..." passes on arguments correctly'
)

expect_identical(
  DTSg$new(DT1)$colapply(
    as.character,
    cols = c("col1", "col2"),
    resultCols = c("col1", "col4"),
    suffix = "_character"
  )$cols(),
  c("col1", "col2", "col3", "col4"),
  info = '"resultCols" adds and overwrites columns correctly'
)

expect_identical(
  DTSg$new(DT1)$colapply(
    as.character,
    cols = c("col1", "col2"),
    suffix = "_character"
  )$cols(),
  c("col1", "col2", "col3", "col1_character", "col2_character"),
  info = '"suffix" adds columns correctly'
)

expect_identical(
  DTSg$new(DT1)$colapply(
    function(x, ...) {cumsum(x)},
    cols = "col2",
    funby = byYmdH__
  )$values()[["col2"]],
  c(1, NA, 5, 12, 9, 20, 13, 28),
  info = '"funby" is applied correctly'
)

#### cols method ####
expect_identical(
  DTSg$new(DT1)$cols(),
  c("col1", "col2", "col3"),
  info = "all column names are returned"
)

expect_identical(
  DTSg$new(DT1)$cols("character"),
  "col3",
  info = "character column name is returned"
)

expect_identical(
  DTSg$new(DT2)$cols(c("logical", "character")),
  c("col2", "col3"),
  info = "logical and character column names are returned"
)

expect_identical(
  DTSg$new(DT1)$cols("integer"),
  character(),
  info = "no column name is returned (class)"
)

expect_identical(
  DTSg$new(DT1)$cols(pattern = "^c.l1$"),
  "col1",
  info = "column name matching pattern is returned"
)

expect_identical(
  DTSg$new(DT1)$cols(pattern = "COL1"),
  character(),
  info = "no column name is returned (pattern)"
)

expect_identical(
  DTSg$new(DT1)$cols(pattern = "COL1", ignore.case = TRUE),
  "col1",
  info = '"..." passes on arguments correctly'
)

expect_error(
  DTSg$new(DT1)$cols(pattern = ".*", value = FALSE),
  info = "use of arguments not allowed returns error"
)

#### initialize method ####
expect_error(
  DTSg$new(DT1[, .(date)]),
  info = "data.table with a single column only returns error"
)

expect_true(
  DTSg$new(DT1, aggregated = TRUE)$aggregated,
  info = '"aggregated" field is set correctly'
)

expect_true(
  DTSg$new(DT1, fast = TRUE)$fast,
  info = '"fast" field is set correctly'
)

expect_identical(
  DTSg$new(DT1, ID = "ID")$ID,
  "ID",
  info = '"ID" field is set correctly'
)

expect_identical(
  DTSg$new(DT1, parameter = "parameter")$parameter,
  "parameter",
  info = '"parameter" field is set correctly'
)

expect_identical(
  DTSg$new(DT1, unit = "unit")$unit,
  "unit",
  info = '"unit" field is set correctly'
)

expect_identical(
  DTSg$new(DT1, variant = "variant")$variant,
  "variant",
  info = '"variant" field is set correctly'
)

expect_true(
  {
    DTcopy <<- copy(flow)
    DTref1 <<- flow
    DTref2 <<- flow
    DTSg$new(DTref1, swallow = TRUE)
    exists("DTcopy", where = 1L) &&
      !exists("DTref1", where = 1L) &&
      !exists("DTref2", where = 1L)
  },
  info = "all/only global references are removed"
)

#### merge method ####
expect_error(
  DTSg$new(DT1[, .(date, col1)])$merge(DT2, by = "date"),
  info = "use of arguments not allowed returns error"
)

expect_identical(
  DTSg$new(DT1[, .(date, col1)])$merge(DT2)$values(),
  merge(DT1[, .(date, col1)], DT2, by = "date"),
  info = "values are merged correctly"
)

expect_identical(
  DTSg$new(DT1[, .(date, col1)])$merge(DT2, all = TRUE)$values(),
  merge(DT1[, .(date, col1)], DT2, by = "date", all = TRUE),
  info = '"..." passes on arguments correctly'
)

#### nas method ####
expect_identical(
  DTSg$new(DT2)$nas(),
  data.table(
    .col = c("col1", "col2"),
    .group = c(1L, 1L),
    .from = as.POSIXct(
      c("2000-10-29 01:00:00", "2000-10-29 01:00:00"),
      tz = "Europe/Vienna"
    ),
    .to = as.POSIXct(
      c("2000-10-29 01:30:00", "2000-10-29 02:00:00"),
      tz = "Europe/Vienna"
    ),
    .n = c(2L, 3L)
  ),
  info = "missing values are detected correctly"
)

expect_identical(
  DTSg$new(DT2)$nas("col3"),
  data.table(
    .col = character(),
    .group = integer(),
    .from = .POSIXct(numeric(), tz = "Europe/Vienna"),
    .to = .POSIXct(numeric(), tz = "Europe/Vienna"),
    .n = integer()
  ),
  info = "no missing values returns empty data.table"
)

#### plot and print methods ####
expect_true(
  {
    TS <- DTSg$new(DT1, "id", "parameter", "unit", "variant")
    TS$plot(secAxisCols = "col2", secAxisLabel = "y2")
    TS$print()
    TRUE
  },
  info = '"plot" and "print" work'
)

#### refresh method ####
expect_error(
  DTSg$new(data.table(date = "timestamp", col1 = DT2[["col1"]])),
  info = "failing to coerce .dateTime column returns error"
)

expect_warning(
  DTSg$new(data.table(date = as.character(DT2[["date"]]), col1 = DT2[["col1"]])),
  info = "coercing .dateTime column returns warning"
)

expect_error(
  DTSg$new(data.table(
    date = c(DT1[["date"]][1:2], NA, DT1[["date"]][4:8]),
    col1 = DT1[["col1"]]
  )),
  info = "data.table with missing timestamps returns error"
)

expect_error(
  DTSg$new(DT1[c(1L, 1L), ]),
  info = "data.table with duplicated timestamps returns error"
)

for (by in c(
  sprintf("%s DSTdays", c(1:15, 21L, 28L, 30L)),
  sprintf("%s months", c(1:4, 6L)),
  sprintf("%s years", c(1:3, 5:6))
)) {
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
      DTSg$new(data.table(
        date = date,
        col1 = 1L
      ))$periodicity
    },
    by,
    info = "irregular periodicity is recognised correctly"
  )
}

expect_identical(
  DTSg$new(data.table(
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
  "unrecognised",
  info = "unrecognised periodicity is recognised correctly"
)

expect_identical(
  DTSg$new(DT1)$periodicity,
  difftime(DT1[["date"]][2L], DT1[["date"]][1L]),
  info = '"periodicity" field is set correctly'
)

expect_true(
  DTSg$new(DT1)$regular,
  info = '"regular" field is set correctly'
)

expect_identical(
  DTSg$new(DT1)$timestamps,
  8L,
  info = '"timestamps" field is set correctly'
)

expect_identical(
  DTSg$new(DT1)$timezone,
  "Europe/Vienna",
  info = '"timezone" field is set correctly'
)

expect_error(
  DTSg$new(DT1)$periodicity <- "1 month",
  info = '"periodicity" field is read-only'
)

expect_error(
  DTSg$new(DT1)$regular <- FALSE,
  info = '"regular" field is read-only'
)

expect_error(
  DTSg$new(DT1)$timestamps <- 1L,
  info = '"timestamps" field is read-only'
)

expect_identical(
  {
    TS <- DTSg$new(DT1)
    TS$timezone <- "Europe/Kiev"
    TS$values()[["date"]]
  },
  seq(
    as.POSIXct("2000-10-29 02:00:00", tz = "Europe/Kiev"),
    as.POSIXct("2000-10-29 04:30:00", tz = "Europe/Kiev"),
    by = "30 min"
  ),
  info = '"timezone" is converted correctly'
)

#### rollapply method ####
expect_identical(
  DTSg$new(DT1)$rollapply(function(x, ...) {sum(x)}, before = 2L, after = 1L)$values()[["col1"]],
  c(NA, NA, 16, 24, 32, 40, 48, NA),
  info = '"fun" is applied correctly (memoryOverCPU = TRUE)'
)

expect_identical(
  DTSg$new(DT1)$rollapply(function(x, ...) {sum(x)}, before = 2L, after = 1L, memoryOverCPU = FALSE)$values()[["col1"]],
  c(NA, NA, 16, 24, 32, 40, 48, NA),
  info = '"fun" is applied correctly (memoryOverCPU = FALSE)'
)

expect_identical(
  DTSg$new(DT1)$rollapply(mean, na.rm = TRUE, before = 2L, after = 1L)$values()[["col1"]],
  c(2, 3, 4, 6, 8, 10, 12, 13),
  info = '"..." passes on arguments correctly (memoryOverCPU = TRUE)'
)

expect_identical(
  DTSg$new(DT1)$rollapply(mean, na.rm = TRUE, before = 2L, after = 1L, memoryOverCPU = FALSE)$values()[["col1"]],
  c(2, 3, 4, 6, 8, 10, 12, 13),
  info = '"..." passes on arguments correctly (memoryOverCPU = FALSE)'
)

expect_identical(
  DTSg$new(DT1)$rollapply(mean, na.rm = TRUE, before = 0L, after = 0L)$values()[["col1"]],
  DT1[["col1"]],
  info = "window of size one returns identity (memoryOverCPU = TRUE)"
)

expect_identical(
  DTSg$new(DT1)$rollapply(mean, na.rm = TRUE, before = 0L, after = 0L, memoryOverCPU = FALSE)$values()[["col1"]],
  DT1[["col1"]],
  info = "window of size one returns identity (memoryOverCPU = FALSE)"
)

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
  },
  info = '"inverseDistance" weights are correct (power = one)'
)

expect_identical(
  DTSg$new(DT1)$rollapply(
    weighted.mean,
    na.rm = TRUE,
    before = 2L,
    after = 1L,
    parameters = list(power = 2),
    memoryOverCPU = FALSE
  )$values()[["col1"]][3L],
  {
    weights <- 1 / c(rev(seq_len(2) + 1), 1, seq_len(1) + 1)^2
    weights <- weights / sum(weights)
    weighted.mean(DT1[["col1"]][1:4], weights, na.rm = TRUE)
  },
  info = '"inverseDistance" weights are correct (power = two)'
)

expect_identical(
  DTSg$new(DT1)$rollapply(
    function(x, ...) {identity(x)},
    before = 0L,
    after = 0L,
    cols = c("col1", "col2"),
    resultCols = c("col1", "col4"),
    suffix = "_character"
  )$cols(),
  c("col1", "col2", "col3", "col4"),
  info = '"resultCols" adds and overwrites columns correctly'
)

expect_identical(
  DTSg$new(DT1)$rollapply(
    function(x, ...) {identity(x)},
    before = 0L,
    after = 0L,
    cols = c("col1", "col2"),
    suffix = "_identity"
  )$cols(),
  c("col1", "col2", "col3", "col1_identity", "col2_identity"),
  info = '"suffix" adds columns correctly'
)

#### summary method ####
expect_identical(
  DTSg$new(DT1)$summary(),
  summary(DT1[, -1L, with = FALSE]),
  info = "values are summarised correctly"
)

expect_identical(
  DTSg$new(DT1)$summary(digits = 0L)[1L, 1L],
  "Min.   : 1  ",
  info = '"..." passes on arguments correctly'
)

#### values method ####
expect_identical(
  {
    TS <- DTSg$new(DT1)
    TS$values()[, col1 := NULL]
    TS$values()[["col1"]]
  },
  DT1[["col1"]],
  info = "values are copied"
)

expect_identical(
  {
    TS <- DTSg$new(DT1)
    TS$values(TRUE)[, col1 := NULL]
    TS$values()[["col1"]]
  },
  NULL,
  info = "reference to values is returned"
)

expect_true(
  {
    TS <- DTSg$new(flow)
    TScopy <<- TS$clone(deep = TRUE)
    TSref1 <<- TS
    TSref2 <<- TS
    TS$values(TRUE, TRUE)
    exists("TScopy", where = 1L) &&
      !exists("TSref1", where = 1L) &&
      !exists("TSref2", where = 1L)
  },
  info = "all/only global references are removed"
)

expect_identical(
  names(DTSg$new(DT1)$values(FALSE, FALSE))[1L],
  "date",
  info = "name of .dateTime column is restored (drop = FALSE)"
)

expect_identical(
  names(DTSg$new(DT1)$values(FALSE, TRUE))[1L],
  "date",
  info = "name of .dateTime column is restored (drop = TRUE)"
)

expect_identical(
  names(DTSg$new(DT1)$values(TRUE))[1L],
  ".dateTime",
  info = "name of .dateTime column is not restored"
)

expect_identical(
  class(DTSg$new(DT1)$values(FALSE)),
  c("data.table", "data.frame"),
  info = "class is data.table (reference = FALSE)"
)

expect_identical(
  class(DTSg$new(DT1)$values(TRUE, FALSE, "data.frame")),
  c("data.table", "data.frame"),
  info = "class is data.table (reference = TRUE)"
)

expect_identical(
  class(DTSg$new(DT1)$values(FALSE, TRUE)),
  c("data.table", "data.frame"),
  info = "class is data.table (drop = TRUE)"
)

expect_identical(
  class(DTSg$new(DT1)$values(FALSE, FALSE, "data.frame")),
  "data.frame",
  info = "class is data.frame (drop = FALSE)"
)

expect_identical(
  class(DTSg$new(DT1)$values(FALSE, TRUE, "data.frame")),
  "data.frame",
  info = "class is data.frame (drop = TRUE)"
)
