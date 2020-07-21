source("data.R")

#### aggregate method ####
expect_identical(
  DTSg$new(DT1)$aggregate(byYmdH__, list(mean = mean))$values(TRUE),
  data.table(
    .dateTime = seq(
      as.POSIXct("2000-10-29 01:00:00", tz = "Europe/Vienna"),
      as.POSIXct("2000-10-29 03:30:00", tz = "Europe/Vienna"),
      "1 hour"
    ),
    col1.mean = c( 2, 6, 10, 14),
    col2.mean = c(NA, 6, 10, 14),
    key = ".dateTime"
  ),
  info = "values are aggregated correctly (multiple columns and single function)"
)

expect_identical(
  DTSg$new(DT1)$aggregate(byYmdH__, c(mean = "mean"))$values(TRUE),
  data.table(
    .dateTime = seq(
      as.POSIXct("2000-10-29 01:00:00", tz = "Europe/Vienna"),
      as.POSIXct("2000-10-29 03:30:00", tz = "Europe/Vienna"),
      "1 hour"
    ),
    col1.mean = c( 2, 6, 10, 14),
    col2.mean = c(NA, 6, 10, 14),
    key = ".dateTime"
  ),
  info = "values are aggregated correctly (multiple columns and single character function)"
)

expect_identical(
  DTSg$new(DT1)$aggregate(byYmdH__, mean, cols = "col2", n = TRUE)$values(TRUE)[[".n"]],
  c(1L, 2L, 2L, 2L),
  info = ".n is correct (single column and function)"
)

expect_identical(
  DTSg$new(DT1)$aggregate(byYmdH__, "mean", cols = "col2", n = TRUE)$values(TRUE)[[".n"]],
  c(1L, 2L, 2L, 2L),
  info = ".n is correct (single column and character function)"
)

expect_identical(
  DTSg$new(DT1)$aggregate(byYmdH__, list(mean = mean, sum = sum), n = TRUE)$values(TRUE),
  data.table(
    .dateTime = seq(
      as.POSIXct("2000-10-29 01:00:00", tz = "Europe/Vienna"),
      as.POSIXct("2000-10-29 03:30:00", tz = "Europe/Vienna"),
      "1 hour"
    ),
    col1.mean = c( 2, 6 , 10, 14),
    col1.sum  = c( 4, 12, 20, 28),
    col2.mean = c(NA, 6 , 10, 14),
    col2.sum  = c(NA, 12, 20, 28),
    .n = rep(2L, 4),
    key = ".dateTime"
  ),
  info = "values are aggregated correctly and .n is correct (multiple columns and functions)"
)

expect_identical(
  DTSg$new(DT1)$aggregate(byYmdH__, c(mean = "mean", sum = "sum"), n = TRUE)$values(TRUE),
  data.table(
    .dateTime = seq(
      as.POSIXct("2000-10-29 01:00:00", tz = "Europe/Vienna"),
      as.POSIXct("2000-10-29 03:30:00", tz = "Europe/Vienna"),
      "1 hour"
    ),
    col1.mean = c( 2, 6 , 10, 14),
    col1.sum  = c( 4, 12, 20, 28),
    col2.mean = c(NA, 6 , 10, 14),
    col2.sum  = c(NA, 12, 20, 28),
    .n = rep(2L, 4),
    key = ".dateTime"
  ),
  info = "values are aggregated correctly and .n is correct (multiple columns and character functions)"
)

expect_identical(
  DTSg$new(DT1)$aggregate(byYmdH__, list(mean), na.rm = TRUE)$values(TRUE)[["col2"]],
  c(1, 6, 10, 14),
  info = '"..." passes on arguments correctly (single function)'
)

expect_identical(
  DTSg$new(DT1)$aggregate(byYmdH__, "mean", na.rm = TRUE)$values(TRUE)[["col2"]],
  c(1, 6, 10, 14),
  info = '"..." passes on arguments correctly (single character function)'
)

expect_identical(
  DTSg$new(DT1)$aggregate(
    byYmdH__,
    list(mean = mean, sum = sum),
    na.rm = TRUE
  )$values(TRUE)[["col2.sum"]],
  c(1, 12, 20, 28),
  info = '"..." passes on arguments correctly (multiple functions)'
)

expect_identical(
  DTSg$new(DT1)$aggregate(
    byYmdH__,
    c(mean = "mean", sum = "sum"),
    na.rm = TRUE
  )$values(TRUE)[["col2.sum"]],
  c(1, 12, 20, 28),
  info = '"..." passes on arguments correctly (multiple character functions)'
)

expect_error(
  DTSg$new(DT1)$aggregate(byYmdH__, list(mean = mean, sum)),
  info = 'single missing "fun" name returns error'
)

expect_error(
  DTSg$new(DT1)$aggregate(byYmdH__, c("mean", "sum")),
  info = 'missing "fun" names return error'
)

expect_true(
  DTSg$new(DT1)$aggregate(byYmdH__, mean)$aggregated,
  info = '"aggregated" field is set correctly'
)

#### alter method ####
expect_identical(
  DTSg$new(DT1[-3L, ])$alter(
    "2000-10-29 01:00:00",
    "2000-10-29 03:00:00",
    "1 hour"
  )$values(TRUE),
  data.table(
    .dateTime = seq(
      as.POSIXct("2000-10-29 01:00:00", tz = "Europe/Vienna"),
      as.POSIXct("2000-10-29 03:00:00", tz = "Europe/Vienna"),
      "1 hour"
    ),
    col1 = c(  1, NA,   9,  13),
    col2 = c(  1, NA,   9,  13),
    col3 = c("A", NA, "E", "G"),
    key = ".dateTime"
  ),
  info = "values are altered correctly (explicitly missing values)"
)

expect_identical(
  DTSg$new(DT2[, .(date, col1)])$alter(na.status = "implicit")$values(),
  setkey(DT2[3L, .(date, col1)], "date"),
  info = "values are altered correctly (single column and implicitly missing values)"
)

expect_identical(
  DTSg$new(DT2[, -4L])$alter(na.status = "implicit")$values(),
  setkey(DT2[3L, -4L], "date"),
  info = "values are altered correctly (multiple columns and implicitly missing values)"
)

expect_identical(
  DTSg$new(DT1[-3L, ])$alter(
    "2000-01-31",
    "2000-07-01",
    "1 month"
  )$values(TRUE)[[".dateTime"]],
  as.POSIXct(
    c("2000-01-31", "2000-02-29", "2000-03-31", "2000-04-30", "2000-05-31", "2000-06-30"),
    tz = "Europe/Vienna"
  ),
  info = 'call to "rollback" is made'
)

expect_error(
  DTSg$new(DT1)$alter(by = "unrecognised"),
  info = "recognised to unrecognised periodicity returns error"
)

expect_warning(
  DTSg$new(DT3),
  info = "explicitly missing values and unrecognised periodicity returns warning"
)

expect_error(
  DTSg$new(DT1)$alter(na.status = "undecided"),
  info = 'decided to undecided "na.status" returns error'
)

expect_identical(
  DTSg$new(DT1)$na.status,
  "explicit",
  info = '"na.status" field is set correctly'
)

expect_identical(
  {
    TS <- DTSg$new(DT1, na.status = "undecided")
    TS$na.status <- "implicit"
    TS$na.status
  },
  "implicit",
  info = '"na.status" field is changed correctly'
)

expect_identical(
  {
    TS <- DTSg$new(DT1)
    TS$periodicity <- "1 hour"
    TS$periodicity
  },
  .difftime(1, units = "hours"),
  info = '"periodicity" field is changed correctly'
)

#### clone method ####
expect_identical(
  {
    TS <- DTSg$new(DT1)
    TS$aggregate(byYmdH__, mean)
    TS$alter(by = "1 hour")
    TS$colapply(as.factor, helpers = FALSE)
    TS$merge(DT2)
    TS$rollapply(weighted.mean, na.rm = TRUE)
    TS$rowaggregate("col", sum, na.rm = TRUE)
    TS$rowbind(data.table(
      date = as.POSIXct("2000-10-29 04:00:00", tz = "Europe/Vienna"),
      col1 = 17
    ))
    TS$setColNames("col1", "col4")
    TS$setCols(values = 1)
    TS$subset(1L)
    TS
  },
  TS,
  info = "deep clone is created by default"
)

#### colapply method ####
expect_identical(
  DTSg$new(DT1)$colapply(
    as.factor,
    cols = c("col1", "col2"),
    helpers = FALSE
  )$cols("factor"),
  c("col1", "col2"),
  info = '"fun" is applied correctly'
)

expect_identical(
  DTSg$new(DT1[-1L, ])$colapply(
    interpolateLinear,
    rollends = FALSE,
    cols = "col2"
  )$values(TRUE)[["col2"]],
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
    cumsum,
    cols = "col2",
    funby = byYmdH__,
    helpers = FALSE
  )$values(TRUE)[["col2"]],
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
  info = "character column names are returned"
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
  info = "column names matching pattern are returned"
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

#### getCol method ####
expect_identical(
  DTSg$new(DT1)$getCol("col2"),
  DT1[["col2"]],
  info = "column is returned correctly (getCol)"
)

expect_identical(
  {
    TS <- DTSg$new(DT1)
    TS["col2"]
  },
  DT1[["col2"]],
  info = "column is returned correctly ([)"
)

#### initialize method ####
expect_error(
  DTSg$new(data.table(date = numeric(0L), col1 = integer(0L))),
  info = "data.table without a single row returns error"
)

expect_error(
  DTSg$new(DT1[, .(date)]),
  info = "data.table with a single column returns error"
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
expect_identical(
  DTSg$new(DT1[, .(date, col1)])$merge(DT2)$values(),
  merge(DT1[, .(date, col1)], DT2, by = "date"),
  info = "values are merged correctly"
)

expect_identical(
  DTSg$new(DT1[1:4, .(date, col1)])$merge(
    DTSg$new(DT1[c(5L, 7L, 8L), .(date, col1)], na.status = "implicit"),
    all = TRUE
  )$values(),
  setkey(rbind(
    DT1[1:4, .(date, col1.x = col1, col1.y = NA                  )],
    DT1[5:8, .(date, col1.x = NA  , col1.y = col1[c(1, NA, 3, 4)])]
  ), "date"),
  info = '"..." passes on arguments correctly and missing values are made explicit'
)

expect_error(
  DTSg$new(DT1[, .(date, col1)])$merge(DT2, by = "date"),
  info = "use of arguments not allowed returns error"
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
expect_silent(
  {
    TS <- DTSg$new(DT1, "id", "parameter", "unit", "variant")
    TS$plot(secAxisCols = "col2", secAxisLabel = "y2")
    TS$print()
  },
  info = '"plot" and "print" do not throw an error or warning'
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
  DTSg$new(data.table(date = .POSIXct(NA_real_), col1 = pi)),
  info = "data.table with a single row and missing timestamp returns error"
)

expect_identical(
  names(DTSg$new(DT1[1L, ])$values(TRUE))[1L],
  ".dateTime",
  info = "data.table with a single row ends up with .dateTime column"
)

expect_error(
  DTSg$new(data.table(
    date = c(DT1[["date"]][1:2], NA, NA, DT1[["date"]][5:8]),
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
  DTSg$new(DT3)$periodicity,
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
    TS$values(TRUE)[[".dateTime"]]
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
  DTSg$new(DT1)$rollapply(
    sum,
    before = 2L,
    after = 1L,
    helpers = FALSE,
    memoryOverCPU = TRUE
  )$values(TRUE)[["col1"]],
  c(NA, NA, 16, 24, 32, 40, 48, NA),
  info = '"fun" is applied correctly (memoryOverCPU = TRUE)'
)

expect_identical(
  DTSg$new(DT1)$rollapply(
    sum,
    before = 2L,
    after = 1L,
    helpers = FALSE,
    memoryOverCPU = FALSE
  )$values(TRUE)[["col1"]],
  c(NA, NA, 16, 24, 32, 40, 48, NA),
  info = '"fun" is applied correctly (memoryOverCPU = FALSE)'
)

expect_identical(
  DTSg$new(DT1)$rollapply(
    mean,
    na.rm = TRUE,
    before = 2L,
    after = 1L,
    memoryOverCPU = TRUE
  )$values(TRUE)[["col1"]],
  c(2, 3, 4, 6, 8, 10, 12, 13),
  info = '"..." passes on arguments correctly (memoryOverCPU = TRUE)'
)

expect_identical(
  DTSg$new(DT1)$rollapply(
    mean,
    na.rm = TRUE,
    before = 2L,
    after = 1L,
    memoryOverCPU = FALSE
  )$values(TRUE)[["col1"]],
  c(2, 3, 4, 6, 8, 10, 12, 13),
  info = '"..." passes on arguments correctly (memoryOverCPU = FALSE)'
)

expect_identical(
  DTSg$new(DT1)$rollapply(
    mean,
    na.rm = TRUE,
    before = 0L,
    memoryOverCPU = TRUE
  )$values(TRUE)[["col1"]],
  DT1[["col1"]],
  info = "window of size one returns identity (memoryOverCPU = TRUE)"
)

expect_identical(
  DTSg$new(DT1)$rollapply(
    mean,
    na.rm = TRUE,
    before = 0L,
    memoryOverCPU = FALSE
  )$values(TRUE)[["col1"]],
  DT1[["col1"]],
  info = "window of size one returns identity (memoryOverCPU = FALSE)"
)

expect_identical(
  DTSg$new(DT1)$rollapply(
    weighted.mean,
    na.rm = TRUE,
    before = 2L,
    after = 1L,
    memoryOverCPU = TRUE
  )$values(TRUE)[["col1"]][3L],
  {
    weights <- 1 / c(rev(seq_len(2) + 1), 1, seq_len(1) + 1)^1
    weights <- weights / sum(weights)
    weighted.mean(DT1[["col1"]][1:4], weights, na.rm = TRUE)
  },
  info = '"inverseDistance" weights are correct (power = 1 and memoryOverCPU = TRUE)'
)

expect_identical(
  DTSg$new(DT1)$rollapply(
    weighted.mean,
    na.rm = TRUE,
    before = 2L,
    after = 1L,
    parameters = list(power = 2),
    memoryOverCPU = FALSE
  )$values(TRUE)[["col1"]][3L],
  {
    weights <- 1 / c(rev(seq_len(2) + 1), 1, seq_len(1) + 1)^2
    weights <- weights / sum(weights)
    weighted.mean(DT1[["col1"]][1:4], weights, na.rm = TRUE)
  },
  info = '"inverseDistance" weights are correct (power = 2 and memoryOverCPU = FALSE)'
)

expect_identical(
  DTSg$new(DT1)$rollapply(
    identity,
    before = 0L,
    cols = c("col1", "col2"),
    resultCols = c("col1", "col4"),
    suffix = "_character",
    helpers = FALSE
  )$cols(),
  c("col1", "col2", "col3", "col4"),
  info = '"resultCols" adds and overwrites columns correctly'
)

expect_identical(
  DTSg$new(DT1)$rollapply(
    identity,
    before = 0L,
    cols = c("col1", "col2"),
    suffix = "_identity",
    helpers = FALSE
  )$cols(),
  c("col1", "col2", "col3", "col1_identity", "col2_identity"),
  info = '"suffix" adds columns correctly'
)

#### rowaggregate method ####
expect_identical(
  DTSg$new(DT1)$rowaggregate("col", list(sum = sum))$values(TRUE),
  setkey(
    DT1[, .(.dateTime = date, col1, col2, col3, col.sum = col1 + col2)],
    ".dateTime"
  ),
  info = "values are aggregated correctly (single function)"
)

expect_identical(
  DTSg$new(DT1)$rowaggregate("col", c(sum = "sum"))$values(TRUE),
  setkey(
    DT1[, .(.dateTime = date, col1, col2, col3, col.sum = col1 + col2)],
    ".dateTime"
  ),
  info = "values are aggregated correctly (single character function)"
)

expect_identical(
  DTSg$new(DT1)$rowaggregate(c("colmean", "colsum"), list(mean = mean, sum = sum))$values(TRUE),
  setkey(
    DT1[, .(.dateTime = date, col1, col2, col3, colmean = (col1 + col2) / 2, colsum = col1 + col2)],
    ".dateTime"
  ),
  info = "values are aggregated correctly (multiple functions)"
)

expect_identical(
  DTSg$new(DT1)$rowaggregate("col", c(mean = "mean", sum = "sum"))$values(TRUE),
  setkey(
    DT1[, .(.dateTime = date, col1, col2, col3, col.mean = (col1 + col2) / 2, col.sum = col1 + col2)],
    ".dateTime"
  ),
  info = "values are aggregated correctly (multiple character functions)"
)

expect_identical(
  DTSg$new(DT1)$rowaggregate("col", list(sum), na.rm = TRUE)$values(TRUE)[["col"]],
  c(2, 3, seq(10, 30, 4)),
  info = '"..." passes on arguments correctly (single function)'
)

expect_identical(
  DTSg$new(DT1)$rowaggregate("col", "sum", na.rm = TRUE)$values(TRUE)[["col"]],
  c(2, 3, seq(10, 30, 4)),
  info = '"..." passes on arguments correctly (single character function)'
)

expect_identical(
  DTSg$new(DT1)$rowaggregate("col", list(mean = mean, sum = sum), na.rm = TRUE)$values(TRUE)[["col.sum"]],
  c(2, 3, seq(10, 30, 4)),
  info = '"..." passes on arguments correctly (multiple functions)'
)

expect_identical(
  DTSg$new(DT1)$rowaggregate(c("colmean", "colsum"), c(mean = "mean", sum = "sum"), na.rm = TRUE)$values(TRUE)[["colsum"]],
  c(2, 3, seq(10, 30, 4)),
  info = '"..." passes on arguments correctly (multiple character functions)'
)

expect_error(
  DTSg$new(DT1)$rowaggregate(c("colmean", "col1"), list(mean = mean, sum = sum)),
  info = 'existing "resultCols" return error'
)

expect_error(
  DTSg$new(DT1)$rowaggregate("col", list(mean, sum)),
  info = 'missing "fun" names return error'
)

expect_error(
  DTSg$new(DT1)$rowaggregate("col", c("mean", sum = "sum")),
  info = 'single missing "fun" name returns error'
)

#### rowbind method ####
expect_identical(
  DTSg$new(DT1[1:2, ])$rowbind(
    setnames(DT1[3:4, ], "col3", "col4"),
    list(DTSg$new(DT1[5:6, -"col3"]), DT1[7:8, ])
  )$values(),
  setkey(DT1[, .(
    date, col1, col2,
    col3 = c("A", "B", rep(NA, 4), "G", "H"),
    col4 = c(NA , NA , "C", "D", rep(NA, 4))
  )], "date"),
  info = "rows are bound correctly"
)

expect_error(
  DTSg$new(DT1)$rowbind(DT1),
  info = "duplicated timestamps return error"
)

#### setColNames method ####
expect_identical(
  DTSg$new(DT1)$setColNames(c("col2", "col3"), c("column2", "column3"))$cols(),
  c("col1", "column2", "column3"),
  info = "column names are set correctly"
)

expect_error(
  DTSg$new(DT1)$setColNames("col2", ""),
  info = "blank column name returns error"
)

expect_error(
  DTSg$new(DT1)$setColNames("col2", ".column2"),
  info = "column name with a starting dot returns error"
)

#### setCols method ####
expect_identical(
  DTSg$new(DT1)$setCols(2L, "col2", 3)$values(TRUE)[["col2"]],
  DT1[["col1"]],
  info = "values are set correctly (numeric vector and single column)"
)

expect_identical(
  {
    i <- 1:3
    DTSg$new(DT1)$setCols(
      i,
      c("col1", "col2", "col3"),
      DT2[1:3, .(col1, as.numeric(col2), col3)],
    )$values()
  },
  setkey(rbind(
    DT2[1:3, .(date = DT1$date[1:3], col1, col2 = as.numeric(col2), col3)],
    DT1[4:8]
  ), "date"),
  info = "values are set correctly (numeric vector and multiple columns)"
)

expect_identical(
  {
    col2 <- rep(NA_real_, 8)
    DTSg$new(DT1)$setCols(is.na(col2), "col2", 3)$values(TRUE)[["col2"]]
  },
  DT1[["col1"]],
  info = "values are set correctly (expression)"
)

expect_identical(
  DTSg$new(DT1)$setCols(, "col4", DT1[["col1"]])$values(TRUE)[["col4"]],
  DT1[["col1"]],
  info = "column is added correctly"
)

expect_identical(
  DTSg$new(DT1)$setCols(, "col2", NULL)$cols(),
  c("col1", "col3"),
  info = "column is removed correctly"
)

expect_error(
  DTSg$new(DT1)$setCols(, ".dateTime", 1),
  info = "setting .dateTime column returns error"
)

expect_error(
  DTSg$new(DT1)$setCols(, c("col1", "col2", "col3"), NULL),
  info = "removing all value columns returns error"
)

#### subset method ####
expect_identical(
  {
    i <- 1L
    DTSg$new(DT1)$subset(i, funby = byYmdH__)$values()
  },
  setkey(DT1[c(1L, 3L, 5L, 7L), ], "date"),
  info = "rows are filtered correctly (numeric vector and funby)"
)

expect_identical(
  DTSg$new(DT1)$subset(.N, funby = byYmdH__)$values(),
  setkey(DT1[c(2L, 4L, 6L, 8L), ], "date"),
  info = "rows are filtered correctly (expression and funby)"
)

expect_identical(
  {
    col2 <- 1:4
    DTSg$new(DT1)$subset(col2 > 8, "col2")$values()
  },
  setkey(DT1[5:8, .(date, col2)], "date"),
  info = "rows and columns are subset correctly"
)

expect_identical(
  DTSg$new(DT1)$subset(, c("col1", "col3"))$values(),
  setkey(DT1[, .(date, col1, col3)], "date"),
  info = "columns are selected correctly"
)

expect_identical(
  DTSg$new(DT1)$subset(c(1L, 8L))$values(),
  setkey(DT1[c(1L, 8L), ], "date"),
  info = "missing values are made implicit"
)

expect_error(
  DTSg$new(DT1)$subset(col2 > 100),
  info = "empty filter result returns error"
)

expect_error(
  DTSg$new(DT1)$subset(12L),
  info = "missing timestamps return error"
)

expect_error(
  DTSg$new(DT1)$subset(c(1L, 1L)),
  info = "duplicated timestamps return error"
)

expect_error(
  DTSg$new(DT1)$subset(, ""),
  info = "blank selection returns error"
)

#### summary method ####
expect_identical(
  DTSg$new(DT1)$summary(),
  summary(DT1[, -1L]),
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
    TS$values(TRUE)[["col1"]]
  },
  DT1[["col1"]],
  info = "values are copied"
)

expect_identical(
  {
    TS <- DTSg$new(DT1)
    TS$values(TRUE)[, col1 := NULL]
    TS$values(TRUE)[["col1"]]
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
