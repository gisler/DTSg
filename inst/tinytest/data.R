library(data.table) # nolint

#### global test data ####
DT1 <- data.table(
  date = seq(
    as.POSIXct("2000-10-29 01:00:00", tz = "Europe/Vienna"),
    as.POSIXct("2000-10-29 03:30:00", tz = "Europe/Vienna"),
    "30 mins"
  ),
  col1 = seq(1, 15, by = 2),
  col2 = c(1, NA, seq(5, 15, by = 2)),
  col3 = LETTERS[1:8]
)

DT2 <- data.table(
  date = seq(
    as.POSIXct("2000-10-29 01:00:00", tz = "Europe/Vienna"),
    as.POSIXct("2000-10-29 02:00:00", tz = "Europe/Vienna"),
    "30 mins"
  ),
  col1 = c(NA, NA, 1),
  col2 = rep(NA, 3L),
  col3 = letters[1:3]
)

DT3 <- data.table(
  date = structure(c(
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
  ), tzone = "Europe/Vienna"),
  col1 = LETTERS[1:5]
)

#### funby test data ####
daysPerHalfyear <- c(second = 184L, first = 182L)
daysPerQuarter <- c(third = 92L, fourth = 92L, first = 91L, second = 91L)
daysPerMonth <- c(31L, 31L, 30L, 31L, 30L, 31L, 31L, 29L, 31L, 30L, 31L, 30L)
names(daysPerMonth) <- c(month.name[7:12], month.name[1:6])

#### UTC ####
UTChourlyData <- data.table(
  date = seq(
    as.POSIXct("1999-07-01 00:00:00", tz = "UTC"),
    as.POSIXct("2000-06-30 23:00:00", tz = "UTC"),
    "1 hour"
  ),
  value = as.numeric(seq_len(8784L)),
  year      = rep(  1:2        , daysPerHalfyear * 24L),
  quarter   = rep(c(3:4  , 1:2), daysPerQuarter  * 24L),
  month     = rep(c(7:12 , 1:6), daysPerMonth    * 24L),
  yearMonth = rep(  7:18       , daysPerMonth    * 24L),
  day       = rep(  1:366                              , each = 24L),
  monthDay  = rep(unlist(lapply(daysPerMonth, seq_len)), each = 24L)
)

UTCfractionalSecondData <- data.table(
  date = seq(
    as.POSIXct("2000-03-26 01:00:00.0", tz = "UTC"),
    as.POSIXct("2000-03-26 03:59:59.5", tz = "UTC"),
    0.5
  ),
  value = as.numeric(seq_len(21600L)),
  hour         = rep(1:3     , each = 7200L),
  minute       = rep(0:179   , each =  120L),
  second       = rep(0:10799 , each =    2L),
  hourMinute   = rep(rep(0:59, each =  120L),   3L),
  minuteSecond = rep(rep(0:59, each =    2L), 180L)
)

#### CET ####
CEThourlyData <- data.table(
  date = seq(
    as.POSIXct("1999-07-01 01:00:00", tz = "Europe/Vienna"),
    as.POSIXct("2000-07-01 00:00:00", tz = "Europe/Vienna"),
    "1 hour"
  ),
  value = as.numeric(seq_len(8784L)),
  year      = rep(  1:2        , daysPerHalfyear * 24L),
  quarter   = rep(c(3:4  , 1:2), daysPerQuarter  * 24L),
  month     = rep(c(7:12 , 1:6), daysPerMonth    * 24L),
  yearMonth = rep(  7:18       , daysPerMonth    * 24L),
  day       = rep(  1:366                              , each = 24L),
  monthDay  = rep(unlist(lapply(daysPerMonth, seq_len)), each = 24L)
)

CETtoDSTfractionalSecondData <- data.table(
  date = seq(
    as.POSIXct("2000-03-26 01:00:00.0", tz = "Europe/Vienna"),
    as.POSIXct("2000-03-26 03:59:59.5", tz = "Europe/Vienna"),
    0.5
  ),
  value = as.numeric(seq_len(14400L)),
  hour         = rep(1:2     , each = 7200L),
  minute       = rep(0:119   , each =  120L),
  second       = rep(0:7199  , each =    2L),
  hourMinute   = rep(rep(0:59, each =  120L),   2L),
  minuteSecond = rep(rep(0:59, each =    2L), 120L)
)

CETfromDSTfractionalSecondData <- data.table(
  date = seq(
    as.POSIXct("2000-10-29 02:00:00.0", tz = "Europe/Vienna"),
    as.POSIXct("2000-10-29 03:59:59.5", tz = "Europe/Vienna"),
    0.5
  ),
  value = as.numeric(seq_len(21600L)),
  hour         = rep(1:3     , each = 7200L),
  minute       = rep(0:179   , each =  120L),
  second       = rep(0:10799 , each =    2L),
  hourMinute   = rep(rep(0:59, each =  120L),   3L),
  minuteSecond = rep(rep(0:59, each =    2L), 180L)
)
