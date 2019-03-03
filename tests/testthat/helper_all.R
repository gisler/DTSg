DT1 <- data.table::data.table(
  date = seq(
    as.POSIXct("2000-10-29 01:00:00", tz = "Europe/Vienna"),
    as.POSIXct("2000-10-29 03:30:00", tz = "Europe/Vienna"),
    "30 mins"
  ),
  col1 = seq(1, 15, by = 2),
  col2 = c(1, NA, seq(5, 15, by = 2)),
  col3 = LETTERS[1:8]
)

DT2 <- data.table::data.table(
  date = seq(
    as.POSIXct("2000-10-29 01:00:00", tz = "Europe/Vienna"),
    as.POSIXct("2000-10-29 02:00:00", tz = "Europe/Vienna"),
    "30 mins"
  ),
  col1 = c(NA, NA, 1),
  col2 = rep(NA, 3L),
  col3 = letters[1:3]
)
