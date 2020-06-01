# no R CMD check notes
.dateTime <- NULL
.colAfter <- NULL
.colBefore <- NULL
.dateTimeAfter <- NULL
.dateTimeBefore <- NULL
.divisor <- NULL

#' Linear Interpolation
#'
#' Linearly interpolates missing values of a numeric vector. For use with the
#'  \code{\link{colapply}} method of a \code{\link{DTSg}} object. Other uses are
#'  possible, but not recommended. It also serves as an example for writing user
#'  defined functions utilising one of the \code{\link{list}}s with helper data
#'  as handed over by various methods of \code{\link{DTSg}} objects. See
#'  \code{\link{DTSg}} for further information.
#'
#' @param .col A numeric vector.
#' @param roll A positive numeric specifying the maximum size of gaps whose
#'  missing values shall be filled. For time series with unrecognised
#'  periodicity it is interpreted as seconds and for time series with recognised
#'  periodicity it is multiplied with the maximum time difference between two
#'  subsequent time steps in seconds. So for regular time series it is the
#'  number of time steps and for irregular it is an approximation of it.
#' @param rollends A logical specifying if missing values at the start and end
#'  of the time series shall be filled as well. See
#'  \code{\link[data.table]{data.table}} for further information.
#' @param .helpers A \code{\link{list}} with helper data as handed over by
#'  \code{\link{colapply}}. See \code{\link{colapply}} for further information.
#'
#' @return Returns a numeric vector.
#'
#' @seealso \code{\link{DTSg}}, \code{\link{colapply}},
#'  \code{\link[data.table]{data.table}}
#'
#' @examples
#' # new DTSg object
#' x <- DTSg$new(values = flow)
#'
#' # linear interpolation of missing values
#' ## R6 method
#' x$colapply(fun = interpolateLinear)
#'
#' ## S3 method
#' colapply(x = x, fun = interpolateLinear)
#'
#' @export
interpolateLinear <- function(.col, roll = Inf, rollends = TRUE, .helpers) {
  qassert(.col, "n+")
  qassert(roll, "N1(0,]")

  if (.helpers$periodicity != "unrecognised") {
    roll <- roll * as.numeric(.helpers$maxLag, units = "secs")
  }

  DT <- data.table(.dateTime = .helpers$.dateTime, key = ".dateTime")
  values <- data.table(.dateTime = .helpers$.dateTime, .col = .col, key = ".dateTime")
  values <- values[!is.na(.col), ]

  DT <- values[
    DT,
    c(".dateTime", sprintf("x.%s", c(".dateTime", ".col"))),
    with = FALSE,
    roll = roll,
    rollends = rollends
  ]
  setnames(DT, c(2L, 3L), c(".dateTimeBefore", ".colBefore"))

  DT <- values[
    DT,
    c(names(DT), sprintf("x.%s", c(".dateTime", ".col"))),
    with = FALSE,
    roll = -roll,
    rollends = rollends
  ]
  setnames(DT, c(4L, 5L), c(".dateTimeAfter", ".colAfter"))

  DT[, .divisor := as.numeric(.dateTimeAfter - .dateTimeBefore, units = "secs")]
  DT[, .col := .colBefore]
  DT[
    .divisor > 0,
    .col := .colBefore + as.numeric(.dateTime - .dateTimeBefore, units = "secs") *
      (.colAfter - .colBefore) / .divisor
  ]

  DT[[".col"]]
}
