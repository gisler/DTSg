#' Daily river flows
#'
#' A dataset containing a fictional time series of daily river flows with
#' implicitly missing values.
#'
#' @format
#' A [`data.table::data.table`] with 2169 rows and two columns:
#' \describe{
#'   \item{date}{A [`POSIXct`] vector ranging from the start of the year 2007 to
#'   the end of the year 2012.}
#'   \item{flow}{A numeric vector with daily river flows in cubic metres per
#'   second.}
#' }
"flow"
