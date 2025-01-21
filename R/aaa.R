#' @import checkmate
#' @import data.table
#' @importFrom methods setClass setMethod
#' @import R6
#' @import timechange
NULL

.onLoad <- function(libname, pkgname) {
  suppressWarnings(onCRAN <- isTRUE(as.logical(as.numeric( # nolint
    Sys.getenv("_R_CHECK_EXAMPLE_TIMING_CPU_TO_ELAPSED_THRESHOLD_")
  ))))
  if (onCRAN) {
    setDTthreads(2L)
  }

  getOption("DTSgClone"             , options(DTSgClone              = TRUE        ))
  getOption("DTSgDeprecatedWarnings", options(DTSgDeprecatedWarnings = TRUE        ))
  getOption("DTSgFast"              , options(DTSgFast               = FALSE       ))
  getOption("DTSgFunbyApproach"     , options(DTSgFunbyApproach      = "timechange"))
  getOption("DTSgNA.status"         , options(DTSgNA.status          = "explicit"  ))
}
