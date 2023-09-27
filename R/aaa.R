#' @import checkmate
#' @import data.table
#' @import methods
#' @import R6
NULL

.onLoad <- function(libname, pkgname) {
  suppressWarnings(onCRAN <- isTRUE(as.logical(as.numeric(
    Sys.getenv("_R_CHECK_EXAMPLE_TIMING_CPU_TO_ELAPSED_THRESHOLD_")
  ))))
  if(onCRAN) {
    setDTthreads(2L)
  }

  getOption("DTSgClone"             , options(DTSgClone              = TRUE      )) # nolint
  getOption("DTSgDeprecatedWarnings", options(DTSgDeprecatedWarnings = TRUE      )) # nolint
  getOption("DTSgFast"              , options(DTSgFast               = FALSE     )) # nolint
  getOption("DTSgFunbyApproach"     , options(DTSgFunbyApproach      = "base"    )) # nolint
  getOption("DTSgNA.status"         , options(DTSgNA.status          = "explicit")) # nolint
}
