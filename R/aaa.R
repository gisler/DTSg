#' @import checkmate
#' @import data.table
#' @import methods
#' @import R6
NULL

.onLoad <- function(libname, pkgname) {
  getOption("DTSgClone"             , options(DTSgClone              = TRUE      )) # nolint
  getOption("DTSgDeprecatedWarnings", options(DTSgDeprecatedWarnings = TRUE      )) # nolint
  getOption("DTSgFast"              , options(DTSgFast               = FALSE     )) # nolint
  getOption("DTSgFunbyApproach"     , options(DTSgFunbyApproach      = "base"    )) # nolint
  getOption("DTSgNA.status"         , options(DTSgNA.status          = "explicit")) # nolint
}
