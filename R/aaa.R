#' @import checkmate
#' @import data.table
#' @import methods
#' @import R6
NULL

.onLoad <- function(libname, pkgname) {
  getOption("DTSgClone", options(DTSgClone = TRUE))
}
