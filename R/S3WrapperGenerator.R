#' S3 wrapper method generator
#'
#' Generates S3 wrapper methods for public methods of `R6ClassGenerator`s, but
#' can also be used to generate \dQuote{plain} function wrappers.
#'
#' @param R6Method An [`expression`] with or a public method ([`function`]) of
#'   an `R6ClassGenerator`.
#' @param self A character string specifying the name of the parameter, which
#'   will take the R6 object.
#' @param dots A logical specifying if a `...` parameter shall be added as last
#'   parameter in case none already exists. This might be required for S3
#'   generic/method consistency.
#'
#' @return Returns an S3 method ([`function`]).
#'
#' @seealso [`S3Methods`], [`R6::R6Class`]
#'
#' @examples
#' # generate an S3 wrapper method for 'alter' of 'DTSg'
#' alter.DTSg <- S3WrapperGenerator(
#'   R6Method = DTSg$public_methods$alter
#' )
#'
#' @export
S3WrapperGenerator <- function(R6Method, self = "x", dots = TRUE) {
  if (is.function(R6Method)) {
    R6Method <- as.expression(substitute(R6Method))
  }
  if (!is.expression(R6Method) ||
      R6Method[[1L]][[2L]][[3L]] != "public_methods" ||
      class(eval(R6Method[[1L]][[2L]][[2L]])) != "R6ClassGenerator") {
    stop(
      '"R6Method" must contain a public method of an "R6ClassGenerator".',
      call. = FALSE
    )
  }
  qassert(self, "S1")
  qassert(dots, "B1")

  args <- list()
  args[[self]] <- alist(`self` = )$`self`
  args <- c(args, formals(eval(R6Method)))

  if (dots && !any(names(args) == "...")) {
    args[["..."]] <- alist(... = )$...
  }

  S3Method <- function() {
    call <- match.call()
    call[[1L]] <- call("$", as.name(self), R6Method[[1L]][[3L]])
    call[[2L]] <- NULL

    eval(call)
  }

  formals(S3Method) <- args

  S3Method
}
