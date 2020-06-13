#' S3 Wrapper Method Generator
#'
#' Generates S3 wrapper methods for public methods of \code{R6ClassGenerator}s,
#'  but can also be used to generate \dQuote{plain} function wrappers.
#'
#' @param R6Method An \code{\link{expression}} with or a public method
#'  (\code{\link{function}}) of an \code{R6ClassGenerator}.
#' @param self A character string specifying the name of the parameter which
#'  will take the R6 object.
#' @param dots A logical specifying if a \code{\dots} parameter shall be added
#'  as last parameter in case none already exists. This might be required for S3
#'  generic/method consistency.
#'
#' @return Returns an S3 method (\code{\link{function}}).
#'
#' @seealso \code{\link{S3Methods}}, \code{\link[R6]{R6Class}},
#'  \code{\link{expression}}, \code{\link{function}}
#'
#' @examples
#' # generate S3 wrapper method for alter of DTSg
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
