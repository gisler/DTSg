byFasttimeWarning <- function() {
  if (isTRUE(getOption("DTSgDeprecatedWarnings"))) {
    warning(
      '"byFasttime*" TALFs are deprecated. Use "funbyApproach" argument instead.',
      call. = FALSE
    )
  }

  if (!requireNamespace("fasttime", quietly = TRUE)) {
    stop('Package "fasttime" must be installed for this TALF.', call. = FALSE)
  }
}

## Truncating family ####
#' @rdname TALFs
#' @export
byFasttimeY_____ <- function(.dateTime, .helpers) {
  byFasttimeWarning()
  assertFasttimeOK(.dateTime, .helpers)

  singleOrMulti <- if (.helpers[["multiplier"]] == 1L) "single" else "multi"

  eval(byExpr[[singleOrMulti]][["fasttime"]][["byY_____"]])
}

#' @rdname TALFs
#' @export
byFasttimeYQ____ <- function(.dateTime, .helpers) {
  byFasttimeWarning()
  assertFasttimeOK(.dateTime, .helpers)

  eval(byExpr[["single"]][["fasttime"]][["byYQ____"]])
}

#' @rdname TALFs
#' @export
byFasttimeYm____ <- function(.dateTime, .helpers) {
  byFasttimeWarning()
  assertFasttimeOK(.dateTime, .helpers)

  singleOrMulti <- if (.helpers[["multiplier"]] == 1L) "single" else "multi"

  eval(byExpr[[singleOrMulti]][["fasttime"]][["byYm____"]])
}

#' @rdname TALFs
#' @export
byFasttimeYmd___ <- function(.dateTime, .helpers) {
  byFasttimeWarning()
  assertFasttimeOK(.dateTime, .helpers)

  eval(byExpr[["single"]][["fasttime"]][["byYmd___"]])
}

#' @rdname TALFs
#' @export
byFasttimeYmdH__ <- function(.dateTime, .helpers) {
  byFasttimeWarning()
  assertFasttimeOK(.dateTime, .helpers)

  singleOrMulti <- if (.helpers[["multiplier"]] == 1L) "single" else "multi"

  eval(byExpr[[singleOrMulti]][["fasttime"]][["byYmdH__"]])
}

#' @rdname TALFs
#' @export
byFasttimeYmdHM_ <- function(.dateTime, .helpers) {
  byFasttimeWarning()
  assertFasttimeOK(.dateTime, .helpers)

  singleOrMulti <- if (.helpers[["multiplier"]] == 1L) "single" else "multi"

  eval(byExpr[[singleOrMulti]][["fasttime"]][["byYmdHM_"]])
}

#' @rdname TALFs
#' @export
byFasttimeYmdHMS <- function(.dateTime, .helpers) {
  byFasttimeWarning()
  assertFasttimeOK(.dateTime, .helpers)

  singleOrMulti <- if (.helpers[["multiplier"]] == 1L) "single" else "multi"

  eval(byExpr[[singleOrMulti]][["fasttime"]][["byYmdHMS"]])
}

## Extracting family ####
#' @rdname TALFs
#' @export
byFasttime______ <- function(.dateTime, .helpers) {
  byFasttimeWarning()
  assertFasttimeOK(.dateTime, .helpers)

  eval(byExpr[["single"]][["fasttime"]][["by______"]])
}

#' @rdname TALFs
#' @export
byFasttime_Q____ <- function(.dateTime, .helpers) {
  byFasttimeWarning()
  assertFasttimeOK(.dateTime, .helpers)

  eval(byExpr[["single"]][["fasttime"]][["by_Q____"]])
}

#' @rdname TALFs
#' @export
byFasttime_m____ <- function(.dateTime, .helpers) {
  byFasttimeWarning()
  assertFasttimeOK(.dateTime, .helpers)

  singleOrMulti <- if (.helpers[["multiplier"]] == 1L) "single" else "multi"

  eval(byExpr[[singleOrMulti]][["fasttime"]][["by_m____"]])
}

#' @rdname TALFs
#' @export
byFasttime___H__ <- function(.dateTime, .helpers) {
  byFasttimeWarning()
  assertFasttimeOK(.dateTime, .helpers)

  singleOrMulti <- if (.helpers[["multiplier"]] == 1L) "single" else "multi"

  eval(byExpr[[singleOrMulti]][["fasttime"]][["by___H__"]])
}

#' @rdname TALFs
#' @export
byFasttime____M_ <- function(.dateTime, .helpers) {
  byFasttimeWarning()
  assertFasttimeOK(.dateTime, .helpers)

  singleOrMulti <- if (.helpers[["multiplier"]] == 1L) "single" else "multi"

  eval(byExpr[[singleOrMulti]][["fasttime"]][["by____M_"]])
}

#' @rdname TALFs
#' @export
byFasttime_____S <- function(.dateTime, .helpers) {
  byFasttimeWarning()
  assertFasttimeOK(.dateTime, .helpers)

  singleOrMulti <- if (.helpers[["multiplier"]] == 1L) "single" else "multi"

  eval(byExpr[[singleOrMulti]][["fasttime"]][["by_____S"]])
}
