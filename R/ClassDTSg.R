#### Documentation ####
#' DTSg Class
#'
#' The \code{DTSg} class is the working horse of the package. It is an
#'  \code{\link[R6]{R6Class}} and offers an S3 interface in addition to its
#'  native R6 interface. In the usage sections of the documentation only the S3
#'  interface is shown, however, the examples always show both possibilities.
#'  Generally, they are very similar anyway. While the R6 interface always has
#'  the object first and the method is selected with the help of the \code{$}
#'  operator (for instance, \code{x$cols()}), the S3 interface always has the
#'  method first and the object as its first argument (for instance,
#'  \code{cols(x)}). An exception is the \code{new} method. It is not an S3
#'  method, but an abused S4 constructor with the character string \code{"DTSg"}
#'  as its first argument. Regarding the R6 interface, the \code{DTSg} class
#'  generator has to be used to access the \code{new} method with the help of
#'  the \code{$} operator.
#'
#' @usage new(Class, values, ID = "", parameter = "", unit = "", variant = "",
#'  aggregated = FALSE, fast = FALSE, swallow = FALSE, na.status = c("explicit",
#'  "implicit", "undecided"))
#'
#' @param Class A character string. Must be \code{"DTSg"} in order to create a
#'  \code{DTSg} object. Otherwise a different object may or may not be created
#'  (S4 constructor only).
#' @param values A \code{\link{data.frame}} or object inherited from class
#'  \code{\link{data.frame}}, for instance,
#'  \code{\link[data.table]{data.table}}. Its first column must be of class
#'  \code{\link{POSIXct}} or coercible to it. It serves as the object's time
#'  index and is renamed to \emph{.dateTime.}
#' @param ID A character string specifying the ID (name) of the time series.
#' @param parameter A character string specifying the parameter of the time
#'  series.
#' @param unit A character string specifying the unit of the time series.
#' @param variant A character string specifying further metadata of the time
#'  series, for instance, \code{"min"} to point out that it is a time series of
#'  lower bound measurements.
#' @param aggregated A logical signalling how the timestamps of the series have
#'  to be interpreted: as snap-shots (\code{FALSE}) or as periods between
#'  subsequent timestamps (\code{TRUE}).
#' @param fast A logical signalling if all rows (\code{FALSE}) or only the first
#'  1000 rows (\code{TRUE}) shall be used to check the object's integrity and
#'  for the automatic detection of the time series' periodicity.
#' @param swallow A logical signalling if the object provided through the
#'  \code{values} argument shall be \dQuote{swallowed} by the \code{DTSg}
#'  object, i.e. no copy of the data shall be made. This is generally more
#'  resource efficient, but only works if the object provided through the
#'  \code{values} argument is a \code{\link[data.table]{data.table}}. Be warned,
#'  however, that if the creation of the \code{DTSg} object fails for some
#'  reason, the first column of the provided
#'  \code{\link[data.table]{data.table}} might have been coerced to
#'  \code{\link{POSIXct}} and keyed (see \code{\link[data.table]{setkey}} for
#'  further information). Furthermore, all references to the \dQuote{swallowed}
#'  \code{\link[data.table]{data.table}} in the global (and only the global)
#'  environment are removed upon successful creation of a \code{DTSg} object.
#' @param na.status A character string. Either \code{"explicit"}, which makes
#'  missing timestamps according to the recognised periodicity explicit, or
#'  \code{"implicit"}, which removes timestamps with missing values on all value
#'  columns, or \code{"undecided"} for no such action. Please note that
#'  \code{\link{DTSg}} objects work best with explicitly missing values.
#'
#' @return Returns a \code{DTSg} object.
#'
#' @section Methods:
#' A \code{DTSg} object has the following methods:
#'  \itemize{
#'    \item \code{aggregate}: See \code{\link{aggregate}} for further
#'      information.
#'    \item \code{alter}: See \code{\link{alter}} for further information.
#'    \item \code{clone}: See \code{\link{clone}} for further information.
#'    \item \code{colapply}: See \code{\link{colapply}} for further information.
#'    \item \code{cols}: See \code{\link{cols}} for further information.
#'    \item \code{getCol}: See \code{\link{getCol}} for further information.
#'    \item \code{merge}: See \code{\link{merge}} for further information.
#'    \item \code{nas}: See \code{\link{nas}} for further information.
#'    \item \code{plot}: See \code{\link{plot}} for further information.
#'    \item \code{print}: See \code{\link{print}} for further information.
#'    \item \code{refresh}: See \code{\link{refresh}} for further information.
#'    \item \code{rollapply}: See \code{\link{rollapply}} for further
#'      information.
#'    \item \code{rowapply}: See \code{\link{rowapply}} for further information.
#'    \item \code{rowbind}: See \code{\link{rowbind}} for further information.
#'    \item \code{setCols}: See \code{\link{setCols}} for further information.
#'    \item \code{subset}: See \code{\link{subset}} for further information.
#'    \item \code{summary}: See \code{\link{summary}} for further information.
#'    \item \code{values}: See \code{\link{values}} for further information.
#'  }
#'
#' @section Fields:
#' A \code{DTSg} object has the following fields or properties as they are often
#'  called. They are implemented through so called active bindings which means
#'  that they can be accessed and actively set with the help of the \code{$}
#'  operator (for instance, \code{x$ID} gets the value of the \emph{ID} field
#'  and \code{x$ID <- "River Flow"} sets its value). Please note that fields are
#'  always modified in place, i.e. no clone (copy) of the object is made
#'  beforehand. See \code{\link{clone}} for further information. Some of the
#'  fields are read-only though:
#'  \itemize{
#'    \item \emph{aggregated:} Same as \code{aggregated} argument.
#'    \item \emph{fast:} Same as \code{fast} argument.
#'    \item \emph{ID:} Same as \code{ID} argument. It is used as the title of
#'      plots.
#'    \item \emph{na.status:} Same as \code{na.status} argument. When set, the
#'      \emph{values} of the object are expanded or collapsed accordingly.
#'    \item \emph{parameter:} Same as \code{parameter} argument. It is used as
#'      the label of the primary axis of plots.
#'    \item \emph{periodicity:} A \code{\link{difftime}} object for a regular
#'      and a character string for an irregular \code{DTSg} object describing
#'      its periodicity or containing \code{"unrecognised"} in case it could not
#'      be detected. When set, the periodicity of the time series is changed as
#'      specified. See \code{by} argument of \code{\link{alter}} for further
#'      information.
#'    \item \emph{regular:} A logical signalling if all lags in seconds between
#'      subsequent timestamps are the same (\code{TRUE}) or if some are
#'      different (\code{FALSE}). A, for instance, monthly time series is
#'      considered irregular in this sense (read-only).
#'    \item \emph{timestamps:} An integer showing the total number of timestamps
#'      of the time series (read-only).
#'    \item \emph{timezone:} A character string containing the time zone of the
#'      time series. When set, the series is converted to the specified time
#'      zone. Only names from \code{\link{OlsonNames}} are accepted.
#'    \item \emph{unit:} Same as \code{unit} argument. It is added to the label
#'      of the primary axis of plots if the \emph{parameter} field is set.
#'    \item \emph{variant:} Same as \code{variant} argument. It is added to the
#'      label of the primary axis of plots if the \emph{parameter} field is set.
#'  }
#'
#' The \emph{parameter}, \emph{unit} and \emph{variant} fields are especially
#'  useful for time series with one variable (value column) only.
#'
#' @section Options:
#' The behaviour of \code{DTSg} objects can be customised with the help of the
#'  following option. See \code{\link{options}} for further information:
#'  \itemize{
#'    \item \emph{DTSgClone:} A logical specifying if \code{DTSg} objects are,
#'      by default, modified in place (\code{FALSE}) or if a clone (copy) is
#'      made beforehand (\code{TRUE}).
#'  }
#'
#' @note
#' Due to the \code{\link{POSIXct}} nature of the \emph{.dateTime} column, the
#'  same sub-second accuracy, issues and limitations apply to \code{DTSg}
#'  objects. In order to prevent at least some of the possible precision issues,
#'  the lags in seconds between subsequent timestamps are rounded to
#'  microseconds during integrity checks. This corresponds to the maximum value
#'  allowed in \code{\link{options}("digits.secs")}. As a consequence, time
#'  series with a sub-second accuracy higher than a microsecond will never work.
#'
#' Some of the methods which take a \code{\link{function}} as an argument
#'  (\code{\link{colapply}} and \code{\link{rollapply}}) hand over to it an
#'  additional \code{\link{list}} argument called \code{.helpers} containing
#'  useful data for the development of user defined functions (see the
#'  respective help pages for further information). This can of course be a
#'  problem for functions like \code{\link{sum}} which do not expect such a
#'  thing. A solution is to wrap it in an anonymous function with a
#'  \code{\dots} parameter like this: \code{function(x, ...) sum(x)}.
#'
#' @seealso \code{\link[R6]{R6Class}}, \code{\link{data.frame}},
#'  \code{\link[data.table]{data.table}}, \code{\link{POSIXct}},
#'  \code{\link[data.table]{setkey}}, \code{\link{difftime}},
#'  \code{\link{OlsonNames}}, \code{\link{options}}, \code{\link{function}},
#'  \code{\link{list}}
#'
#' @examples
#' # new DTSg object
#' ## R6 constructor
#' DTSg$new(values = flow, ID = "River Flow")
#'
#' ## S4 constructor
#' new(Class = "DTSg", values = flow, ID = "River Flow")
#'
#' @docType class
#'
#' @aliases new
#'
#' @export
DTSg <- R6Class(
  classname = "DTSg",

  #### Private ####
  private = list(
    .ID = character(),
    .isAggregated = logical(),
    .isFast = logical(),
    .isRegular = logical(),
    .maxLag = .difftime(0, units = "secs"),
    .minLag = .difftime(0, units = "secs"),
    .na.status = "undecided",
    .na.statuses = c("explicit", "implicit", "undecided"),
    .origDateTimeCol = character(),
    .parameter = character(),
    .periodicity = NULL,
    .timestamps = integer(),
    .timezone = character(),
    .unit = character(),
    .values = data.table(),
    .variant = character(),

    coerceCol = function(x, fun, ..., colname) {
      toClass <- substring(deparse(substitute(fun)), 4L)
      msgPart <- sprintf("column %s to class %s", deparse(colname), deparse(toClass))

      x <- tryCatch(
        fun(x, ...),
        error = function(e) {
          stop(
            sprintf("Cannot coerce %s because %s.", msgPart, deparse(e$message)),
            call. = FALSE
          )
        },
        warning = function(w) {
          stop(
            sprintf("Will not coerce %s because %s.", msgPart, deparse(w$message)),
            call. = FALSE
          )
        }
      )

      warning(sprintf("Coerced %s.", msgPart), call. = FALSE)

      x
    },

    deep_clone = function(name, value) {
      if (name == ".values") {
        copy(value)
      } else {
        value
      }
    },

    determineCols = function(resultCols, suffix, cols) {
      if (!is.null(resultCols)) {
        assertCharacter(
          resultCols,
          min.chars = 1L,
          any.missing = FALSE,
          len = length(cols),
          unique = TRUE
        )

        assertNoBeginningDot(resultCols)
      } else if (!is.null(suffix)) {
        qassert(suffix, "S1")
        assertDisjunct(sprintf("%s%s", cols, suffix), self$cols())

        sprintf("%s%s", cols, suffix)
      } else {
        cols
      }
    },

    determineLen = function(timestamps) {
      if (!private$.isFast || timestamps < 1000L) {
        timestamps
      } else {
        1000L
      }
    },

    determineFilter = function(i, expr) {
      tryCatch(
        {
          if (!testMultiClass(i, c("integer", "numeric")) &&
              !is.list(i) && !is.expression(i)) {
            i <- expr
          }

          i
        },
        error = function(e) {
          expr
        }
      )
    },

    determineFun = function(fun, isNames) {
      if (!testClass(fun, "list")) {
        fun <- list(fun)
      }
      lapply(fun, assertFunction, .var.name = "fun' or 'fun[[i]]")
      if (isNames && length(fun) > 1L) {
        assertCharacter(names(fun), min.chars = 1L, any.missing = FALSE, unique = TRUE)
      }

      fun
    },

    determineFrom = function(from) {
      if (qtest(from, "P1")) {
        assertSetEqual(attr(from, "tzone"), self$timezone)
      } else {
        from <- as.POSIXct(from, tz = private$.timezone)
        qassert(from, "P1")
      }

      from
    },

    determineTo = function(to, from) {
      if (qtest(to, "P1")) {
        assertSetEqual(attr(to, "tzone"), self$timezone)
      } else {
        to <- as.POSIXct(to, tz = private$.timezone)
      }

      assertPOSIXct(to, lower = from, any.missing = FALSE, len = 1L)
    },

    funbyHelpers = function(ignoreDST, .helpers) {
      elements <- names(.helpers)

      if (any(elements %chin% c("timezone", "periodicity", "na.status"))) {
        stop(
          '"timezone", "periodicity" and "na.status" elements are not allowed in this context.',
          call. = FALSE
        )
      }

      if ("ignoreDST" %chin% elements) {
        qassert(
          .helpers[["ignoreDST"]],
          "B1",
          .var.name = 'funbyHelpers[["ignoreDST"]]'
        )

        ignoreDST <- .helpers[["ignoreDST"]]
        .helpers[["ignoreDST"]] <- NULL
      }

      c(list(
        timezone = private$.timezone,
        ignoreDST = ignoreDST,
        periodicity = private$.periodicity,
        na.status = private$.na.status
      ), .helpers)
    },

    multiLapply = function(.SD, funs, ...) {
      do.call(c, lapply(
        .SD,
        function(x, ...) {
          lapply(funs, function(fun, y, ...) {fun(y, ...)}, y = x, ... = ...)
        },
        ... = ...
      ))
    },

    rmGlobalReferences = function(addr) {
      globalObjs <- ls(globalenv(), sorted = FALSE)

      rmGlobalReferences <- function(globalObj, addr) {
        if (addr == address(get(globalObj, envir = globalenv()))) {
          rm(list = globalObj, envir = globalenv())
        }
      }

      lapply(globalObjs, rmGlobalReferences, addr = addr)
    }
  ),

  #### Public ####
  public = list(
    aggregate = function(
      funby,
      fun,
      ...,
      cols = self$cols(class = "numeric"),
      n = FALSE,
      ignoreDST = FALSE,
      clone = getOption("DTSgClone")
    ) {
      assertFunction(funby)
      qassert(ignoreDST, "B1")
      .funbyHelpers <- private$funbyHelpers(ignoreDST, list())
      qassert(funby(
        self$values(reference = TRUE)[[".dateTime"]][1L],
        .funbyHelpers
      ), "P1")
      fun <- private$determineFun(fun, length(fun) == length(cols))
      assertCharacter(cols, any.missing = FALSE, min.len = 1L, unique = TRUE)
      assertSubset(cols, self$cols())
      qassert(n, "B1")
      qassert(clone, "B1")

      if (clone) {
        TS <- self$clone(deep = TRUE)
        return(TS$aggregate(
          funby = funby,
          fun = fun,
          ... = ...,
          cols = cols,
          n = n,
          ignoreDST = ignoreDST,
          clone = FALSE
        ))
      }

      if (n) {
        if (length(cols) > 1L) {
          private$.values <- private$.values[
            ,
            c(private$multiLapply(.SD, fun, ...), .(.n = .N)),
            keyby = .(.dateTime = funby(.dateTime, .funbyHelpers)),
            .SDcols = cols
          ]

          message(".n column calculated from .dateTime column.")
        } else {
          private$.values <- private$.values[
            !is.na(get(cols)),
            c(private$multiLapply(.SD, fun, ...), .(.n = .N)),
            keyby = .(.dateTime = funby(.dateTime, .funbyHelpers)),
            .SDcols = cols
          ]

          message(
            'Missing values are always stripped regardless of the value of a possible "na.rm" argument.'
          )
        }
      } else {
        private$.values <- private$.values[
          ,
          private$multiLapply(.SD, fun, ...),
          keyby = .(.dateTime = funby(.dateTime, .funbyHelpers)),
          .SDcols = cols
        ]
      }

      private$.isAggregated <- TRUE

      self$refresh()
      self$alter(clone = FALSE)

      invisible(self)
    },

    alter = function(
      from = first(self$values(reference = TRUE)[[".dateTime"]]),
      to = last(self$values(reference = TRUE)[[".dateTime"]]),
      by = self$periodicity,
      rollback = TRUE,
      clone = getOption("DTSgClone"),
      na.status = self$na.status
    ) {
      from <- private$determineFrom(from)
      to <- private$determineTo(to, from)
      qassert(rollback, "B1")
      qassert(clone, "B1")
      na.status <- match.arg(na.status, private$.na.statuses)

      if (clone) {
        TS <- self$clone(deep = TRUE)
        return(TS$alter(
          from = from,
          to = to,
          by = by,
          rollback = rollback,
          clone = FALSE,
          na.status = na.status
        ))
      }

      if ((by != private$.periodicity || na.status == "explicit") &&
          by != "unrecognised") {
        if (rollback && grepl("^\\d+ (month|year)(s?)$", by) && mday(from) > 28L) {
          DT <- data.table(
            .dateTime = rollback(seq(
              from,
              to + diff(seq(to, by = "1 DSTday", length.out = 2L)),
              by
            ), by),
            key = ".dateTime"
          )
        } else {
          DT <- data.table(.dateTime = seq(from, to, by), key = ".dateTime")
        }

        if (by != private$.periodicity || nrow(DT) != private$.timestamps) {
          private$.values <- private$.values[DT, ]

          self$refresh()
        }

        private$.na.status <- na.status
      } else if (by != private$.periodicity && by == "unrecognised") {
        stop(
          'Periodicity of the time series cannot be changed to "unrecognised".',
          call. = FALSE
        )
      } else if (na.status == "explicit" && by == "unrecognised" &&
                 private$.timestamps > 2L) {
        warning(
          paste(
            "Only time series with recognised periodicity can have explicitly missing values.",
            'Consider calling "alter()" with "na.status = \'explicit\'" and specified "by" argument.',
            sep = "\n"
          ),
          call. = FALSE
        )
      }

      if (na.status == "implicit") {
        allNA <- rowSums(is.na(private$.values[, -1L, with = FALSE])) ==
          ncol(private$.values) - 1L

        if (any(allNA)) {
          private$.values <- private$.values[!allNA, ]

          self$refresh()
        }

        private$.na.status <- na.status
      } else if (na.status == "undecided" && private$.na.status != "undecided") {
        stop("Status of missing values has already been decided on.", call. = FALSE)
      }

      invisible(self)
    },

    colapply = function(
      fun,
      ...,
      cols = self$cols(class = "numeric")[1L],
      resultCols = NULL,
      suffix = NULL,
      funby = NULL,
      ignoreDST = FALSE,
      clone = getOption("DTSgClone")
    ) {
      assertFunction(fun)
      assertCharacter(cols, any.missing = FALSE, min.len = 1L, unique = TRUE)
      assertSubset(cols, self$cols())
      .cols <- private$determineCols(resultCols, suffix, cols)
      qassert(clone, "B1")

      if (clone) {
        TS <- self$clone(deep = TRUE)
        return(TS$colapply(
          fun = fun,
          ... = ...,
          cols = cols,
          resultCols = resultCols,
          suffix = suffix,
          funby = funby,
          ignoreDST = ignoreDST,
          clone = FALSE
        ))
      }

      .helpers <- list(
        .dateTime = private$.values[[".dateTime"]],
        periodicity = private$.periodicity,
        minLag = private$.minLag,
        maxLag = private$.maxLag
      )

      if (!is.null(funby)) {
        assertFunction(funby)
        qassert(ignoreDST, "B1")
        .funbyHelpers <- private$funbyHelpers(ignoreDST, list())
        assertAtomic(funby(
          self$values(reference = TRUE)[[".dateTime"]][1L],
          .funbyHelpers
        ), any.missing = FALSE, len = 1L)

        by <- funby(private$.values[[".dateTime"]], .funbyHelpers)
      } else {
        by <- NULL
      }

      private$.values[
        ,
        (.cols) := lapply(
          .SD,
          fun,
          ...,
          .helpers = .helpers
        ),
        by = by,
        .SDcols = cols
      ]

      invisible(self)
    },

    cols = function(class = NULL, pattern = NULL, ...) {
      cols <- names(private$.values)[-1L]

      if (is.character(class) && length(class) == 1L && class == "all") {
        warning(
          paste(
            '"class = \'all\'" is deprecated.',
            "Please use argument's default value NULL to get all column names.",
            sep = "\n"
          ),
          call. = FALSE
        )
      } else if (!is.null(class)) {
        qassert(class, "S+")

        classes <- vapply(
          private$.values[, -1L, with = FALSE],
          function(col) {class(col)[1L]},
          character(1L)
        )

        cols <- cols[classes %chin% class]
      }

      if (!is.null(pattern)) {
        if (any(names(list(...)) %chin% c("x", "value"))) {
          stop(
            '"x" and "value" arguments are not allowed in this context.',
            call. = FALSE
          )
        }

        cols <- grep(pattern, cols, value = TRUE, ...)
      }

      cols
    },

    getCol = function(col = self$cols(class = "numeric")[1L]) {
      qassert(col, "S1")
      assertSubset(col, c(".dateTime", self$cols()))

      private$.values[[col]]
    },

    initialize = function(
      values,
      ID = "",
      parameter = "",
      unit = "",
      variant = "",
      aggregated = FALSE,
      fast = FALSE,
      swallow = FALSE,
      na.status = c("explicit", "implicit", "undecided")
    ) {
      assertDataFrame(values, min.rows = 1L, min.cols = 2L)
      assertCharacter(
        names(values)[-1L],
        min.chars = 1L,
        any.missing = FALSE,
        unique = TRUE
      )
      assertNoBeginningDot(names(values)[-1L])
      qassert(swallow, "B1")
      na.status <- match.arg(na.status)

      if (is.data.table(values)) {
        if (swallow) {
          private$.values <- values
        } else {
          private$.values <- copy(values)
        }
      } else {
        private$.values <- as.data.table(values)
      }

      self$ID <- ID
      self$parameter <- parameter
      self$unit <- unit
      self$variant <- variant
      self$aggregated <- aggregated
      self$fast <- fast

      private$.origDateTimeCol <- names(private$.values)[1L]

      self$refresh()

      if (swallow) {
        private$rmGlobalReferences(address(private$.values))
      }

      self$alter(clone = FALSE, na.status = na.status)
    },

    merge = function(y, ..., clone = getOption("DTSgClone")) {
      if (!testR6(y, "DTSg")) {
        y <- DTSg$new(
          y,
          aggregated = private$.isAggregated,
          fast = private$.isFast,
          na.status = private$.na.status
        )
      }
      assertSetEqual(y$timezone, self$timezone)
      assertSetEqual(y$aggregated, self$aggregated)
      if (any(names(list(...)) %chin% c("x", "by", "by.x", "by.y"))) {
        stop(
          '"x", "by", "by.x" and "by.y" arguments are not allowed in this context.',
          call. = FALSE
        )
      }
      qassert(clone, "B1")

      if (clone) {
        TS <- self$clone(deep = TRUE)
        return(TS$merge(y = y, ... = ..., clone = FALSE))
      }

      private$.values <- merge(
        private$.values,
        y$values(TRUE),
        ...
      )

      self$refresh()
      self$alter(clone = FALSE)

      invisible(self)
    },

    nas = function(cols = self$cols()) {
      assertNAstatusPeriodicityOK(
        private$.na.status,
        private$.periodicity,
        level = "warning"
      )
      assertCharacter(cols, any.missing = FALSE, min.len = 1L, unique = TRUE)
      assertSubset(cols, self$cols())

      DTs <- list()

      for (col in cols) {
        if (anyNA(private$.values[[col]])) {
          DT <- private$.values[
            ,
            .(.dateTime, .col = get(col), .group = rleid(get(col)))
          ]

          DT <- DT[
            is.na(.col),
            .(.from = min(.dateTime), .to = max(.dateTime), .n = .N),
            by = .(.col, .group = rleid(.group))
          ]
          DT[, .col := as.character(.col)]
          DT[, .col := col]

          DTs <- c(DTs, list(DT))
        } else {
          DTs <- c(
            DTs,
            list(data.table(
              .col = character(),
              .group = integer(),
              .from = .POSIXct(numeric(), tz = private$.timezone),
              .to = .POSIXct(numeric(), tz = private$.timezone),
              .n = integer()
            ))
          )
        }
      }

      rbindlist(DTs)
    },

    plot = function(
      from = first(self$values(reference = TRUE)[[".dateTime"]]),
      to = last(self$values(reference = TRUE)[[".dateTime"]]),
      cols = self$cols(class = "numeric"),
      secAxisCols  = NULL,
      secAxisLabel = ""
    ) {
      if (!requireNamespace("dygraphs", quietly = TRUE) ||
          !requireNamespace("RColorBrewer", quietly = TRUE)) {
        stop(
          'Packages "dygraphs" and "RColorBrewer" must be installed for this method.',
          call. = FALSE
        )
      }
      from <- private$determineFrom(from)
      to <- private$determineTo(to, from)
      assertCharacter(cols, any.missing = FALSE, min.len = 1L, unique = TRUE)
      assertSubset(cols, self$cols())

      ylab <- ""

      if (private$.parameter != "") {
        ylab <- private$.parameter

        if (private$.variant != "") {
          ylab <- sprintf("%s, %s", ylab, private$.variant)
        }
        if (private$.unit != "") {
          ylab <- sprintf("%s (%s)", ylab, private$.unit)
        }
      }

      plot <- dygraphs::dygraph(
        as.xts.data.table(private$.values[
          .dateTime >= from & .dateTime <= to,
          c(".dateTime", cols),
          with = FALSE
        ]),
        private$.ID,
        ylab = ylab
      )
      plot <- dygraphs::dyOptions(
        plot,
        colors = RColorBrewer::brewer.pal(max(length(cols), 3L), "Set2"),
        useDataTimezone = TRUE
      )
      plot <- dygraphs::dyRangeSelector(plot)

      if (!is.null(secAxisCols)) {
        assertCharacter(secAxisCols, any.missing = FALSE, min.len = 1L, unique = TRUE)
        assertSubset(secAxisCols, cols)
        qassert(secAxisLabel, "S1")

        plot <- dygraphs::dyAxis(
          plot,
          "y2",
          label = secAxisLabel,
          drawGrid = FALSE,
          independentTicks = TRUE
        )

        for (i in seq_along(secAxisCols)) {
          plot <- dygraphs::dySeries(plot, secAxisCols[i], axis = "y2")
        }
      }

      print(plot)

      invisible(self)
    },

    print = function() {
      cat(  "Values:\n")
      print(private$.values, class = TRUE)
      cat(  "\n")
      if (private$.ID != "") {
        cat("ID:             ", private$.ID          , "\n", sep = "")
      }
      if (private$.parameter != "") {
        cat("Parameter:      ", private$.parameter   , "\n", sep = "")
      }
      if (private$.unit != "") {
        cat("Unit:           ", private$.unit        , "\n", sep = "")
      }
      if (private$.variant != "") {
        cat("Variant:        ", private$.variant     , "\n", sep = "")
      }
      cat(  "Aggregated:     ", private$.isAggregated, "\n", sep = "")
      cat(  "Regular:        ", private$.isRegular   , "\n", sep = "")
      if (is.character(private$.periodicity)) {
        cat("Periodicity:    ", private$.periodicity , "\n", sep = "")
      } else {
        cat("Periodicity:    ")
        print(private$.periodicity)
      }
      if (!private$.isRegular) {
        cat("Min lag:        ")
        print(private$.minLag)
        cat("Max lag:        ")
        print(private$.maxLag)
      }
      cat(  "Missing values: ", private$.na.status   , "\n", sep = "")
      cat(  "Time zone:      ", private$.timezone    , "\n", sep = "")
      cat(  "Timestamps:     ", private$.timestamps  , "\n", sep = "")

      invisible(self)
    },

    refresh = function() {
      firstCol <- names(private$.values)[1L]

      if (!qtest(private$.values[[1L]], "p+")) {
        set(
          private$.values,
          j = 1L,
          value = private$coerceCol(
            private$.values[[1L]],
            as.POSIXct,
            tz = Sys.timezone(),
            colname = firstCol
          )
        )
      }

      if (!isTRUE(key(private$.values) == firstCol)) {
        setkeyv(private$.values, firstCol)
      }

      private$.timestamps <- nrow(private$.values)
      private$.timezone <- attr(private$.values[[1L]], "tzone")

      seqLen <- seq_len(private$determineLen(private$.timestamps))

      if (anyNA(private$.values[[1L]][seqLen])) {
        stop(".dateTime column must not have any missing values.", call. = FALSE)
      }

      if (private$.timestamps < 2L) {
        private$.minLag <- .difftime(0, units = "secs")
        private$.maxLag <- .difftime(0, units = "secs")
        private$.isRegular <- TRUE
        private$.periodicity <- "unrecognised"

        return(invisible(self))
      }

      lags <- round(diff(private$.values[[1L]][seqLen]), 6L)

      if (any(lags == 0)) {
        stop(".dateTime column must not have any duplicates.", call. = FALSE)
      }

      private$.minLag <- min(lags)
      private$.maxLag <- max(lags)
      minLag <- as.numeric(private$.minLag, units = "secs")
      maxLag <- as.numeric(private$.maxLag, units = "secs")

      if (maxLag %% minLag == 0) {
        private$.isRegular <- TRUE
        private$.periodicity <- private$.minLag
      } else {
        private$.isRegular <- FALSE
        private$.periodicity <- "unrecognised"

        from <- private$.values[[1L]][1L]
        to   <- private$.values[[1L]][last(seqLen)]

        for (by in c(
          sprintf("%s DSTdays", c(seq_len(15L), 21L, 28L, 30L)),
          sprintf("%s months", c(seq_len(4L), 6L)),
          sprintf("%s years", c(seq_len(15L), 20L, 25L, seq(30L, 70L, 10L), 75L, 80L, 90L, 100L))
        )) {
          if (grepl("^\\d+ (month|year)(s?)$", by) && mday(from) > 28L) {
            DT <- data.table(
              .dateTime = rollback(seq(
                from,
                to + diff(seq(to, by = "1 DSTday", length.out = 2L)),
                by
              ), by),
              key = ".dateTime"
            )
          } else {
            DT <- data.table(.dateTime = seq(from, to, by), key = ".dateTime")
          }

          DT <- private$.values[DT, on = sprintf("%s == .dateTime", firstCol)]
          lags <- diff(DT[[1L]])
          if (sum(!is.na(DT[, -1L, with = FALSE])) ==
              sum(!is.na(private$.values[seqLen, -1L, with = FALSE])) &&
              all(lags >= private$.minLag) && all(lags <= private$.maxLag)) {
            private$.periodicity <- by

            break
          }
        }
      }

      setnames(private$.values, 1L, ".dateTime")

      invisible(self)
    },

    rollapply = function(
      fun,
      ...,
      cols = self$cols(class = "numeric")[1L],
      before = 1L,
      after = before,
      weights = c("inverseDistance"),
      parameters = list(power = 1),
      resultCols = NULL,
      suffix = NULL,
      memoryOverCPU = TRUE,
      clone = getOption("DTSgClone")
    ) {
      assertNAstatusPeriodicityOK(
        private$.na.status,
        private$.periodicity,
        level = "warning"
      )
      assertFunction(fun)
      assertCharacter(cols, any.missing = FALSE, min.len = 1L, unique = TRUE)
      assertSubset(cols, self$cols())
      before <- assertCount(before, coerce = TRUE)
      after <- assertCount(after, coerce = TRUE)
      weights <- match.arg(weights)
      .cols <- private$determineCols(resultCols, suffix, cols)
      qassert(memoryOverCPU, "B1")
      qassert(clone, "B1")

      if (clone) {
        TS <- self$clone(deep = TRUE)
        return(TS$rollapply(
          fun = fun,
          ... = ...,
          cols = cols,
          before = before,
          after = after,
          weights = weights,
          parameters = parameters,
          resultCols = resultCols,
          suffix = suffix,
          memoryOverCPU = memoryOverCPU,
          clone = FALSE
        ))
      }

      if (weights == "inverseDistance") {
        qassert(parameters$power, "N1()")

        weights <- 1 / c(
          rev(seq_len(before) + 1),
          1,
          seq_len(after) + 1
        )^parameters$power
        weights <- weights / sum(weights)
      }

      .helpers = list(
        before = before,
        after = after,
        windowSize = before + 1L + after,
        centerIndex = before + 1L
      )

      if (memoryOverCPU) {
        wapply <- function(x, fun, ..., before, after, weights) {
          L <- rev(shift(list(x), 0:before))
          if (after != 0L) {
            L <- c(L, shift(list(x), seq_len(after), type = "lead"))
          }

          apply(
            matrix(unlist(L), ncol = length(L)),
            1L,
            fun,
            ...,
            w = weights,
            .helpers = .helpers
          )
        }
      } else {
        wapply <- function(x, fun, ..., before, after, weights) {
          y <- vector(typeof(x), length(x))
          y[] <- NA

          for (i in seq_along(x)) {
            lowerBound <- i - before

            y[i] <- fun(
              if (lowerBound < 1L) {
                c(rep(NA, abs(lowerBound) + 1L), x[seq_len(i + after)])
              } else {
                x[lowerBound:(i + after)]
              },
              ...,
              w = weights,
              .helpers = .helpers
            )
          }

          y
        }
      }

      private$.values[
        ,
        (.cols) := lapply(
          .SD,
          wapply,
          fun = fun,
          ...,
          before = before,
          after = after,
          weights = weights
        ),
        .SDcols = cols
      ]

      invisible(self)
    },

    rowapply = function(
      resultCols,
      fun,
      ...,
      cols = self$cols(class = "numeric"),
      clone = getOption("DTSgClone")
    ) {
      if (length(cols) > 1L && length(resultCols) > 1L) {
        assertCharacter(resultCols, min.chars = 1L, any.missing = FALSE, len = length(fun))
      } else {
        assertCharacter(resultCols, min.chars = 1L, any.missing = FALSE, len = 1L)
        if (!clone && length(names(fun)) > 0L) {
          resultCols <- sprintf("%s.%s", resultCols, names(fun))
        }
      }
      fun <- private$determineFun(fun, length(fun) != length(resultCols))
      assertCharacter(cols, any.missing = FALSE, min.len = 2L, unique = TRUE)
      assertSubset(cols, self$cols())
      qassert(clone, "B1")

      if (clone) {
        TS <- self$clone(deep = TRUE)
        return(TS$rowapply(
          resultCols = resultCols,
          fun = fun,
          ... = ...,
          cols = cols,
          clone = FALSE
        ))
      }

      private$.values[
        ,
        (resultCols) := lapply(fun, function(fun, ...) {fun(unlist(.SD), ...)}),
        by = seq_len(private$.timestamps),
        .SDcols = cols
      ]

      invisible(self)
    },

    rowbind = function(..., clone = getOption("DTSgClone")) {
      qassert(clone, "B1")

      if (clone) {
        TS <- self$clone(deep = TRUE)
        return(TS$rowbind(... = ..., clone = FALSE))
      }

      dotsToList <- function(...) {
        do.call(c, lapply(
          seq_len(...length()),
          function(i) {
            if (testClass(...elt(i), "list")) {
              lapply(
                seq_len(length(...elt(i))),
                function(j, x) {x[[j]]},
                x = ...elt(i)
              )
            } else {
              list(...elt(i))
            }
          }
        ))
      }

      processElements <- function(obj) {
        if (!testR6(obj, "DTSg")) {
          obj <- DTSg$new(
            obj,
            aggregated = private$.isAggregated,
            fast = private$.isFast,
            na.status = private$.na.status
          )
        }
        assertSetEqual(obj$timezone, self$timezone)
        assertSetEqual(obj$aggregated, self$aggregated)

        obj$values(TRUE)
      }

      DTs <- c(list(private$.values), lapply(dotsToList(...), processElements))
      values <- rbindlist(DTs, use.names = TRUE, fill = TRUE)
      len <- private$determineLen(nrow(values))
      assertPOSIXct(
        values[[".dateTime"]][seq_len(len)],
        any.missing = FALSE,
        unique = TRUE,
        .var.name = sprintf('self$values(reference = TRUE)[[".dateTime"]][1:%s]', len)
      )

      private$.values <- values

      self$refresh()
      self$alter(clone = FALSE)

      invisible(self)
    },

    setColNames = function(
      cols = self$cols(class = "numeric")[1L],
      values,
      clone = getOption("DTSgClone")
    ) {
      assertCharacter(cols, any.missing = FALSE, min.len = 1L, unique = TRUE)
      assertSubset(cols, self$cols())
      assertCharacter(
        values,
        min.chars = 1L,
        any.missing = FALSE,
        len = length(cols),
        unique = TRUE
      )
      assertNoBeginningDot(values)

      if (clone) {
        TS <- self$clone(deep = TRUE)
        return(TS$setColNames(cols = cols, values = values, clone = FALSE))
      }

      setnames(private$.values, cols, values)

      invisible(self)
    },

    setCols = function(
      i,
      cols = self$cols(class = "numeric")[1L],
      values,
      clone = getOption("DTSgClone")
    ) {
      if (!missing(i)) {
        i <- private$determineFilter(i, as.expression(substitute(i)))
        assertFilter(i, private$.timestamps)
      }
      assertCharacter(
        cols,
        min.chars = 1L,
        any.missing = FALSE,
        min.len = 1L,
        unique = TRUE
      )
      assertNoBeginningDot(cols)
      if (length(cols) == length(names(private$.values)) - 1L &&
          ((is.list(values) && all(vapply(values, is.null, logical(1L)))) ||
          is.null(values))) {
        stop("Removing all value columns is not allowed.", call. = FALSE)
      }
      qassert(clone, "B1")

      if (clone) {
        TS <- self$clone(deep = TRUE)
        return(TS$setCols(
          i = i,
          cols = cols,
          values = values,
          clone = FALSE
        ))
      }

      if (!missing(i)) {
        private$.values[eval(i), (cols) := values]
      } else {
        private$.values[, (cols) := values]
      }

      invisible(self)
    },

    subset = function(
      i,
      cols = self$cols(),
      funby = NULL,
      ignoreDST = FALSE,
      na.status = "implicit",
      clone = getOption("DTSgClone")
    ) {
      if (!missing(i)) {
        i <- private$determineFilter(i, as.expression(substitute(i)))
        assertFilter(i, private$.timestamps)
      }
      assertCharacter(cols, any.missing = FALSE, min.len = 1L, unique = TRUE)
      assertSubset(cols, self$cols())
      na.status <- match.arg(na.status, private$.na.statuses)
      qassert(clone, "B1")

      if (clone) {
        TS <- self$clone(deep = TRUE)
        return(TS$subset(
          i = i,
          cols = cols,
          funby = funby,
          ignoreDST = ignoreDST,
          na.status = na.status,
          clone = FALSE
        ))
      }

      cols <- c(".dateTime", cols)

      if (!missing(i)) {
        if (!is.null(funby)) {
          assertFunction(funby)
          qassert(ignoreDST, "B1")
          .funbyHelpers <- private$funbyHelpers(ignoreDST, list())
          assertAtomic(funby(
            self$values(reference = TRUE)[[".dateTime"]][1L],
            .funbyHelpers
          ), any.missing = FALSE, len = 1L)

          values <- private$.values[
            ,
            .SD[eval(i)],
            by = .(.group = funby(.dateTime, .funbyHelpers)),
            .SDcols = cols
          ]
          values[, .group := NULL]
        } else {
          values <- private$.values[eval(i), cols, with = FALSE]
        }
      } else {
        values <- private$.values[, cols, with = FALSE]
      }

      assertDataTable(values, min.rows = 1L, .var.name = "self$values(reference = TRUE)")

      private$.values <- values

      self$refresh()
      self$alter(clone = FALSE, na.status = na.status)

      invisible(self)
    },

    summary = function(cols = self$cols(), ...) {
      assertCharacter(cols, any.missing = FALSE, min.len = 1L, unique = TRUE)
      assertSubset(cols, self$cols())

      summary(private$.values[, cols, with = FALSE], ...)
    },

    values = function(
      reference = FALSE,
      drop = FALSE,
      class = c("data.table", "data.frame")
    ) {
      qassert(reference, "B1")
      qassert(drop, "B1")
      class <- match.arg(class)

      if (reference || drop) {
        values <- private$.values

        if (drop) {
          private$rmGlobalReferences(address(self))
        }
      } else {
        values <- copy(private$.values)
      }

      if (!reference || drop) {
        setnames(values, 1L, private$.origDateTimeCol)

        if (class == "data.frame") {
          setDF(values)
        }
      }

      values
    }
  ),

  #### Active ####
  active = list(
    aggregated = function(value) {
      if (missing(value)) {
        private$.isAggregated
      } else {
        qassert(value, "B1")

        private$.isAggregated <- value

        invisible(self)
      }
    },

    fast = function(value) {
      if (missing(value)) {
        private$.isFast
      } else {
        qassert(value, "B1")

        private$.isFast <- value

        invisible(self)
      }
    },

    ID = function(value) {
      if (missing(value)) {
        private$.ID
      } else {
        qassert(value, "S1")

        private$.ID <- value

        invisible(self)
      }
    },

    na.status = function(value) {
      if (missing(value)) {
        private$.na.status
      } else {
        self$alter(clone = FALSE, na.status = value)

        invisible(self)
      }
    },

    parameter = function(value) {
      if (missing(value)) {
        private$.parameter
      } else {
        qassert(value, "S1")

        private$.parameter <- value

        invisible(self)
      }
    },

    periodicity = function(value) {
      if (missing(value)) {
        private$.periodicity
      } else {
        self$alter(by = value, clone = FALSE)

        invisible(self)
      }
    },

    regular = function(value) {
      if (missing(value)) {
        private$.isRegular
      } else {
        stop("Read-only field.", call. = FALSE)
      }
    },

    timestamps = function(value) {
      if (missing(value)) {
        private$.timestamps
      } else {
        stop("Read-only field.", call. = FALSE)
      }
    },

    timezone = function(value) {
      if (missing(value)) {
        private$.timezone
      } else {
        qassert(value, "S1")
        assertSubset(value, OlsonNames())

        attr(private$.values[[".dateTime"]], "tzone") <- value
        private$.timezone <- value

        invisible(self)
      }
    },

    unit = function(value) {
      if (missing(value)) {
        private$.unit
      } else {
        qassert(value, "S1")

        private$.unit <- value

        invisible(self)
      }
    },

    variant = function(value) {
      if (missing(value)) {
        private$.variant
      } else {
        qassert(value, "S1")

        private$.variant <- value

        invisible(self)
      }
    }
  )
)
