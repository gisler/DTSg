#' @import assertive.base
#' @import assertive.numbers
#' @import assertive.sets
#' @import assertive.types
#' @import data.table
#' @import methods
#' @import R6
NULL

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
#'  aggregated = FALSE, fast = FALSE, swallow = FALSE)
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
#'  ressource efficient, but only works if the object provided through the
#'  \code{values} argument is a \code{\link[data.table]{data.table}}. Be warned,
#'  however, that if the creation of the \code{DTSg} object fails for some
#'  reason, the first column of the provided
#'  \code{\link[data.table]{data.table}} might have been coerced to
#'  \code{\link{POSIXct}} and keyed (see \code{\link[data.table]{setkey}} for
#'  further information). Furthermore, all references to the \dQuote{swallowed}
#'  \code{\link[data.table]{data.table}} in the global environment are removed
#'  upon successfull creation of a \code{DTSg} object.
#'
#' @return Returns a \code{DTSg} object.
#'
#' @section Methods:
#' A \code{DTSg} object has the following methods:
#'  \itemize{
#'    \item \code{aggregate}: See \code{\link{aggregate}} for further
#'      information.
#'    \item \code{alter}: See \code{\link{alter}} for further information.
#'    \item \code{colapply}: See \code{\link{colapply}} for further information.
#'    \item \code{cols}: See \code{\link{cols}} for further information.
#'    \item \code{merge}: See \code{\link{merge}} for further information.
#'    \item \code{nas}: See \code{\link{nas}} for further information.
#'    \item \code{plot}: See \code{\link{plot}} for further information.
#'    \item \code{refresh}: See \code{\link{refresh}} for further information.
#'    \item \code{rollapply}: See \code{\link{rollapply}} for further
#'      information.
#'    \item \code{summary}: See \code{\link{summary}} for further information.
#'    \item \code{values}: See \code{\link{values}} for further information.
#'  }
#'
#' @section Fields:
#' A \code{DTSg} object has the following fields or properties as they are often
#'  called. They are implemented through so called active bindings, which means
#'  that they can be accessed and actively set with the help of the \code{$}
#'  operator (for instance, \code{x$ID} gets the value of the \emph{ID} field
#'  and \code{x$ID <- "River Flow"} sets its value). Please note that fields are
#'  always modified in place, i.e., no clone (copy) of the object is made
#'  beforehand. See \code{\link{clone}} for further information. Some of the
#'  fields are read-only though:
#'  \itemize{
#'    \item \emph{aggregated:} Same as \code{aggregated} argument.
#'    \item \emph{fast:} Same as \code{fast} argument.
#'    \item \emph{ID:} Same as \code{ID} argument. It is used as the title of
#'      plots.
#'    \item \emph{parameter:} Same as \code{parameter} argument. It is used as
#'      the label of the primary axis of plots.
#'    \item \emph{periodicity:} A \code{\link{difftime}} object for a regular
#'      and a character string for an irregular \code{DTSg} object describing
#'      its periodicity or containing \code{"unrecognised"} in case it could not
#'      be detected (read-only).
#'    \item \emph{regular:} A logical signalling if all lags in seconds between
#'      subsequent timestamps are the same (\code{TRUE}) or if some are
#'      different (\code{FALSE}). A, for instance, monthly time series is
#'      considered irregular in this sense (read-only).
#'    \item \emph{timestamps:} An integer showing the total number of timestamps
#'      of the time series (read-only).
#'    \item \emph{timezone:} A character string containing the time zone of the
#'      time series (read-only).
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
#'  series with a sub-second accuracy higher than a microsecond will never work
#'  and with a sub-second accuracy lower than a microsecond might work.
#'
#' Some of the methods which take a function as an argument
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
#'  \code{\link{clone}}, \code{\link{options}}, \code{\link{list}}
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
    .maxLag = difftime(Sys.time(), Sys.time()),
    .minLag = difftime(Sys.time(), Sys.time()),
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

    rmGlobalReferences = function(addr) {
      globalObjs <- ls(".GlobalEnv", sorted = FALSE)

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
      assert_is_function(funby)
      .helpers <- list(
        timezone = private$.timezone,
        ignoreDST = ignoreDST,
        periodicity = private$.periodicity
      )
      assert_is_posixct(funby(private$.values[[".dateTime"]][1L], .helpers))
      assert_is_function(fun)
      assert_is_subset(assert_is_character(cols), self$cols())
      assert_is_length_cols_greater_than_or_equal_to_one(cols)
      assert_is_a_bool(assert_all_are_not_na(n))
      assert_is_a_bool(assert_all_are_not_na(ignoreDST))
      assert_is_a_bool(assert_all_are_not_na(clone))

      if (clone) {
        TS <- self$clone(deep = TRUE)
        return(TS$aggregate(
          funby = funby,
          fun = fun,
          ...,
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
            c(lapply(.SD, fun, ...), .(.n = .N)),
            keyby = .(.dateTime = funby(.dateTime, .helpers)),
            .SDcols = cols
          ]

          message(".n column calculated from .dateTime column.")
        } else {
          private$.values <- private$.values[
            !is.na(eval(as.name(cols))),
            c(lapply(.SD, fun, ...), .(.n = .N)),
            keyby = .(.dateTime = funby(.dateTime, .helpers)),
            .SDcols = cols
          ]

          message(
            'NA values are always stripped regardless of the value of a possible "na.rm" argument.'
          )
        }
      } else {
        private$.values <- private$.values[
          ,
          lapply(.SD, fun, ...),
          keyby = .(.dateTime = funby(.dateTime, .helpers)),
          .SDcols = cols
        ]
      }

      private$.isAggregated <- TRUE

      self$refresh()

      if (private$.periodicity != "unrecognised") {
        self$alter(clone = FALSE)
      } else {
        invisible(self)
      }
    },

    alter = function(
      from = first(self$values(TRUE)[[1L]]),
      to = last(self$values(TRUE)[[1L]]),
      by = self$periodicity,
      rollback = TRUE,
      clone = getOption("DTSgClone")
    ) {
      if (is_posixct(from)) {
        assert_are_identical(attr(from, "tzone"), private$.timezone)
      } else {
        from <- as.POSIXct(from, tz = private$.timezone)
      }
      if (is_posixct(to)) {
        assert_are_identical(attr(to, "tzone"), private$.timezone)
      } else {
        to <- as.POSIXct(to, tz = private$.timezone)
      }
      assert_all_are_greater_than_or_equal_to(as.numeric(to), as.numeric(from))
      if (by == "unrecognised") {
        stop(
          '"by" must be explicitly set for time series with unrecognised periodicity.',
          call. = FALSE
        )
      }
      assert_is_a_bool(assert_all_are_not_na(rollback))
      assert_is_a_bool(assert_all_are_not_na(clone))

      if (clone) {
        TS <- self$clone(deep = TRUE)
        return(TS$alter(
          from = from,
          to = to,
          by = by,
          rollback = rollback,
          clone = FALSE
        ))
      }

      if (rollback && grepl("^\\d+ (month|year)(s?)$", by) && mday(from) > 28L) {
        DT <- data.table(
          .dateTime = seq(from, to + diff(seq(to, by = "1 DSTday", length.out = 2L)), by),
          key = ".dateTime"
        )
        DT[, .dateTime := rollback(.dateTime, by)]
      } else {
        DT <- data.table(.dateTime = seq(from, to, by), key = ".dateTime")
      }
      if (by != private$.periodicity || nrow(DT) != private$.timestamps) {
        private$.values <- private$.values[DT]

        self$refresh()
      }

      invisible(self)
    },

    colapply = function(
      fun,
      ...,
      cols = self$cols(class = "numeric")[1L],
      clone = getOption("DTSgClone")
    ) {
      assert_is_function(fun)
      assert_is_subset(assert_is_character(cols), self$cols())
      assert_is_length_cols_greater_than_or_equal_to_one(cols)
      assert_is_a_bool(assert_all_are_not_na(clone))

      if (clone) {
        TS <- self$clone(deep = TRUE)
        return(TS$colapply(
          fun = fun,
          ...,
          cols = cols,
          clone = FALSE
        ))
      }

      private$.values[
        ,
        (cols) := lapply(
          .SD,
          fun,
          ...,
          .helpers = list(
            .dateTime = .dateTime,
            periodicity = private$.periodicity,
            minLag = private$.minLag,
            maxLag = private$.maxLag
          )
        ),
        .SDcols = cols
      ]

      invisible(self)
    },

    cols = function(class = "all") {
      assert_is_a_string(class)

      cols <- names(private$.values)[-1L]

      if (class == "all") {
        cols
      } else {
        classes <- vapply(
          private$.values[, -1L, with = FALSE],
          function(col) {class(col)[1L]},
          character(1L)
        )
        cols[classes == class]
      }
    },

    initialize = function(
      values,
      ID = "",
      parameter = "",
      unit = "",
      variant = "",
      aggregated = FALSE,
      fast = FALSE,
      swallow = FALSE
    ) {
      assert_is_inherited_from(values, "data.frame")
      if (nrow(values) < 1L || ncol(values) < 2L) {
        stop('"values" must have at least one row and two columns.', call. = FALSE)
      }
      if (any(grepl("^\\.", names(values)[-1L]))) {
        stop('Column names must not start with a ".".', call. = FALSE)
      }
      if (anyDuplicated(names(values)[-1L]) > 0) {
        stop('Column names must not have any duplicates.', call. = FALSE)
      }
      assert_is_a_bool(assert_all_are_not_na(swallow))

      if (is.data.table(values)) {
        if (swallow) {
          private$.values <- values
        } else {
          private$.values <- copy(values)
        }
      } else {
        private$.values <- as.data.table(values)
      }

      private$.origDateTimeCol <- names(private$.values)[1L]

      self$ID <- ID
      self$parameter <- parameter
      self$unit <- unit
      self$variant <- variant
      self$aggregated <- aggregated
      self$fast <- fast

      self$refresh()

      if (swallow) {
        private$rmGlobalReferences(address(private$.values))
      }

      if (private$.periodicity != "unrecognised") {
        self$alter(clone = FALSE)
      }
    },

    merge = function(y, ..., clone = getOption("DTSgClone")) {
      if (!is_inherited_from(y, "DTSg")) {
        y <- DTSg$new(y)
      }
      assert_are_identical(private$.timezone, y$timezone)
      assert_are_identical(private$.isAggregated, y$aggregated)
      if (any(names(list(...)) %in% c("by", "by.x", "by.y"))) {
        stop('"by", "by.x" and "by.y" arguments are not allowed in this context.', call. = FALSE)
      }
      assert_is_a_bool(assert_all_are_not_na(clone))

      if (clone) {
        TS <- self$clone(deep = TRUE)
        return(TS$merge(y = y, ..., clone = FALSE))
      }

      private$.values <- merge(
        private$.values,
        y$values(TRUE),
        ...
      )

      self$refresh()

      if (private$.periodicity != "unrecognised") {
        self$alter(clone = FALSE)
      } else {
        invisible(self)
      }
    },

    nas = function(cols = self$cols()) {
      assert_is_periodicity_recognised(private$.periodicity)
      assert_is_subset(assert_is_character(cols), self$cols())
      assert_is_length_cols_greater_than_or_equal_to_one(cols)

      DetectNA <- function(x) {
        i <- 0L
        isNAlast <- FALSE
        return <- function(x) {
          if (is.na(x)) {
            if (!isNAlast) {
              i <<- i + 1L
              isNAlast <<- TRUE
            }
            n <- i
          } else {
            n <- NA
            isNAlast <<- FALSE
          }
          return <- n
        }
      }

      DTs <- list()

      for (col in cols) {
        detectNA <- DetectNA(x)

        DT <- private$.values[, ".dateTime", with = FALSE]
        set(DT, j = ".col", value = col)
        set(DT, j = ".group", value = vapply(private$.values[[col]], detectNA, integer(1L)))
        if (all(is.na(DT[[".group"]]))) {
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
        } else {
          DTs <- c(
            DTs,
            list(DT[
              !is.na(.group),
              .(.from = min(.dateTime), .to = max(.dateTime), .n = .N),
              by = c(".col", ".group")
            ])
          )
        }
      }

      do.call(rbind, DTs)
    },

    plot = function(
      from = first(self$values(TRUE)[[1L]]),
      to = last(self$values(TRUE)[[1L]]),
      cols = self$cols(class = "numeric"),
      secAxisCols  = NULL,
      secAxisLabel = ""
    ) {
      if (!requireNamespace("dygraphs", quietly = TRUE) ||
          !requireNamespace("RColorBrewer", quietly = TRUE)) {
        stop(
          sprintf(
            "Packages %s and %s must be installed for this method.",
            deparse("dygraphs"), deparse("RColorBrewer")
          ),
          call. = FALSE
        )
      }

      if (is_posixct(from)) {
        assert_are_identical(attr(from, "tzone"), private$.timezone)
      } else {
        from <- as.POSIXct(from, tz = private$.timezone)
      }
      if (is_posixct(to)) {
        assert_are_identical(attr(to, "tzone"), private$.timezone)
      } else {
        to <- as.POSIXct(to, tz = private$.timezone)
      }
      assert_all_are_greater_than_or_equal_to(as.numeric(to), as.numeric(from))
      assert_is_subset(assert_is_character(cols), self$cols())
      assert_is_length_cols_greater_than_or_equal_to_one(cols)
      if (!is.null(secAxisCols)) {
        assert_is_subset(assert_is_character(secAxisCols), cols)
        assert_is_a_string(secAxisLabel)
      }

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
        private$.values[.dateTime >= from & .dateTime <= to, c(".dateTime", cols), with = FALSE],
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
        plot <- dygraphs::dyAxis(
          plot,
          "y2",
          label = secAxisLabel,
          drawGrid = FALSE,
          independentTicks = TRUE
        )
        for (i in 1:length(secAxisCols)) {
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
        cat("ID:          ", private$.ID          , "\n", sep = "")
      }
      if (private$.parameter != "") {
        cat("Parameter:   ", private$.parameter   , "\n", sep = "")
      }
      if (private$.variant != "") {
        cat("Variant:     ", private$.variant     , "\n", sep = "")
      }
      if (private$.unit != "") {
        cat("Unit:        ", private$.unit        , "\n", sep = "")
      }
      cat(  "Aggregated:  ", private$.isAggregated, "\n", sep = "")
      cat(  "Regular:     ", private$.isRegular   , "\n", sep = "")
      if (is.character(private$.periodicity)) {
        cat("Periodicity: ", private$.periodicity , "\n", sep = "")
      } else {
        cat("Periodicity: ")
        print(private$.periodicity)
      }
      if (!private$.isRegular) {
        cat("Min lag:     ")
        print(private$.minLag)
        cat("Max lag:     ")
        print(private$.maxLag)
      }
      cat(  "Time zone:   ", private$.timezone    , "\n", sep = "")
      cat(  "Timestamps:  ", private$.timestamps  , "\n", sep = "")

      invisible(self)
    },

    refresh = function() {
      firstCol <- names(private$.values)[1L]

      if (!is_posixct(private$.values[[1L]])) {
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

      if (private$.timestamps < 2L) {
        zeroSecs <- difftime(as.POSIXct("2000-01-01", tz = "UTC"), as.POSIXct("2000-01-01", tz = "UTC"))
        private$.minLag <- zeroSecs
        private$.maxLag <- zeroSecs
        private$.isRegular <- TRUE
        private$.periodicity <- "unrecognised"
        return(invisible(self))
      }

      if (!private$.isFast || private$.timestamps < 1000L) {
        len <- private$.timestamps
      } else {
        len <- 1000L
      }

      lags <- round(diff(private$.values[[1L]][1:len]), 6)
      if (anyNA(lags)) {
        stop(".dateTime column must not have any NA values.", call. = FALSE)
      }
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
        to   <- private$.values[[1L]][len]

        for (by in c(
          sprintf("%s DSTdays", c(1:15, 21L, 28L, 30L)),
          sprintf("%s months", c(1:4, 6L)),
          sprintf("%s years", c(1:14, 15L, 20L, 25L, 30L, 40L, 50L, 60L, 70L, 75L, 80L, 90L, 100L))
        )) {
          if (grepl("^\\d+ (month|year)(s?)$", by) && mday(from) > 28L) {
            DT <- data.table(
              .dateTime = seq(from, to + diff(seq(to, by = "1 DSTday", length.out = 2L)), by),
              key = ".dateTime"
            )
            DT[, .dateTime := rollback(.dateTime, by)]
          } else {
            DT <- data.table(.dateTime = seq(from, to, by), key = ".dateTime")
          }

          DT <- private$.values[DT, on = sprintf("%s == .dateTime", firstCol)]
          lags <- diff(DT[[1L]])
          if (sum(!is.na(DT[, -1L, with = FALSE])) ==
              sum(!is.na(private$.values[1:len, -1L, with = FALSE])) &&
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
      clone = getOption("DTSgClone")
    ) {
      assert_is_periodicity_recognised(private$.periodicity)
      assert_is_function(fun)
      assert_is_subset(assert_is_character(cols), self$cols())
      assert_is_length_cols_greater_than_or_equal_to_one(cols)
      assert_all_are_greater_than_or_equal_to(
        c(assert_is_a_number(before), assert_is_a_number(after)),
        0L
      )
      weights <- match.arg(weights)
      assert_is_a_bool(assert_all_are_not_na(clone))

      if (clone) {
        TS <- self$clone(deep = TRUE)
        return(TS$rollapply(
          fun = fun,
          ...,
          cols = cols,
          before = before,
          after = after,
          weights = weights,
          parameters = parameters,
          clone = FALSE
        ))
      }

      if (weights == "inverseDistance") {
        assert_all_are_finite(assert_is_a_number(parameters$power))
        weights <- 1 / c(rev(seq_len(before) + 1), 1, seq_len(after) + 1)^parameters$power
        weights <- weights / sum(weights)
      }

      wapply <- function(x, fun, ..., before, after, weights) {
        L <- rev(shift(list(x), 0:before))
        if (after != 0L) {
          L <- c(L, shift(list(x), 1:after, type = "lead"))
        }

        apply(
          matrix(unlist(L), ncol = length(L)),
          1L,
          fun,
          ...,
          w = weights,
          .helpers = list(
            before = before,
            after = after,
            windowSize = before + 1L + after,
            centerIndex = before + 1L
          )
        )
      }

      private$.values[
        ,
        (cols) := lapply(
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

    summary = function(cols = self$cols(), ...) {
      assert_is_subset(assert_is_character(cols), self$cols())
      assert_is_length_cols_greater_than_or_equal_to_one(cols)

      summary(private$.values[, cols, with = FALSE], ...)
    },

    values = function(
      reference = FALSE,
      drop = FALSE,
      class = c("data.table", "data.frame")
    ) {
      assert_is_a_bool(assert_all_are_not_na(reference))
      assert_is_a_bool(assert_all_are_not_na(drop))
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
        assert_is_a_bool(assert_all_are_not_na(value))

        private$.isAggregated <- value

        invisible(self)
      }
    },

    fast = function(value) {
      if (missing(value)) {
        private$.isFast
      } else {
        assert_is_a_bool(assert_all_are_not_na(value))

        private$.isFast <- value

        invisible(self)
      }
    },

    ID = function(value) {
      if (missing(value)) {
        private$.ID
      } else {
        assert_is_a_string(value)

        private$.ID <- value

        invisible(self)
      }
    },

    parameter = function(value) {
      if (missing(value)) {
        private$.parameter
      } else {
        assert_is_a_string(value)

        private$.parameter <- value

        invisible(self)
      }
    },

    periodicity = function(value) {
      if (missing(value)) {
        private$.periodicity
      } else {
        stop("Read-only field.", call. = FALSE)
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
        stop("Read-only field.", call. = FALSE)
      }
    },

    unit = function(value) {
      if (missing(value)) {
        private$.unit
      } else {
        assert_is_a_string(value)

        private$.unit <- value

        invisible(self)
      }
    },

    variant = function(value) {
      if (missing(value)) {
        private$.variant
      } else {
        assert_is_a_string(value)

        private$.variant <- value

        invisible(self)
      }
    }
  )
)
