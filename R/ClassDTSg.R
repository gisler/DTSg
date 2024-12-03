#### Documentation ####
#' DTSg class
#'
#' The `DTSg` class is the working horse of the package. It is an
#' [`R6::R6Class`] and offers an S3 interface in addition to its native R6
#' interface. In the usage sections of the documentation, unfortunately, only
#' the usage of the S3 methods are displayed, however, the examples always show
#' both ways of calling the respective method. Generally, they are very similar
#' anyway. While the R6 interface always has the object first and the method is
#' then selected with the help of the `$` operator, for instance, `x$cols()`,
#' the S3 interface always has the method first and then the object as its first
#' argument, for instance, `cols(x)`. An exception is the `new` method. It is
#' not an S3 method, but an abused S4 constructor with the character string
#' `"DTSg"` as its first argument. Regarding the R6 interface, the `DTSg` class
#' generator has to be used to access the `new` method with the help of the `$`
#' operator.
#'
#' @usage new(Class, values, ID = "", parameter = "", unit = "", variant = "",
#'   aggregated = FALSE, fast = getOption("DTSgFast"), swallow = FALSE,
#'   na.status = getOption("DTSgNA.status"), funbyApproach =
#'   getOption("DTSgFunbyApproach"))
#'
#' @param Class A character string. Must be `"DTSg"` in order to create a `DTSg`
#'   object. Otherwise a different object may or may not be created (S4
#'   constructor only).
#' @param values A [`data.frame`] or object inherited from class [`data.frame`],
#'   e.g. [`data.table::data.table`]. Its first column must be of class
#'   [`POSIXct`] or coercible to it. It serves as the object's time index and is
#'   renamed to _.dateTime._
#' @param ID A character string specifying the ID (name) of the time series data
#'   object.
#' @param parameter A character string specifying the parameter name of the time
#'   series data.
#' @param unit A character string specifying the unit of the time series data.
#' @param variant A character string specifying further metadata of the time
#'   series, for instance, `"min"` to point out that it is a time series of
#'   lower bound measurements.
#' @param aggregated A logical specifying how the timestamps of the series have
#'   to be interpreted: as snap-shots (`FALSE`) or as periods between subsequent
#'   timestamps (`TRUE`).
#' @param fast A logical specifying if all rows (`FALSE`) or only the first 1000
#'   rows (`TRUE`) shall be used to check the object's integrity and for the
#'   automatic detection of the time series' periodicity.
#' @param swallow A logical specifying if the object provided through the
#'   `values` argument shall be \dQuote{swallowed} by the `DTSg` object, i.e. no
#'   copy of the data shall be made. This is generally more resource efficient,
#'   but only works when the provided object is a [`data.table::data.table`]. Be
#'   warned, however, that when the creation of the `DTSg` object fails for some
#'   reason, the first column of the provided [`data.table::data.table`] might
#'   have been coerced to [`POSIXct`] and keyed (see [`data.table::setkey`] for
#'   further information). Furthermore, all references to the \dQuote{swallowed}
#'   [`data.table::data.table`] in the global (and only the global) environment
#'   are removed upon the successful creation of the `DTSg` object.
#' @param na.status A character string. Either `"explicit"`, which makes missing
#'   timestamps explicit according to the recognised periodicity, or
#'   `"implicit"`, which removes timestamps with missing values on all value
#'   columns, or `"undecided"` for no such action. Please note that `DTSg`
#'   objects work best with explicitly missing values.
#' @param funbyApproach A character string specifying the default flavour of
#'   [`TALFs`] used with the created `DTSg` object. Either `"base"`, which
#'   utilises [`as.POSIXct`], or `"fasttime"`, which utilises
#'   [`fasttime::fastPOSIXct`], or `"RcppCCTZ"`, which utilises
#'   [`RcppCCTZ::parseDatetime`] as the main function for transforming
#'   timestamps. Custom approaches for user defined temporal aggregation level
#'   functions are also possible.
#'
#' @return Returns a `DTSg` object.
#'
#' @section Methods:
#' A `DTSg` object has the following methods:
#' * `aggregate`: See [`aggregate`] for further information.
#' * `alter`: See [`alter`] for further information.
#' * `clone`: See [`clone`] for further information.
#' * `colapply`: See [`colapply`] for further information.
#' * `cols`: See [`cols`] for further information.
#' * `getCol`: See [`getCol`] for further information.
#' * `merge`: See [`merge`] for further information.
#' * `nas`: See [`nas`] for further information.
#' * `plot`: See [`plot`] for further information.
#' * `print`: See [`print`] for further information.
#' * `refresh`: See [`refresh`] for further information.
#' * `rollapply`: See [`rollapply`] for further information.
#' * `rowaggregate`: See [`rowaggregate`] for further information.
#' * `rowbind`: See [`rowbind`] for further information.
#' * `setColNames`: See [`setColNames`] for further information.
#' * `setCols`: See [`setCols`] for further information.
#' * `subset`: See [`subset`] for further information.
#' * `summary`: See [`summary`] for further information.
#' * `values`: See [`values`] for further information.
#'
#' @section Fields:
#' A `DTSg` object has the following fields or properties as they are often
#' called. They are implemented through so called active bindings, which means
#' that they can be accessed and actively set with the help of the `$` operator,
#' for instance, `x$ID` gets the value of the `ID` field and
#' `x$ID <- "River Flow"` sets its value. Please note that fields are always
#' modified in place, i.e. no deep clone (copy) of the object is made
#' beforehand. See [`clone`] for further information. Some of the fields are
#' read-only though:
#' * `aggregated`: Same as the `aggregated` argument.
#' * `fast`: Same as the `fast` argument.
#' * `funbyApproach`: Same as the `funbyApproach` argument.
#' * `ID`: Same as the `ID` argument. It is used as the title of plots.
#' * `na.status`: Same as the `na.status` argument. When set, the missing values
#' of the object are expanded or collapsed accordingly.
#' * `parameter`: Same as the `parameter` argument. It is used as the label of
#' the primary axis of plots.
#' * `periodicity`: A [`difftime`] object for a regular and a character string
#' for an irregular `DTSg` object describing its periodicity or containing
#' `"unrecognised"` in case it could not be detected. When set, the periodicity
#' of the time series is changed as specified. See the `by` argument of
#' [`alter`] for further information.
#' * `regular`: A logical signalling if all lags in seconds between subsequent
#' timestamps are the same (`TRUE`) or if some are different (`FALSE`). A, for
#' instance, monthly time series is considered irregular in this sense
#' (read-only).
#' * `timestamps`: An integer showing the total number of timestamps of the time
#' series (read-only).
#' * `timezone`: A character string showing the time zone of the time series.
#' When set, the series is converted to the specified time zone. Only names from
#' [`OlsonNames`] are accepted.
#' * `unit`: Same as the `unit` argument. It is added to the label of the
#' primary axis of plots when the `parameter` field is set.
#' * `variant`: Same as the `variant` argument. It is added to the label of the
#' primary axis of plots when the `parameter` field is set.
#'
#' The `parameter`, `unit` and `variant` fields are especially useful for time
#' series of a single variable. For time series of multiple variables with
#' differing units the functionality of the \pkg{units} package may pose a
#' viable alternative.
#'
#' @section Options:
#' The behaviour of `DTSg` objects can be customised with the help of the
#' following option. See [`options`] for further information:
#' * _DTSgClone:_ A logical specifying if `DTSg` objects are, by default,
#' modified in place (`FALSE`) or if a deep clone (copy) shall be made
#' beforehand (`TRUE`).
#' * _DTSgDeprecatedWarnings:_ A logical specifying if warnings are displayed
#' when calling deprecated features.
#' * _DTSgFast:_ Default value for the `fast` argument.
#' * _DTSgFunbyApproach:_ Default value for the `funbyApproach` argument.
#' * _DTSgNA.status:_ Default value for the `na.status` argument.
#'
#' @note
#' Due to the [`POSIXct`] nature of the _.dateTime_ column, the same sub-second
#' accuracy, issues and limitations apply to `DTSg` objects. In order to prevent
#' at least some of the possible precision issues, the lags between subsequent
#' timestamps are rounded to microseconds during integrity checks. This
#' corresponds to the maximum value allowed for
#' \code{\link{options}("digits.secs")}. As a consequence, time series with a
#' sub-second accuracy higher than a microsecond will never work.
#'
#' @examples
#' # new DTSg object
#' ## R6 constructor
#' DTSg$new(
#'   values = flow,
#'   ID = "River Flow"
#' )
#'
#' ## abused S4 constructor
#' new(
#'   Class = "DTSg",
#'   values = flow,
#'   ID = "River Flow"
#' )
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
    .funbyApproach = character(),
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
          stop(sprintf(
            "Cannot coerce %s because %s.",
            msgPart,
            deparse(e$message)
          ), call. = FALSE)
        },
        warning = function(w) {
          stop(sprintf(
            "Will not coerce %s because %s.",
            msgPart,
            deparse(w$message)
          ), call. = FALSE)
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
        resultCols <- private$extractCols(
          resultCols,
          colon = FALSE,
          len = length(cols),
          .var.name = "resultCols"
        )

        assertNoStartingDot(resultCols)
      } else if (!is.null(suffix)) {
        qassert(suffix, "S1")

        assertDisjunct(sprintf("%s%s", cols, suffix), self$cols())
      } else {
        cols
      }
    },

    determineFilter = function(i, expr) {
      tryCatch(
        {
          if (!testMultiClass(i, c("integer", "numeric")) && !is.expression(i) &&
                !is.character(i) && !is.list(i)) {
            i <- expr
          }

          i
        },
        error = function(e) {
          expr
        }
      )
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

    determineFun = function(fun, isNames) {
      if (!testClass(fun, "character")) {
        if (!testClass(fun, "list")) {
          fun <- list(fun)
        }
        lapply(fun, assertFunction, .var.name = "fun' or 'fun[[i]]")
      }
      if (isNames && length(fun) > 1L) {
        assertCharacter(names(fun), min.chars = 1L, any.missing = FALSE, unique = TRUE)
      }

      fun
    },

    determineLen = function(timestamps) {
      if (!private$.isFast || timestamps < 1000L) {
        timestamps
      } else {
        1000L
      }
    },

    determineTo = function(to, from) {
      if (qtest(to, "P1")) {
        assertSetEqual(attr(to, "tzone"), self$timezone)
      } else {
        to <- as.POSIXct(to, tz = private$.timezone)
      }

      assertPOSIXct(to, lower = from, any.missing = FALSE, len = 1L)
    },

    extractCols = function(
      cols,
      colon = TRUE,
      min.chars = 1L,
      any.missing = FALSE,
      len = NULL,
      min.len = 1L,
      unique = TRUE,
      .var.name = "cols"
    ) {
      qassert(cols, "S+")

      allCols <- names(private$.values)[-1L]

      if (length(cols) == 1L && !cols %chin% allCols) {
        if (colon && grepl(":", cols, fixed = TRUE)) {
          cols <- strsplit(cols, ":", fixed = TRUE)[[1L]]

          assertSubset(cols, allCols)

          startCol <- which(cols[1L] == allCols)
          endCol   <- which(cols[2L] == allCols)

          cols <- allCols[startCol:endCol]
        } else if (grepl(",", cols, fixed = TRUE)) {
          cols <- strsplit(cols, ",", fixed = TRUE)[[1L]]
        }
      }

      assertCharacter(
        cols,
        min.chars = min.chars,
        any.missing = any.missing,
        len = len,
        min.len = min.len,
        unique = unique,
        .var.name = .var.name
      )
      if (colon) {
        assertSubset(cols, allCols)
      }

      cols
    },

    funbyHelpers = function(ignoreDST, multiplier, funbyApproach, .helpers) {
      if (!is.null(.helpers)) {
        qassert(.helpers, "L+")
        helpers <- names(.helpers)
        assertCharacter(
          helpers,
          min.chars = 1L,
          any.missing = FALSE,
          unique = TRUE,
          .var.name = "names(funbyHelpers)"
        )

        if (any(helpers %chin% c("timezone", "periodicity", "na.status"))) {
          stop(
            '"timezone", "periodicity" and "na.status" helpers are not ',
            "allowed in this context.",
            call. = FALSE
          )
        }

        if ("ignoreDST" %chin% helpers) {
          ignoreDST <- qassert(
            .helpers[["ignoreDST"]],
            "B1",
            .var.name = 'funbyHelpers[["ignoreDST"]]'
          )
          .helpers[["ignoreDST"]] <- NULL
        }
        if ("multiplier" %chin% helpers) {
          multiplier <- assertCount(
            .helpers[["multiplier"]],
            positive = TRUE,
            coerce = TRUE,
            .var.name = 'funbyHelpers[["multiplier"]]'
          )
          .helpers[["multiplier"]] <- NULL
        }
        if ("funbyApproach" %chin% helpers) {
          funbyApproach <- .helpers[["funbyApproach"]]
          .helpers[["funbyApproach"]] <- NULL
        }
      }

      c(list(
        timezone = private$.timezone,
        ignoreDST = ignoreDST,
        periodicity = private$.periodicity,
        na.status = private$.na.status,
        multiplier = multiplier,
        funbyApproach = funbyApproach
      ), .helpers)
    },

    multiLapply = function(.SD, funs, ...) {
      do.call(c, lapply(
        .SD,
        function(x, ...) {
          lapply(funs, function(fun, y, ...) fun(y, ...), y = x, ... = ...)
        },
        ... = ...
      ))
    },

    optiLapply = function(funs, cols, resultCols, ...) {
      if (is.null(resultCols)) {
        funs <- rep(funs, length(cols))
        cols <- rep(cols, each = length(funs) / length(cols))
        if (!is.null(names(funs))) {
          resultCols <- sprintf("%s.%s", cols, names(funs))
        } else {
          resultCols <- cols
        }
      }

      dotsToCharacter <- function(...) {
        if (...length() > 0L) {
          dots <- list(...)
          dots <- sprintf("%s = %s", names(dots), dots)
          sprintf(", %s", toString(dots))
        } else {
          ""
        }
      }

      text <- toString(
        sprintf("%s = %s(%s%s)", resultCols, funs, cols, dotsToCharacter(...))
      )

      sprintf("list(%s)", text)
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
    `[` = function(...) {
      self$getCol(...)
    },

    aggregate = function(
      funby,
      fun,
      ...,
      cols = self$cols(class = "numeric"),
      n = FALSE,
      ignoreDST = FALSE,
      multiplier = 1L,
      funbyHelpers = NULL,
      funbyApproach = self$funbyApproach,
      clone = getOption("DTSgClone")
    ) {
      assertFunction(funby)
      qassert(ignoreDST, "B1")
      multiplier <- assertCount(multiplier, positive = TRUE, coerce = TRUE)
      .funbyHelpers <- private$funbyHelpers(
        ignoreDST,
        multiplier,
        funbyApproach,
        funbyHelpers
      )
      qassert(funby(
        self$values(reference = TRUE)[[".dateTime"]][1L],
        .funbyHelpers
      ), "P1")
      fun <- private$determineFun(fun, TRUE)
      cols <- private$extractCols(cols)
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
          multiplier = multiplier,
          funbyHelpers = funbyHelpers,
          funbyApproach = funbyApproach,
          clone = FALSE
        ))
      }

      if (testClass(fun, "character")) {
        expr <- expression(eval(parse(text = private$optiLapply(fun, cols, NULL, ...))))
      } else {
        expr <- expression(private$multiLapply(.SD, fun, ...))
      }

      if (n) {
        if (length(cols) > 1L) {
          private$.values <- private$.values[
            ,
            c(eval(expr), .(.n = .N)),
            keyby = .(.dateTime = funby(.dateTime, .funbyHelpers)),
            .SDcols = cols
          ]

          message(".n column calculated from .dateTime column.")
        } else {
          private$.values <- private$.values[
            !is.na(get(cols)),
            c(eval(expr), .(.n = .N)),
            keyby = .(.dateTime = funby(.dateTime, .funbyHelpers)),
            .SDcols = cols
          ]

          message(
            "Missing values are always stripped regardless of the value of a ",
            'possible "na.rm" argument.'
          )
        }
      } else {
        private$.values <- private$.values[
          ,
          .j,
          keyby = .(.dateTime = funby(.dateTime, .funbyHelpers)),
          .SDcols = cols,
          env = list(.j = expr)
        ]

        if (n) {
          message(".n column calculated from .dateTime column.")
        }
      } else {
        private$.values <- private$.values[
          !is.na(.cols),
          .j,
          keyby = .(.dateTime = funby(.dateTime, .funbyHelpers)),
          .SDcols = cols,
          env = list(.cols = cols, .j = expr)
        ]

        if (n) {
          message(
            "Missing values are always stripped regardless of the value of a ",
            'possible "na.rm" argument.'
          )
        }
      }

      if (!is.null(names(fun))) {
        resultCols <- sprintf(
          "%s.%s",
          rep(cols, length(fun)),
          rep(names(fun), each = length(cols))
        )

        setnames(private$.values, 2:(length(resultCols) + 1L), resultCols)
      }
      if (n) {
        setnames(private$.values, length(private$.values), ".n")
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
        if (rollback && mday(from) > 28L && grepl("^\\d+ (month|year)(s?)$", by)) {
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
          "Only time series with recognised periodicity can have explicitly missing values.\n",
          'Consider calling "alter()" with "na.status = \'explicit\'" and specified "by" argument.',
          call. = FALSE
        )
      }

      if (na.status == "implicit") {
        allNA <- rowSums(is.na(private$.values[, -1L])) == ncol(private$.values) - 1L

        if (any(allNA)) {
          private$.values <- private$.values[!allNA, ]

          self$refresh()
        }

        private$.na.status <- na.status
      } else if (na.status == "undecided" && private$.na.status != "undecided") {
        stop(
          "Status of missing values has already been decided on.",
          call. = FALSE
        )
      }

      invisible(self)
    },

    colapply = function(
      fun,
      ...,
      cols = self$cols(class = "numeric")[1L],
      resultCols = NULL,
      suffix = NULL,
      helpers = TRUE,
      funby = NULL,
      ignoreDST = FALSE,
      multiplier = 1L,
      funbyHelpers = NULL,
      funbyApproach = self$funbyApproach,
      clone = getOption("DTSgClone")
    ) {
      assertFunction(fun)
      cols <- private$extractCols(cols)
      .cols <- private$determineCols(resultCols, suffix, cols)
      qassert(helpers, "B1")
      qassert(clone, "B1")

      if (clone) {
        TS <- self$clone(deep = TRUE)
        return(TS$colapply(
          fun = fun,
          ... = ...,
          cols = cols,
          resultCols = resultCols,
          suffix = suffix,
          helpers = helpers,
          funby = funby,
          ignoreDST = ignoreDST,
          multiplier = multiplier,
          funbyHelpers = funbyHelpers,
          funbyApproach = funbyApproach,
          clone = FALSE
        ))
      }

      if (!is.null(funby)) {
        assertFunction(funby)
        qassert(ignoreDST, "B1")
        multiplier <- assertCount(multiplier, positive = TRUE, coerce = TRUE)
        .funbyHelpers <- private$funbyHelpers(
          ignoreDST,
          multiplier,
          funbyApproach,
          funbyHelpers
        )
        assertAtomic(funby(
          self$values(reference = TRUE)[[".dateTime"]][1L],
          .funbyHelpers
        ), any.missing = FALSE, len = 1L)

        by <- funby(private$.values[[".dateTime"]], .funbyHelpers)
      } else {
        by <- NULL
      }

      if (helpers) {
        .helpers <- list(
          .dateTime = private$.values[[".dateTime"]],
          periodicity = private$.periodicity,
          minLag = private$.minLag,
          maxLag = private$.maxLag
        )

        expr <- quote((.cols) := lapply(.SD, fun, ..., .helpers = .helpers))
      } else {
        expr <- quote((.cols) := lapply(.SD, fun, ...))
      }

      private$.values[
        ,
        .j,
        by = by,
        .SDcols = cols,
        env = list(.j = expr)
      ]

      invisible(self)
    },

    cols = function(
      class = NULL,
      pattern = NULL,
      mode = NULL,
      typeof = NULL,
      ...
    ) {
      cols <- names(private$.values)[-1L]

      if (!is.null(class)) {
        qassert(class, "S+")

        if (".numerary" %chin% class) {
          class <- c(setdiff(class, ".numerary"), "integer", "numeric")
        }

        classes <- vapply(
          private$.values[, -1L],
          function(col) class(col)[1L],
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

      if (!is.null(mode)) {
        qassert(mode, "S+")

        modes <- vapply(
          private$.values[, ..cols],
          function(col) mode(col), # nolint
          character(1L)
        )

        cols <- cols[modes %chin% mode]
      }

      if (!is.null(typeof)) {
        qassert(typeof, "S+")

        typeofs <- vapply(
          private$.values[, ..cols],
          function(col) typeof(col), # nolint
          character(1L)
        )

        cols <- cols[typeofs %chin% typeof]
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
      fast = getOption("DTSgFast"),
      swallow = FALSE,
      na.status = getOption("DTSgNA.status"),
      funbyApproach = getOption("DTSgFunbyApproach")
    ) {
      assertDataFrame(values, min.rows = 1L, min.cols = 2L)
      assertCharacter(
        names(values)[-1L],
        min.chars = 1L,
        any.missing = FALSE,
        unique = TRUE
      )
      assertNoStartingDot(names(values)[-1L])
      qassert(swallow, "B1")
      na.status <- match.arg(na.status, private$.na.statuses)

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
      self$funbyApproach <- funbyApproach

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

      values <- merge(
        private$.values,
        y$values(TRUE),
        ...
      )
      len <- private$determineLen(nrow(values))
      assertPOSIXct(
        values[[".dateTime"]][seq_len(len)],
        any.missing = FALSE,
        min.len = 1L,
        unique = TRUE,
        .var.name = sprintf('self$values(reference = TRUE)[[".dateTime"]][seq_len(%s)]', len)
      )

      private$.values <- values

      self$refresh()
      self$alter(clone = FALSE)

      invisible(self)
    },

    names = function(...) {
      self$cols(...)
    },

    nas = function(cols = self$cols()) {
      assertNAstatusPeriodicityOK(
        private$.na.status,
        private$.periodicity,
        level = "warning"
      )
      cols <- private$extractCols(cols)

      DTs <- vector("list", length(cols))

      for (i in seq_along(cols)) {
        if (anyNA(private$.values[[cols[i]]])) {
          DT <- private$.values[
            ,
            .(.dateTime, .col = .cols, .group = rleid(.cols)),
            env = list(.cols = cols[i])
          ]

          DT <- DT[
            is.na(.col),
            .(.from = min(.dateTime), .to = max(.dateTime), .n = .N),
            by = .(.col, .group = rleid(.group))
          ]
          DT[, .col := as.character(.col)]
          DT[, .col := cols[i]]

          DTs[[i]] <- DT
        } else {
          DTs[[i]] <- data.table(
            .col = character(),
            .group = integer(),
            .from = .POSIXct(numeric(), tz = private$.timezone),
            .to = .POSIXct(numeric(), tz = private$.timezone),
            .n = integer()
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
      cols <- private$extractCols(cols)

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
          between(.dateTime, from, to),
          c(".dateTime", ..cols)
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
        secAxisCols <- private$extractCols(cols)
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
      print(private$.values, nrows = 11L, class = TRUE, print.keys = FALSE)
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

    raggregate = function(...) {
      self$rowaggregate(...)
    },

    rbind = function(...) {
      self$rowbind(...)
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
        stop(
          ".dateTime column must not have any missing values.",
          call. = FALSE
        )
      }

      if (private$.timestamps < 2L) {
        private$.minLag <- .difftime(0, units = "secs")
        private$.maxLag <- .difftime(0, units = "secs")
        private$.isRegular <- TRUE
        private$.periodicity <- "unrecognised"
      } else {
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
            if (mday(from) > 28L && grepl("^\\d+ (month|year)(s?)$", by)) {
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
            if (all(lags >= private$.minLag) && all(lags <= private$.maxLag) &&
                  sum(!is.na(DT[, -1L])) == sum(!is.na(private$.values[seqLen, -1L]))) {
              private$.periodicity <- by

              break
            }
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
      weights = "inverseDistance",
      parameters = list(power = 1),
      resultCols = NULL,
      suffix = NULL,
      helpers = TRUE,
      memoryOverCPU = TRUE,
      clone = getOption("DTSgClone")
    ) {
      assertNAstatusPeriodicityOK(
        private$.na.status,
        private$.periodicity,
        level = "warning"
      )
      assertFunction(fun)
      cols <- private$extractCols(cols)
      before <- assertCount(before, coerce = TRUE)
      after <- assertCount(after, coerce = TRUE)
      weights <- match.arg(weights)
      qassert(parameters, "L+")
      .cols <- private$determineCols(resultCols, suffix, cols)
      qassert(helpers, "B1")
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
          helpers = helpers,
          memoryOverCPU = memoryOverCPU,
          clone = FALSE
        ))
      }

      if (weights == "inverseDistance") {
        qassert(parameters[["power"]], "N1()")

        weights <- 1 / c(
          rev(seq_len(before) + 1),
          1,
          seq_len(after) + 1
        )^parameters[["power"]]
        weights <- weights / sum(weights)
      }

      .helpers <- list(
        before = before,
        after = after,
        windowSize = before + 1L + after,
        centerIndex = before + 1L
      )

      if (memoryOverCPU) {
        wapply <- function(x, fun, ..., before, after, weights) {
          L <- shift(list(x), before:0)
          if (after > 0L) {
            L <- c(L, shift(list(x), seq_len(after), type = "lead"))
          }

          if (helpers) {
            apply(
              matrix(unlist(L), ncol = length(L)),
              1L,
              fun,
              ...,
              w = weights,
              .helpers = .helpers
            )
          } else {
            apply(
              matrix(unlist(L), ncol = length(L)),
              1L,
              fun,
              ...
            )
          }
        }
      } else {
        wapply <- function(x, fun, ..., before, after, weights) {
          y <- vector(typeof(x), length(x))
          y[] <- NA

          for (i in seq_along(x)) {
            lowerBound <- i - before

            if (helpers) {
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
            } else {
              y[i] <- fun(
                if (lowerBound < 1L) {
                  c(rep(NA, abs(lowerBound) + 1L), x[seq_len(i + after)])
                } else {
                  x[lowerBound:(i + after)]
                },
                ...
              )
            }
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

    rowaggregate = function(
      resultCols,
      fun,
      ...,
      cols = self$cols(class = "numeric"),
      clone = getOption("DTSgClone")
    ) {
      if (length(resultCols) > 1L && length(cols) > 1L) {
        assertCharacter(resultCols, min.chars = 1L, any.missing = FALSE, len = length(fun))
      } else {
        assertCharacter(resultCols, min.chars = 1L, any.missing = FALSE, len = 1L)
        if (!clone && length(names(fun)) > 0L) {
          resultCols <- sprintf("%s.%s", resultCols, names(fun))
        }
      }
      assertNoStartingDot(resultCols)
      assertDisjunct(resultCols, self$cols())
      fun <- private$determineFun(fun, length(fun) != length(resultCols))
      cols <- private$extractCols(cols, min.len = 2L)
      qassert(clone, "B1")

      if (clone) {
        TS <- self$clone(deep = TRUE)
        return(TS$rowaggregate(
          resultCols = resultCols,
          fun = fun,
          ... = ...,
          cols = cols,
          clone = FALSE
        ))
      }

      expr <- private$fapply(unname(fun), rowaggregate = TRUE, ...)

      private$.values[
        ,
        (resultCols) := .j,
        by = seq_len(private$.timestamps),
        .SDcols = cols,
        env = list(.j = expr)
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
                function(j, x) x[[j]],
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
        .var.name = sprintf('self$values(reference = TRUE)[[".dateTime"]][seq_len(%s)]', len)
      )

      private$.values <- values

      self$refresh()
      self$alter(clone = FALSE)

      invisible(self)
    },

    set = function(...) {
      self$setCols(...)
    },

    setColNames = function(
      cols = self$cols(class = "numeric")[1L],
      values,
      clone = getOption("DTSgClone")
    ) {
      cols <- private$extractCols(cols)
      values <- private$extractCols(
        values,
        colon = FALSE,
        len = length(cols)
      )
      assertNoStartingDot(values)

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
      cols <- private$extractCols(
        cols,
        colon = FALSE
      )
      assertNoStartingDot(cols)
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
        private$.values[.i, (cols) := values, env = list(.i = i)]
      } else {
        private$.values[, (cols) := values]
      }

      invisible(self)
    },

    setnames = function(...) {
      self$setColNames(...)
    },

    subset = function(
      i,
      cols = self$cols(),
      funby = NULL,
      ignoreDST = FALSE,
      na.status = "implicit",
      clone = getOption("DTSgClone"),
      multiplier = 1L,
      funbyHelpers = NULL,
      funbyApproach = self$funbyApproach
    ) {
      if (!missing(i)) {
        i <- private$determineFilter(i, as.expression(substitute(i)))
        assertFilter(i, private$.timestamps)
      }
      cols <- private$extractCols(cols)
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
          clone = FALSE,
          multiplier = multiplier,
          funbyHelpers = funbyHelpers,
          funbyApproach = funbyApproach
        ))
      }

      cols <- c(".dateTime", cols)

      if (!missing(i)) {
        if (!is.null(funby)) {
          assertFunction(funby)
          qassert(ignoreDST, "B1")
          multiplier <- assertCount(multiplier, positive = TRUE, coerce = TRUE)
          .funbyHelpers <- private$funbyHelpers(
            ignoreDST,
            multiplier,
            funbyApproach,
            funbyHelpers
          )
          assertAtomic(funby(
            self$values(reference = TRUE)[[".dateTime"]][1L],
            .funbyHelpers
          ), any.missing = FALSE, len = 1L)

          values <- private$.values[
            ,
            .SD[.i, env = list(.i = i)],
            by = .(.group = funby(.dateTime, .funbyHelpers)),
            .SDcols = cols
          ]
          values[, .group := NULL]
        } else {
          values <- private$.values[.i, ..cols, env = list(.i = i)]
        }
      } else {
        values <- private$.values[, ..cols]
      }

      assertDataTable(values, min.rows = 1L, .var.name = "self$values(reference = TRUE)")

      private$.values <- values

      self$refresh()
      self$alter(clone = FALSE, na.status = na.status)

      invisible(self)
    },

    summary = function(cols = self$cols(), ...) {
      cols <- private$extractCols(cols)

      summary(private$.values[, ..cols], ...)
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

    funbyApproach = function(value) {
      if (missing(value)) {
        private$.funbyApproach
      } else {
        qassert(value, "S1")

        private$.funbyApproach <- value

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
