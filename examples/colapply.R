# new DTSg object
x <- DTSg$new(values = flow)

# linear interpolation of missing values
## R6 method
x$colapply(fun = interpolateLinear)$print()

## S3 method
print(colapply(x = x, fun = interpolateLinear))

# daily cumulative sums per month
## R6 method
x$colapply(
  fun = cumsum,
  helpers = FALSE,
  funby = byYm____
)$print()

## S3 method
print(colapply(
  x = x,
  fun = cumsum,
  helpers = FALSE,
  funby = byYm____
))

# calculate moving averages with the help of 'runner' (all four given
# approaches provide the same result with explicitly missing timestamps)
if (requireNamespace("runner", quietly = TRUE) &&
    packageVersion("runner") >= package_version("0.3.5")) {
  wrapper <- function(..., .helpers) {
    runner::runner(..., idx = .helpers[[".dateTime"]])
  }

  ## R6 method
  x$colapply(
    fun = runner::runner,
    f = mean,
    k = 5,
    lag = -2
  )$print()
  x$colapply(
    fun = wrapper,
    f = mean,
    k = "5 days",
    lag = "-2 days"
  )$print()
  x$colapply(
    fun = runner::runner,
    f = mean,
    k = "5 days",
    lag = "-2 days",
    idx = x$getCol(col = ".dateTime")
  )$print()
  x$colapply(
    fun = runner::runner,
    f = mean,
    k = "5 days",
    lag = "-2 days",
    idx = x[".dateTime"]
  )$print()

  ## S3 method
  print(colapply(
    x = x,
    fun = runner::runner,
    f = mean,
    k = 5,
    lag = -2
  ))
  print(colapply(
    x = x,
    fun = wrapper,
    f = mean,
    k = "5 days",
    lag = "-2 days"
  ))
  print(colapply(
    x = x,
    fun = runner::runner,
    f = mean,
    k = "5 days",
    lag = "-2 days",
    idx = getCol(x = x, col = ".dateTime")
  ))
  print(colapply(
    x = x,
    fun = runner::runner,
    f = mean,
    k = "5 days",
    lag = "-2 days",
    idx = x[".dateTime"]
  ))
}

# calculate rolling correlations somewhat inefficiently with the help of
# 'runner'
if (requireNamespace("runner", quietly = TRUE) &&
    packageVersion("runner") >= package_version("0.3.8")) {
  wrapper <- function(x, y, f, k, lag, ...) {
    runner::runner(
      cbind(x, y),
      f = function(x) f(x[, 1], x[, 2]),
      k = k,
      lag = lag
    )
  }

  ## R6 method
  x$colapply(
    fun = wrapper,
    y = x["flow"] + rnorm(length(x["flow"])),
    f = cor,
    k = 5,
    lag = -2
  )$print()

  ## S3 method
  print(colapply(
    x = x,
    fun = wrapper,
    y = x["flow"] + rnorm(length(x["flow"])),
    f = cor,
    k = 5,
    lag = -2
  ))
}
