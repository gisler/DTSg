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
