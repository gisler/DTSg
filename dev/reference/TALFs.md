# Temporal Aggregation Level Functions (TALFs)

Simply hand over one of these functions to the `funby` argument of one
of the methods of a
[`DTSg`](https://gisler.github.io/DTSg/dev/reference/DTSg.md) object,
which supports it. The method then does the rest of the work. See
respective calling method for further information. Other uses are
possible, but not recommended.

## Usage

``` r
byY_____(.dateTime, .helpers)

byYQ____(.dateTime, .helpers)

byYm____(.dateTime, .helpers)

byYmd___(.dateTime, .helpers)

byYmdH__(.dateTime, .helpers)

byYmdHM_(.dateTime, .helpers)

byYmdHMS(.dateTime, .helpers)

by______(.dateTime, .helpers)

by_Q____(.dateTime, .helpers)

by_m____(.dateTime, .helpers)

by___H__(.dateTime, .helpers)

by____M_(.dateTime, .helpers)

by_____S(.dateTime, .helpers)
```

## Arguments

- .dateTime:

  A [`POSIXct`](https://rdrr.io/r/base/DateTimeClasses.html) vector.

- .helpers:

  A [`list`](https://rdrr.io/r/base/list.html) with helper data as
  handed over by methods of
  [`DTSg`](https://gisler.github.io/DTSg/dev/reference/DTSg.md) objects,
  which support the `funby` argument.

## Value

All functions return a
[`POSIXct`](https://rdrr.io/r/base/DateTimeClasses.html) vector with
timestamps corresponding to the function's temporal aggregation level.

## Families and flavours

There are two families of temporal aggregation level functions. The one
family truncates timestamps (truncating family), the other extracts a
certain part of them (extracting family). Each family comes in four
flavours: the first relies solely on base R, the second utilises
[`fasttime::fastPOSIXct`](https://rdrr.io/pkg/fasttime/man/fastPOSIXct.html)
of fasttime, the third
[`RcppCCTZ::parseDatetime`](https://rdrr.io/pkg/RcppCCTZ/man/parseDatetime.html)
of RcppCCTZ and the fourth
[`timechange::time_floor`](https://rdrr.io/pkg/timechange/man/time_round.html)
of timechange.

The timechange flavour generally is the fastest for both families of
functions and all time zones. Second best option for the extracting
family of functions generally is the fasttime flavour, which, however,
works with UTC and equivalent as well as all Etc/GMT time zones only
(execute
`grep("^(Etc/)?(UTC|UCT|Universal|Zulu)$|^(Etc/)?(GMT(\\+|-)?0?|Greenwich)$", OlsonNames(), ignore.case = TRUE, value = TRUE)`
for a full list of supported time zones) and is limited to timestamps
between the years 1970 and 2199. For time zones other than UTC and
equivalent the RcppCCTZ flavour generally is the second best option.

Use the `funbyApproach` argument of the respective calling method in
order to specify the utilised flavour.

The truncating family sets timestamps to the lowest possible point in
time of the corresponding temporal aggregation level:

- `byY_____` truncates to years, e.g. *2000-11-11 11:11:11.1* becomes
  *2000-01-01 00:00:00.0*

- `byYQ____` truncates to quarters, e.g. *2000-11-11 11:11:11.1* becomes
  *2000-10-01 00:00:00.0*

- `byYm____` truncates to months, e.g. *2000-11-11 11:11:11.1* becomes
  *2000-11-01 00:00:00.0*

- `byYmd___` truncates to days, e.g. *2000-11-11 11:11:11.1* becomes
  *2000-11-11 00:00:00.0*

- `byYmdH__` truncates to hours, e.g. *2000-11-11 11:11:11.1* becomes
  *2000-11-11 11:00:00.0*

- `byYmdHM_` truncates to minutes, e.g. *2000-11-11 11:11:11.1* becomes
  *2000-11-11 11:11:00.0*

- `byYmdHMS` truncates to seconds, e.g. *2000-11-11 11:11:11.1* becomes
  *2000-11-11 11:11:11.0*

By convention, the extracting family sets the year to 2199 and extracts
a certain part of timestamps:

- `by______` extracts nothing, i.e. all timestamps become *2199-01-01
  00:00:00.0*

- `by_Q____` extracts the quarters, e.g. *2000-11-11 11:11:11.1* becomes
  *2199-10-01 00:00:00.0*

- `by_m____` extracts the months, e.g. *2000-11-11 11:11:11.1* becomes
  *2199-11-01 00:00:00.0*

- `by___H__` extracts the hours, e.g. *2000-11-11 11:11:11.1* becomes
  *2199-01-01 11:00:00.0*

- `by____M_` extracts the minutes, e.g. *2000-11-11 11:11:11.1* becomes
  *2199-01-01 00:11:00.0*

- `by_____S` extracts the seconds, e.g. *2000-11-11 11:11:11.1* becomes
  *2199-01-01 00:00:11.0*

## See also

[`aggregate`](https://gisler.github.io/DTSg/dev/reference/aggregate.DTSg.md),
[`colapply`](https://gisler.github.io/DTSg/dev/reference/colapply.DTSg.md),
[`subset`](https://gisler.github.io/DTSg/dev/reference/subset.DTSg.md)
