#' Convert to Unix timestamp
#'
#' Converts a POSIX datetime to Unix timestamp
#'
#' @param datetime Either a `POSIXct` datetime value with a second-level
#'     precision or a character value that can be coerced into such a `POSIXct`
#'     datetime value. Expected format: `%Y-%m-%d %H:%M:%S`. Hours, minutes,
#'     and seconds must be provided explicitly (so, for example, a value of
#'     `2010-01-01 00:00:00` will work, while `2010-01-01` will throw
#'     an error).
#' @param tz Character value corresponding to the timezone of `datetime`.
#'     Defaults to `UTC`.
#'
#' @return A character value that corresponds to a Unix timestamp.
#'
#' @details The TronGrid API commands expect Unix timestamps with a
#'     millisecond-level precision. This function will therefore automatically
#'     add 3 more zeros after converting `datetime` into a Unix timestamp.
#'
#' @export
#'
#' @examples
#' dt <- "2019-01-01 00:00:10"
#' to_unix_timestamp(datetime = dt, tz = "UTC")
#'
to_unix_timestamp <- function(datetime, tz = "UTC") {

  if (!(is.character(datetime) | inherits(datetime, "POSIXct"))) {
    rlang::abort("`datetime` is neither a character nor a POSIXct value")
  }

  ts <- lubridate::as_datetime(datetime, format = "%Y-%m-%d %H:%M:%S")

  if (is.na(ts)) {
    rlang::abort("`datetime` cannot be coerced to a POSIXct value")
  }

  ts <- lubridate::round_date(ts, unit = "second")
  ts <- as.character(gmp::as.bigz(unclass(ts) * 1000))

  return(ts)

}
