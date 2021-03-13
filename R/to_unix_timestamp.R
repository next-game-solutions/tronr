#' Convert to Unix timestamp
#'
#' Converts a POSIXct datetime to Unix timestamp
#'
#' @param datetime Either a `POSIXct` datetime value or a character value that
#'     can be coerced into such a `POSIXct` datetime value. Expected format:
#'     `%Y-%m-%d %H:%M:%S`. If `datetime` is a character value, then hours,
#'     minutes, and seconds must be provided explicitly (so, for example,
#'     a value of `"2010-01-01 00:00:00"` will work, while `"2010-01-01"` will
#'     throw an error).
#' @param tz Character value corresponding to the timezone of `datetime`.
#'     Defaults to `UTC`.
#'
#' @return A character value that corresponds to a Unix timestamp.
#'
#' @details All Unix timestamps returned by this function have a
#'     millisecond-level precision.
#'
#' @export
#'
#' @examples
#' dt <- "2019-01-01 00:00:10"
#' to_unix_timestamp(datetime = dt, tz = "UTC")
to_unix_timestamp <- function(datetime, tz = "UTC") {
  if (!(is.character(datetime) | inherits(datetime, "POSIXct"))) {
    rlang::abort("`datetime` is neither a character nor a POSIXct value")
  }

  if (is.character(datetime)) {
    ts <- lubridate::as_datetime(datetime,
      format = "%Y-%m-%d %H:%M:%S",
      tz = tz
    )

    if (is.na(ts)) {
      rlang::abort("`datetime` cannot be coerced to a POSIXct value")
    }
  }

  if (inherits(datetime, "POSIXct")) {
    attr(datetime, "tzone") <- "UTC"
    ts <- lubridate::round_date(datetime, unit = "second")
  }

  ts <- paste0(unclass(ts), "000")

  return(ts)
}
