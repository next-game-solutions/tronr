#' Convert to Unix timestamp
#'
#' Converts a Unix timestamp to a POSIXct datatime value
#'
#' @param ts A Unix timestamp, _including milliseconds_. Either a numeric or
#'     a character value.
#' @param tz Character value corresponding to the timezone of the returned
#'     datetime value. Defaults to `UTC`.
#'
#' @return A `POSIXct` datetime value in the format `%Y-%m-%d %H:%M:%S`.
#' @export
#'
#' @examples
#' ts <- 60*60*24*1000
#' from_unix_timestamp(ts, tz = "UTC")
#'
from_unix_timestamp <- function(ts, tz = "UTC") {

  if (!(is.character(ts) | is.numeric(ts))) {
    stop("`ts` is neither a numeric nor a character value", call. = FALSE)
  }

  dt <- suppressWarnings(as.numeric(ts) / 1000)

  if (is.na(dt)) {
    stop("`ts` cannot be coerced to a POSIXct value", call. = FALSE)
  }

  dt <- as.POSIXct(dt,
                   origin = as.Date("1970-01-01"),
                   tz = tz,
                   format = "%Y-%m-%d %H:%M:%S")


  return(dt)

}
