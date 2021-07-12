#' Convert to POSIXct datetime values
#'
#' Converts a Unix timestamp to a POSIXct datetime value
#'
#' @param ts (character or numeric): Unix timestamps,
#'     _including milliseconds_.
#' @param tz (character): timezone of the returned datetime values.
#'     Defaults to `UTC`.
#'
#' @return A `POSIXct` datetime value in the format `%Y-%m-%d %H:%M:%S`.
#' @export
#'
#' @examples
#' ts1 <- 60 * 60 * 24 * 1000
#' ts2 <- as.character(60 * 60 * 24 * 1000)
#' from_unix_timestamp(ts1, tz = "UTC")
#' from_unix_timestamp(ts2, tz = "UTC")
from_unix_timestamp <- function(ts, tz = "UTC") {
  if (!(is.character(ts) | is.numeric(ts))) {
    rlang::abort("`ts` must be either numeric or character")
  }

  dt <- suppressWarnings(as.numeric(ts) / 1000)

  if (any(is.na(dt))) {
    rlang::abort("At least one `ts` value cannot be coerced to a POSIXct value")
  }

  dt <- as.POSIXct(dt,
    origin = as.Date("1970-01-01"),
    tz = tz,
    format = "%Y-%m-%d %H:%M:%S"
  )

  return(dt)
}
