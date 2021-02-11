#' Get historical TRX price data
#'
#' Retrieves TRX open-high-low-close price data for the last `n` days
#'
#' @param vs_currency (character): name of the base currency to benchmark TRX
#'     against (`usd` by default). An up-to-date list of supported currencies
#'     (both fiat and cryptocurrencies) can be retrieved with the
#'     [get_supported_coingecko_currencies()] function. If an unsupported
#'     `vs_currency` is requested, the call will fail with the respective error
#'     message.
#' @param days (numeric or `"max"`): number of days to look back. The only
#'     acceptable values are 1, 7, 14, 30, 90, 180, 365 and `"max"`. Attempts to
#'     assign any other values will fail with the corresponding console message.
#'     If `days = "max"`, the entire available history will be retrieved. If the
#'     requested number of `days` covers dates before `2017-11-09`, the retrived
#'     data will be clipped at `2017-11-09` (the beginning of history for TRX).
#'     Depending on the value of `days`, the time interval used to present the
#'     data will differ - see "Details".
#' @param max_attempts function_params("max_attempts")
#'
#' @details Granularity of the retrieved data
#'     (i.e. [candle](https://en.wikipedia.org/wiki/Open-high-low-close_chart)'s
#'     body) depends on the number of requested `days` as follows:
#' * 1 day: 30 minutes
#' * 7 - 30 days: 4 hours
#' * 31 and above: 4 days
#'
#' @return A tibble with the following columns:
#' * `timestamp` (POSIXct): timestamp;
#' * `vs_currency` (character): same as the argument `vs_currency`;
#' * `price_open` (double): TRX price in the beginning of a time iterval;
#' * `price_high` (double): highest TRX price observed within a time interval;
#' * `price_low` (double): lowest TRX price observed within a time interval;
#' * `price_close` (double): TRX price in the end of a time interval.
#'
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' r <- get_trx_ohlc_data_for_last_n_days(days = 7)
#' print(r)
get_trx_ohlc_data_for_last_n_days <- function(vs_currency = "usd",
                                              days,
                                              max_attempts = 3L) {
  if (length(vs_currency) > 1L) {
    rlang::abort("Only one `vs_currency` value is allowed")
  }

  if (length(days) > 1L) {
    rlang::abort("Only one `days` value is allowed")
  }

  tronr::validate_arguments(
    arg_vs_currencies = vs_currency,
    arg_max_attempts = max_attempts
  )

  if (is.na(days) |
    is.na(suppressWarnings(as.numeric(days))) &
      days != "max") {
    rlang::abort("`days` only accepts coercible-to-numeric values or a character value \"max\"")
  }

  if (is.numeric(days) & !days %in% c(1, 7, 14, 30, 90, 180, 365)) {
    rlang::abort("`days` only accepts the following numeric values: 1, 7, 14, 30, 90, 180, 365")
  }

  if (!is.character(days) && (Sys.Date() - days) < as.Date("2017-11-09")) {
    days <- "max"
  }

  supported_currencies <- tronr::get_supported_coingecko_currencies(
    max_attempts = max_attempts
  )

  if (!vs_currency %in% supported_currencies) {
    rlang::abort(paste(
      "Currency", vs_currency, "is not currently supported"
    ))
  }

  query_params <- list(
    vs_currency = vs_currency,
    days = days
  )

  url <- tronr::build_get_request(
    base_url = "https://api.coingecko.com",
    path = c("api", "v3", "coins", "tron", "ohlc"),
    query_parameters = query_params
  )

  r <- tronr::api_request(url = url, max_attempts = max_attempts)

  if (length(r) == 0) {
    message("No data found")
    return(NULL)
  }

  prices <- lapply(r, function(x) {
    tibble::tibble(
      timestamp = tronr::from_unix_timestamp(x[[1]]),
      vs_currency = vs_currency,
      price_open = x[[2]],
      price_high = x[[3]],
      price_low = x[[4]],
      price_close = x[[5]]
    )
  }) %>%
    dplyr::bind_rows()

  return(prices)
}
