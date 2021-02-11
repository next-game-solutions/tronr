#' Get historical TRX market data
#'
#' Retrieves TRX market data for the last `n` days
#'
#' @param vs_currency (character): name of the base currency to benchmark TRX
#'     against (`usd` by default). An up-to-date list of supported currencies
#'     (both fiat and cryptocurrencies) can be retrieved with the
#'     [get_supported_coingecko_currencies()] function. If an unsupported
#'     `vs_currency` is requested, the call will fail with the respective error
#'     message.
#' @param days (numeric or `"max"`): number of days to look back. If the
#'     requested number of `days` covers dates before `2017-11-09`, the retrived
#'     data will be clipped at `2017-11-09` (the beginning of history for TRX).
#'     If `days = "max"`, the entire available history will be retrieved.
#'     Depending on the value of `days`, the time interval used to present the
#'     data will differ - see "Details".
#' @param interval (character or `NULL`): time interval to present the data.
#'     The only currently supported interval is `daily`. Defaults to `NULL`.
#' @eval function_params("max_attempts")
#'
#' @details If `days = 1` and `interval = NULL`, the data will be presented for
#'    every few minutes (typically 3-8 minutes). If `days` is between 2 and 90
#'    (inclusive) and `interval = NULL`, an (approximately) hourly time step will
#'    be used. Daily data are used for `days` above 90. If `interval = "daily"`,
#'    daily data will be used irrespective of the value of `days`.
#'
#' @return A tibble with the following columns:
#' * `timestamp` (POSIXct);
#' * `vs_currency` (character): same as the argument `vs_currency`;
#' * `price` (double): TRX price, as of `datetime`;
#' * `total_trading_vol` (double): a 24 h rolling-window trading volume, as
#' of `datetime`;
#' * `market_cap` (double): TRX market cap, as of `datetime`.
#'
#' @importFrom magrittr %>%
#'
#' @export
#'
#' @examples
#' r <- get_trx_market_data_for_last_n_days(
#'   vs_currency = "gbp",
#'   days = 30
#' )
#' print(r)
get_trx_market_data_for_last_n_days <- function(vs_currency = "usd",
                                                days,
                                                interval = NULL,
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
    is.na(suppressWarnings(as.numeric(days))) && days != "max") {
    rlang::abort("`days` only accepts coercible-to-numeric values or a character value \"max\"")
  }

  if (!is.character(days) && (Sys.Date() - days) < as.Date("2017-11-09")) {
    days <- "max"
  }

  if (!is.null(interval) && interval != "daily") {
    rlang::abort("`interval` must be equal to NULL or \"daily\"")
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
    days = days,
    interval = interval
  )

  url <- tronr::build_get_request(
    base_url = "https://api.coingecko.com",
    path = c("api", "v3", "coins", "tron", "market_chart"),
    query_parameters = query_params
  )

  r <- tronr::api_request(url = url, max_attempts = max_attempts)

  if (length(r$prices) == 0) {
    message("No data found")
    return(NULL)
  }

  prices <- lapply(r$prices, function(x) {
    tibble::tibble(
      timestamp = tronr::from_unix_timestamp(x[[1]]),
      vs_currency = vs_currency,
      price = x[[2]]
    )
  }) %>%
    dplyr::bind_rows()

  market_caps <- lapply(r$market_caps, function(x) {
    tibble::tibble(
      market_cap = x[[2]]
    )
  }) %>%
    dplyr::bind_rows()

  total_volumes <- lapply(r$total_volumes, function(x) {
    tibble::tibble(
      total_trading_vol = x[[2]]
    )
  }) %>%
    dplyr::bind_rows()

  result <- dplyr::bind_cols(prices, total_volumes, market_caps)

  return(result)
}
