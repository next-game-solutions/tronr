#' Get historical TRX market data
#'
#' Retrieves TRX market data for a range of historical dates
#'
#' @param vs_currency (character): name of the base currency to benchmark TRX
#'     against (`usd` by default). An up-to-date list of supported currencies
#'     (both fiat and cryptocurrencies) can be retrieved with the
#'     [get_supported_coingecko_currencies()] function. If an unsupported
#'     `vs_currency` is requested, the call will fail with the respective error
#'     message.
#' @eval function_params(c("min_timestamp", "max_timestamp", "max_attempts"))
#'
#' @details This function returns hourly data for periods of up to 90 days,
#'     and daily data for periods above 90 days.
#'
#' The minimal acceptable `min_timestamp` is `"1510185600000"` (which
#'     corresponds to `2017-11-09 00:00:00`) as no data are available for
#'     earlier dates. Attempts to retrieve data for earlier dates will fail
#'     with the corresponding error message.
#'
#' Attempts to request a future `max_timestamp` for which no history exists
#'     yet will also fail with the corresponding error message.
#'
#' @return A tibble with the following columns:
#' * `timestamp` (POSIXct);
#' * `vs_currency` (character): same as the argument `vs_currency`;
#' * `price` (double): TRX price, as of `datetime`;
#' * `total_trading_vol` (double): a 24 h rolling-window trading volume, as
#' of `timestampt`;
#' * `market_cap` (double): TRX market cap, as of `timestamp`.
#'
#' @export
#'
#' @examples
#' r <- get_trx_market_data_for_time_range(
#'   vs_currency = "eur",
#'   min_timestamp = "1609495210000",
#'   max_timestamp = "1609533900000"
#' )
#' print(r)
get_trx_market_data_for_time_range <- function(vs_currency = "usd",
                                               min_timestamp,
                                               max_timestamp,
                                               max_attempts = 3L) {
  if (length(vs_currency) > 1L) {
    rlang::abort("Only one `vs_currency` at a time is allowed")
  }

  tronr::validate_arguments(
    arg_vs_currencies = vs_currency,
    arg_min_timestamp = min_timestamp,
    arg_max_timestamp = max_timestamp,
    arg_max_attempts = max_attempts
  )

  if (as.Date(from_unix_timestamp(max_timestamp)) > Sys.Date()) {
    rlang::abort("Cannot retrieve data for future dates. Check the `max_timestamp` argument")
  }

  if (as.Date(from_unix_timestamp(min_timestamp)) < as.Date("2017-11-09")) {
    rlang::abort("No data are available for dates before 2017-11-09. Check the `min_timestamp` argument")
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
    from = substr(min_timestamp, 1, nchar(min_timestamp) - 3),
    to = substr(max_timestamp, 1, nchar(max_timestamp) - 3)
  )

  url <- tronr::build_get_request(
    base_url = "https://api.coingecko.com",
    path = c("api", "v3", "coins", "tron", "market_chart", "range"),
    query_parameters = query_params
  )

  r <- tronr::api_request(url = url, max_attempts = max_attempts)

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
