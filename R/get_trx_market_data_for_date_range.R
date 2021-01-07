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
#' @param from_date (POSIXct): start of the time range to retrieve the data for.
#'     Expected format: `%Y-%m-%d %H:%M:%S`. The minimal acceptable `from_date`
#'     is `2017-11-09 00:00:00` as no data are available for earlier dates.
#'     Attempts to retrieve data for earlier dates will fail with the
#'     corresponding error.
#' @param to_date (POSIXct): end of the time range to retrieve the data for
#'     (inclusive). Expected format: `%Y-%m-%d %H:%M:%S`. Attempts to request a
#'     future `to_date` for which no history exists yet will fail with the
#'     corresponding error.
#' @eval function_params("max_attempts")
#'
#' @details Minutely data will typically be used for periods of <1 day,
#'     hourly data for periods between 1 day and 90 days, and daily data for
#'     periods above 90 days. Please note that these data granularity intervals
#'     are only approximate and a substantial variation may be observed.
#'
#' @return A tibble with the following columns:
#' * `datetime` (POSIXct): timestamp;
#' * `vs_currency` (character): same as the argument `vs_currency`;
#' * `price` (double): TRX price, as of `datetime`;
#' * `total_trading_vol` (double): a 24 h rolling-window trading volume, as
#' of `datetime`;
#' * `market_cap` (double): TRX market cap, as of `datetime`.
#'
#' @export
#'
#' @examples from <- as.POSIXct("2021-01-01 10:00:10", tz = "UTC")
#' to <- as.POSIXct("2021-01-01 20:45:00", tz = "UTC")
#' r <- get_trx_market_data_for_date_range(vs_currency = "eur",
#'                                         from_date = from,
#'                                         to_date = to)
#' print(r)
get_trx_market_data_for_date_range <- function(vs_currency = "usd",
                                               from_date,
                                               to_date,
                                               max_attempts = 3L) {

  if (length(vs_currency) > 1L) {
    rlang::abort("Only one `vs_currency` at a time is allowed")
  }

  tronr::validate_arguments(
    arg_vs_currencies = vs_currency,
    arg_max_attempts = max_attempts
  )

  if (!inherits(from_date, "POSIXct")) {
    rlang::abort("`from_date` must be a POSIXct value")
  }

  if (!inherits(to_date, "POSIXct")) {
    rlang::abort("`to_date` must be a POSIXct value")
  }

  if (to_date > Sys.Date()) {
    message("Cannot retrieve data for a future `to_date`")
    return(NULL)
  }

  if (from_date < as.Date("2017-11-09")) {
    message("`from_date` must be larger than 2017-11-09")
    return(NULL)
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
    from = as.character(as.numeric(from_date)),
    to = as.character(as.numeric(to_date))
  )

  url <- tronr::build_get_request(
    base_url = "https://api.coingecko.com",
    path = c("api", "v3", "coins", "tron", "market_chart", "range"),
    query_parameters = query_params
  )

  r <- tronr::api_request(url = url, max_attempts = max_attempts)

  prices <- lapply(r$prices, function(x) {
    tibble::tibble(
      datetime = tronr::from_unix_timestamp(x[[1]]),
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
