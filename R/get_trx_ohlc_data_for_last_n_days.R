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
      datetime = tronr::from_unix_timestamp(x[[1]]),
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
