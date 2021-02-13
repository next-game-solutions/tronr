#' Get Tronix price
#'
#' Retrieves the current TRX price and market data against other currencies
#'
#' @eval function_params("coingecko_vs_currencies")
#' @param include_market_cap (boolean, defaults to `FALSE`): whether to return
#'     the market cap information.
#' @param include_24h_vol (boolean, defaults to `FALSE`): whether to return
#'     the trading volume for the last 24 hours.
#' @param include_24h_change (boolean, defaults to `FALSE`): whether to return
#'     the price percentage change compared to 24 hours ago.
#' @eval function_params("max_attempts")
#'
#' @eval function_params("coingecko_api_note")
#'
#' @return A tibble with as many rows as the length of `vs_currencies` and at
#'     at least 3 columns: `trx_price` (double), `vs_currency` (character),
#'     and `last_updated_at` (POSIXct, UTC timezone). If the boolean arguments
#'     of this function are all `TRUE`, then the resultant tibble will
#'     include additional three columns: `market_cap` (double),
#'     `vol_24h` (double) and `price_percent_change_24h` (double).
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' r <- get_current_trx_price(vs_currencies = c("usd", "eur"))
#' print(r)
get_current_trx_price <- function(vs_currencies = "usd",
                                  include_market_cap = FALSE,
                                  include_24h_vol = FALSE,
                                  include_24h_change = FALSE,
                                  max_attempts = 3L) {
  if (!is.logical(include_market_cap)) {
    rlang::abort("`include_market_cap` must be boolean")
  }

  if (!is.logical(include_24h_vol)) {
    rlang::abort("`include_24h_vol` must be boolean")
  }

  if (!is.logical(include_24h_change)) {
    rlang::abort("`include_24h_change` must be boolean")
  }

  validate_arguments(
    arg_vs_currencies = vs_currencies,
    arg_max_attempts = max_attempts
  )

  supported_currencies <- get_supported_coingecko_currencies(
    max_attempts = max_attempts
  )

  if (!all(vs_currencies %in% supported_currencies)) {
    rlang::abort(c(
      "The following currencies are not currently supported:",
      vs_currencies[!vs_currencies %in% supported_currencies]
    ))
  }

  query_params <- list(
    ids = "tron",
    vs_currencies = paste0(vs_currencies, collapse = ","),
    include_market_cap = tolower(include_market_cap),
    include_24hr_vol = tolower(include_24h_vol),
    include_24hr_change = tolower(include_24h_change),
    include_last_updated_at = tolower(TRUE)
  )

  url <- build_get_request(
    base_url = "https://api.coingecko.com",
    path = c("api", "v3", "simple", "price"),
    query_parameters = query_params
  )

  r <- api_request(url = url, max_attempts = max_attempts)
  data <- r$tron

  if (include_market_cap) {
    market_cap <- as.numeric(
      data[paste(vs_currencies, "market_cap", sep = "_")]
    )
  } else {
    market_cap <- NULL
  }

  if (include_24h_vol) {
    vol_24h <- as.numeric(
      data[paste(vs_currencies, "24h_vol", sep = "_")]
    )
  } else {
    vol_24h <- NULL
  }

  if (include_24h_change) {
    change_24h <- as.numeric(
      data[paste(vs_currencies, "24h_change", sep = "_")]
    )
  } else {
    change_24h <- NULL
  }

  result <- tibble::tibble(
    trx_price = as.numeric(data[vs_currencies]),
    vs_currency = vs_currencies
  ) %>%
    dplyr::mutate(
      market_cap = market_cap,
      vol_24h = vol_24h,
      price_percent_change_24h = change_24h,
      last_updated_at = from_unix_timestamp(
        paste0(data$last_updated_at, "000")
      )
    )

  return(result)
}
