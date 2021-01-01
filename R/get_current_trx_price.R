get_current_trx_price <- function(vs_currencies = c("usd"),
                                  include_market_cap = FALSE,
                                  include_24h_vol = FALSE,
                                  include_24h_change = FALSE,
                                  max_attempts = 3L) {
  query_params <- list(
    ids = "tron",
    vs_currencies = paste0(vs_currencies, collapse = ","),
    include_market_cap = tolower(include_market_cap),
    include_24hr_vol = tolower(include_24h_vol),
    include_24hr_change = tolower(include_24h_change),
    include_last_updated_at = tolower(TRUE)
  )

  url <- tronr::build_get_request(
    base_url = "https://api.coingecko.com",
    path = c("api", "v3", "simple", "price"),
    query_parameters = query_params
  )

  r <- tronr::api_request(url = url, max_attempts = max_attempts)
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
      change_24h = change_24h,
      last_updated_at = tronr::from_unix_timestamp(
        paste0(data$last_updated_at, "000")
      )
    )

  return(result)
}
