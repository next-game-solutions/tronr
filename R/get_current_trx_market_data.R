#' Get TRX market data
#'
#' Retrieves the current market data for TRX (price, market cap, high/low, etc.)
#'
#' @eval function_params(c("coingecko_vs_currencies", "max_attempts"))
#'
#' @return A tibble with as many rows as the length of `vs_currencies`. Please
#'     refer to the ["Methodology"](https://www.coingecko.com/en/methodology)
#'     page on the CoinGecko website for definitions of variables in this
#'     tibble.
#' @export
#'
#' @examples
#' r <- get_current_trx_market_data(vs_currencies = c("usd", "eur"))
#' print(r)
get_current_trx_market_data <- function(vs_currencies = c("usd", "eur"),
                                        max_attempts = 3L) {
  tronr::validate_arguments(
    arg_vs_currencies = vs_currencies,
    arg_max_attempts = max_attempts
  )

  supported_currencies <- tronr::get_supported_coingecko_currencies(
    max_attempts = max_attempts
  )

  if (!all(vs_currencies %in% supported_currencies)) {
    rlang::abort(c(
      "The following currencies are not currently supported:",
      vs_currencies[!vs_currencies %in% supported_currencies]
    ))
  }

  query_params <- list(
    localization = tolower(FALSE),
    tickers = tolower(FALSE),
    market_data = tolower(TRUE),
    community_data = tolower(FALSE),
    developer_data = tolower(FALSE),
    sparkline = tolower(FALSE)
  )

  url <- tronr::build_get_request(
    base_url = "https://api.coingecko.com",
    path = c("api", "v3", "coins", "tron"),
    query_parameters = query_params
  )

  r <- tronr::api_request(url = url, max_attempts = max_attempts)

  market_data <- r$market_data

  result <- tibble::tibble(
    last_updated_at = as.POSIXct(gsub("Z", "", gsub("T", " ", r$last_updated)),
      tz = "UTC"
    ),

    total_supply = market_data$total_supply,

    circulating_supply = market_data$circulating_supply,

    vs_currency = vs_currencies,

    market_cap = as.numeric(
      market_data$market_cap[vs_currencies]
    ),

    market_cap_rank = market_data$market_cap_rank,

    market_cap_change_24h = as.numeric(
      market_data$market_cap_change_24h_in_currency[vs_currencies]
    ),

    market_cap_percentage_change_24h = as.numeric(
      market_data$market_cap_change_percentage_24h_in_currency[vs_currencies]
    ),

    total_trading_vol_24h = as.numeric(
      market_data$total_volume[vs_currencies]
    ),

    current_price = as.numeric(
      market_data$current_price[vs_currencies]
    ),

    price_high_24h = as.numeric(
      market_data$high_24h[vs_currencies]
    ),

    price_low_24h = as.numeric(
      market_data$low_24h[vs_currencies]
    ),

    price_change_24h = as.numeric(
      market_data$price_change_24h_in_currency[vs_currencies]
    ),

    price_percentage_change_24h =
      as.numeric(
        market_data$price_change_percentage_24h_in_currency[vs_currencies]
      ),

    price_percentage_change_7d =
      as.numeric(
        market_data$price_change_percentage_7d_in_currency[vs_currencies]
      ),

    price_percentage_change_14d =
      as.numeric(
        market_data$price_change_percentage_14d_in_currency[vs_currencies]
      ),

    price_percentage_change_30d =
      as.numeric(
        market_data$price_change_percentage_30d_in_currency[vs_currencies]
      ),

    price_percentage_change_60d =
      as.numeric(
        market_data$price_change_percentage_60d_in_currency[vs_currencies]
      ),

    price_percentage_change_200d =
      as.numeric(
        market_data$price_change_percentage_200d_in_currency[vs_currencies]
      ),

    price_percentage_change_1y =
      as.numeric(
        market_data$price_change_percentage_1y_in_currency[vs_currencies]
      ),

    ath = unlist(
      market_data$ath[vs_currencies]
    ),

    ath_change_percentage = unlist(
      market_data$ath_change_percentage[vs_currencies]
    ),

    ath_date = as.POSIXct(gsub("Z", "", gsub(
      "T", " ", market_data$ath_date[vs_currencies]
    )),
    tz = "UTC"
    ),

    atl = unlist(
      market_data$atl[vs_currencies]
    ),

    atl_change_percentage = unlist(
      market_data$atl_change_percentage[vs_currencies]
    ),

    atl_date = as.POSIXct(gsub("Z", "", gsub(
      "T", " ", market_data$atl_date[vs_currencies]
    )),
    tz = "UTC"
    ),

    coingecko_rank = r$coingecko_rank,

    coingecko_score = r$coingecko_score,

    developer_score = r$developer_score,

    community_score = r$community_score,

    liquidity_score = r$liquidity_score,

    public_interest_score = r$public_interest_score,

    sentiment_votes_up_percentage = r$sentiment_votes_up_percentage,

    sentiment_votes_down_percentage = r$sentiment_votes_down_percentage
  )

  return(result)
}
