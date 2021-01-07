#' Get TRX market data for a date
#'
#' Retrieves TRX market data for a specific date (price, market cap, high/low, etc.)
#'
#' @param date (character, Date or POSIXct): date to retrieve the data for.
#'     Expected format: `%Y-%m-%d`. Please note that the minimal acceptable
#'     `date` is `2017-11-09` as no data are available for earlier dates.
#'     Attempts to retrieve data for earlier dates will fail with the
#'     corresponding error. Attempts to request a future `date`
#'     for which no history exists yet will fail as well.
#' @eval function_params(c("coingecko_vs_currencies", "max_attempts"))
#'
#' @return A tibble with as many rows as the length of `vs_currencies` and
#'     the following columns:
#' * `date` (Date): same as `date`;
#' * `vs_currency` (character): same as `vs_currencies`;
#' * `market_cap` (double): market cap as of `date`;
#' * `total_trading_vol` (double): total trading volume on `date`;
#' * `price` (double): average price on `date`.
#'
#' @export
#'
#' @examples
#' r <- get_trx_market_data_for_date(date = Sys.Date() - 1)
#' print(r)
get_trx_market_data_for_date <- function(date,
                                         vs_currencies = c("usd", "eur"),
                                         max_attempts = 3L) {
  tronr::validate_arguments(
    arg_max_attempts = max_attempts,
    arg_vs_currencies = vs_currencies
  )

  if (!(is.character(date) |
    inherits(date, "POSIXct") |
    inherits(date, "Date"))) {
    rlang::abort("`date` must be a character, POSIXct or date value")
  }

  if (is.character(date)) {
    date <- as.Date(date, format = "%Y-%m-%d")

    if (is.na(date)) {
      rlang::abort("`date` cannot be coerced to a Date value")
    }
  } else {
    date <- as.Date(date)
  }

  if (date > Sys.Date()) {
    message("Cannot retrieve data for a future `date`")
    return(NULL)
  }

  if (date < as.Date("2017-11-09")) {
    message("No data are available for dates before 2017-11-09")
    return(NULL)
  }

  supported_currencies <- tronr::get_supported_coingecko_currencies(
    max_attempts = max_attempts
  )

  if (!all(vs_currencies %in% supported_currencies)) {
    rlang::abort(c(
      "The following currencies are not currently supported:",
      vs_currencies[!vs_currencies %in% supported_currencies]
    ))
  }

  date_split <- unlist(strsplit(as.character(date), split = "-"))

  date_reversed <- paste(date_split[3],
    date_split[2],
    date_split[1],
    sep = "-"
  )

  query_params <- list(
    date = date_reversed,
    localization = tolower(FALSE)
  )

  url <- tronr::build_get_request(
    base_url = "https://api.coingecko.com",
    path = c("api", "v3", "coins", "tron", "history"),
    query_parameters = query_params
  )

  r <- tronr::api_request(url = url, max_attempts = max_attempts)$market_data

  if (is.null(r)) {
    message("No data are available for this `date`")
    return(NULL)
  }

  result <- tibble::tibble(
    date = as.Date(date),
    vs_currency = vs_currencies,
    market_cap = as.numeric(r$market_cap[vs_currencies]),
    total_trading_vol = as.numeric(r$total_volume[vs_currencies]),
    price = as.numeric(r$current_price[vs_currencies]),
  )

  return(result)
}
