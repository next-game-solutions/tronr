#' Definitions of common arguments
#'
#' Used to automatically generate documentation for commonly occuring arguments
#'
#' @param arguments (character): vector with names of the arguments to be
#'     documented.
#'
#' @return Literal descriptions of function arguments.
#' @export
#'
function_params <- function(arguments) {
  descriptions <- c(
    owner_address = "@param owner_address (character): address of the
               token issuer, either in `base58check` or `hex` format.",

    address = "@param address (character): address of the account of interest,
               either in `base58check` or `hex` format.",

    contract_address = "@param contract_address (character): contract address of
               a TRC-20 token of interest, either in `base58check` or
               `hex` format.",

    token_id = "@param token_id (character): ID of the TRC-10 token of interest,
               presented either as a set of numbers (e.g., `\"1002000\"`).",

    token_name = "@param token_name (character): common name of the TRC-10
               token of interest (e.g., `BitTorrent`).",

    tx_id = "@param tx_id (character): transaction ID.",

    event_name = "@param event_name (character): name of the event of
               interest (e.g., `Transfer`). Defaults to `NULL`.",

    block_number = "@param block_number (character): number of the block of
               interest.",

    only_confirmed = "@param only_confirmed (boolean or `NULL`): if `NULL`
               (default) or `FALSE`, results are returned for both confirmed and
               unconfirmed transactions. If `TRUE`, only results for confirmed
               transactions are returned.",

    only_unconfirmed = "@param only_unconfirmed (boolean or `NULL`): if `NULL`
               (default) or `FALSE`, results are returned for both confirmed and
               unconfirmed transactions. If `TRUE`, only results for
               unconfirmed transactions are returned. Cannot be used
               simultanously with the `only_confirmed` argument.",

    only_to = "@param only_to (boolean, defaults to `FALSE`): if `TRUE`, only
               inbound transactions are returned.",

    only_from = "@param only_from (boolean, defaults to `FALSE`): if `TRUE`,
               only outbound transactions are returned.",

    min_timestamp = "@param min_timestamp (numeric or character): a Unix
               timestamp (_including milliseconds_), which defines the
               beginning of the period of interest (inclusive). Defaults to 0.",

    max_timestamp = "@param max_timestamp (numeric or character): a Unix
               timestamp (_including milliseconds_), which defines the
               end of the period of interest (inclusive).",

    detailed_info = "@param detailed_info (boolean): if `FALSE` (default),
               only basic information about the token is returned.
               If `TRUE`, an extended information is returned.",

    add_contract_data = "@param add_contract_data (boolean): if `TRUE` (default),
               adds column `contract_data` to the resultant tibble (see Value).",

    order_by = "@param order_by (character): specifies the variable to
               order by (see details).",

    direction = "@param direction (character): specifies the direction of
               ordering the results - descending (`desc`) or ascending (`asc`).",

    max_attempts = "@param max_attempts (integer, positive): specifies the
               maximum number of additional attempts to call a URL if the
               first attempt fails (i.e. its call status is different from
               `200`). Additional attempts are implemented with an exponential
               backoff. Defaults to `3`.",

    coingecko_vs_currencies = "@param vs_currencies (character): a vector with
       names of the base currencies to benchmark TRX against, e.g.
       `c(\"usd\", \"eur\", \"btc\")`. An up-to-date list of supported currencies
       (both fiat and cryptocurrencies) can be retrieved with the
       [get_supported_coingecko_currencies()] function. If `vs_currencies`
       contains at least one unsupported currency, the call will fail with the
       respective error message.",

    coingecko_api_note = "@details This function is based on the public
               CoinGecko API, which has a limit of 100 calls per minute. Please
               keep this limit in mind when developing your code."
  )

  return(descriptions[arguments])
}
