#' Get TRC-20 transactions for a given account
#'
#' Returns various bits of information about the TRC-20 transactions
#' that went through a given account
#'
#' @param address (character) - address of the account of interest, in
#'     `base58` (starts with `T`) or `hex` (starts with `41`) format.
#' @param only_confirmed (boolean or `NULL`) - if `NULL` (default) or `FALSE`,
#'     both confirmed and unconfirmed transactions are returned. If `TRUE`,
#'     only confirmed transactions are returned. Cannot be used simultanously
#'     with the `only_unconfirmed` argument (see next).
#' @param only_unconfirmed (boolean or `NULL`) - if `NULL` (default) or `FALSE`,
#'     both confirmed and unconfirmed transactions are returned. If `TRUE`,
#'     only unconfirmed transactions are returned. Cannot be used simultanously
#'     with the `only_confirmed` argument.
#' @param only_to (boolean, defautls to `FALSE`) - if `TRUE`, only
#'     inbound transactions are returned.
#' @param only_from (boolean, defautls to `FALSE`) - if `TRUE`, only
#'     outbound transactions are returned.
#' @param min_timestamp (numeric or character) - a Unix timestamp
#'     (_including milliseconds_), which defines the beginning of the
#'     period to retrieve the transactions from. Defaults to 0.
#' @param max_timestamp (numeric or character) - a Unix timestamp
#'     (_including milliseconds_), which defines the end of the
#'     period to retrieve the transactions from.
#' @param contract_address (character) - address of the TRC20 token's
#'     contract, in `base58` or `hex` format.
#' @param max_attempts (integer, positive) - specifies the maximum
#'     number of additional attempts to call the API if the first attempt fails
#'     (i.e. its call status is different from `200`). Additional attempts are
#'     implemented with an exponential backoff. Defaults to 3.
#'
#' @details Some accounts may have a very high load of transactions going
#'     through them. Users are, therefore, advised to choose `min_timestamp` and
#'     `max_timestamp` wisely as the TronGrid API will not return more than
#'     10000 transactions in one request. If the number of transactions
#'     exceeds this limit, the the request will fail with status `404` and an
#'     error message
#'     `"Exceeds the maximum limit, please change your query time range"`). If
#'     data are to be retrieved for a large time range, users are advised
#'     to implement their own logic for querying the network (e.g.,
#'     splitting that range into smaller chunks and then combining the results).
#'
#' @return A tibble where each row corresponds to one transaction.
#'     This tibble contains the following columns:
#'
#' - `address` (character) - same as the argument `address`;
#' - `tx_id` (character) - transation ID;
#' - `tx_type` (character) - transation type (e.g., `"Transfer"`);
#' - `block_timestamp` (POSIXct, UTC timezone);
#' - `from_address` (character);
#' - `to_address` (character);
#' - `trc20_symbol` (character) - an abbreviated name of the TRC-20 token;
#' - `trc20_name` (character) - a common name of the TRC-20 token;
#' - `trc20_contract_address` (character);
#' - `precision` (integer) - precision of the `amount` values;
#' - `amount` (character) - transaction amount of the TRC-20 token.
#'
#' If no TRC20 are found for the specified combnations of query parameters,
#' nothing (NULL) is returned, with a console message
#' `"No data found"`.
#'
#' @export
#'
#' @examples tx_df <- get_tx_info_by_account_address(
#'                          address = "TAUN6FwrnwwmaEqYcckffC7wYmbaS6cBiX",
#'                          min_timestamp = "1604188800000",
#'                          max_timestamp = "1604189100000")
#' print(tx_df)
#'
get_trc20_tx_info_by_account_address <- function(address,
                                                 only_confirmed = NULL,
                                                 only_unconfirmed = NULL,
                                                 only_to = FALSE,
                                                 only_from = FALSE,
                                                 min_timestamp = 0,
                                                 max_timestamp = NULL,
                                                 contract_address = NULL,
                                                 max_attempts = 3L) {

  tronr::validate_arguments(arg_address = address,
                            arg_only_confirmed = only_confirmed,
                            arg_only_unconfirmed = only_unconfirmed,
                            arg_only_to = only_to,
                            arg_only_from = only_from,
                            arg_min_timestamp = min_timestamp,
                            arg_max_timestamp = max_timestamp,
                            arg_contract_address = contract_address,
                            arg_max_attempts = max_attempts)

  query_params <- list(only_confirmed = tolower(only_confirmed),
                       only_unconfirmed = tolower(only_unconfirmed),
                       only_to = tolower(only_to),
                       only_from = tolower(only_from),
                       min_timestamp = min_timestamp,
                       max_timestamp = max_timestamp,
                       contract_address = contract_address,
                       search_internal = tolower(FALSE),
                       limit = 200L)

  url <- tronr::build_get_request(base_url = "https://api.trongrid.io",
                                  path = c("v1", "accounts",
                                           address, "transactions", "trc20"),
                                  query_parameters = query_params)

  data <- tronr::run_paginated_query(url = url, max_attempts = max_attempts)

  if (is.null(data)) {return(data)}

  result <- dplyr::bind_rows(lapply(data, tronr::parse_trc20_tx_info))
  result <- dplyr::bind_cols(address = address, result)

  return(result)

}
