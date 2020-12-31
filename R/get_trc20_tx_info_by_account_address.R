#' Get TRC-20 transactions for an account
#'
#' Returns a list of TRC-20 transactions associated with an address
#'
#' @eval function_params(c("address", "only_confirmed", "only_unconfirmed",
#'                         "only_to", "only_from",
#'                         "min_timestamp", "max_timestamp",
#'                         "contract_address", "max_attempts"))
#'
#' @details Some addresses have a very high load of transactions going
#'     through them. Users are, therefore, advised to choose `min_timestamp` and
#'     `max_timestamp` wisely as the TronGrid API may deny calls that
#'     attempt to retrieve a large amount of data in one go. Such requests will
#'     fail with status `404` and an error message
#'     `"Exceeds the maximum limit, please change your query time range"`).
#'     Chunking the time range of interest into smaller periods may help,
#'     however users will have to implement their own logic for this.
#'
#' @return A tibble where each row corresponds to one transaction.
#'     This tibble contains the following columns:
#'
#' - `tx_id` (character) - transation ID;
#' - `tx_type` (character) - transation type (e.g., `"Transfer"`);
#' - `block_timestamp` (POSIXct);
#' - `from_address` (character, `base58check`-formatted);
#' - `to_address` (character, `base58check`-formatted);
#' - `trc20_symbol` (character) - abbreviated name of the TRC-20 token;
#' - `trc20_name` (character) - common name of the TRC-20 token;
#' - `trc20_contract_address` (character, `base58check`-formatted);
#' - `precision` (double) - precision of the `amount` values;
#' - `amount` (double) - transfered amount.
#'
#' If no TRC-20 transactions are found for the specified combination of query
#' parameters, nothing (`NULL`) is returned, with a console message
#' `"No data found"`.
#'
#' @export
#'
#' @examples
#' tx_df <- get_trc20_tx_info_by_account_address(
#'   address = "TAUN6FwrnwwmaEqYcckffC7wYmbaS6cBiX",
#'   min_timestamp = "1604188800000",
#'   max_timestamp = "1604189100000"
#' )
#' print(tx_df)
get_trc20_tx_info_by_account_address <- function(address,
                                                 only_confirmed = NULL,
                                                 only_unconfirmed = NULL,
                                                 only_to = FALSE,
                                                 only_from = FALSE,
                                                 min_timestamp = 0,
                                                 max_timestamp = NULL,
                                                 contract_address = NULL,
                                                 max_attempts = 3L) {
  tronr::validate_arguments(
    arg_address = address,
    arg_only_confirmed = only_confirmed,
    arg_only_unconfirmed = only_unconfirmed,
    arg_only_to = only_to,
    arg_only_from = only_from,
    arg_min_timestamp = min_timestamp,
    arg_max_timestamp = max_timestamp,
    arg_contract_address = contract_address,
    arg_max_attempts = max_attempts
  )

  query_params <- list(
    only_confirmed = tolower(only_confirmed),
    only_unconfirmed = tolower(only_unconfirmed),
    only_to = tolower(only_to),
    only_from = tolower(only_from),
    min_timestamp = min_timestamp,
    max_timestamp = max_timestamp,
    contract_address = contract_address,
    search_internal = tolower(FALSE),
    limit = 200L
  )

  url <- tronr::build_get_request(
    base_url = "https://api.trongrid.io",
    path = c(
      "v1", "accounts",
      address, "transactions", "trc20"
    ),
    query_parameters = query_params
  )

  data <- tronr::run_paginated_query(url = url, max_attempts = max_attempts)

  if (is.null(data)) {
    return(data)
  }

  result <- dplyr::bind_rows(lapply(data, tronr::parse_trc20_tx_info))
  result <- dplyr::bind_cols(address = address, result)

  return(result)
}
