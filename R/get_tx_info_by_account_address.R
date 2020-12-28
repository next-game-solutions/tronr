#' Get transactions for an account
#'
#' Returns a list of transactions associated with an account
#'
#' @eval function_params(c("address", "only_confirmed", "only_unconfirmed",
#'                         "only_to", "only_from",
#'                         "min_timestamp", "max_timestamp",
#'                         "max_attempts"))
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
#' - `address` (character): same as the argument `address`;
#' - `tx_id` (character): transation ID;
#' - `tx_type` (character): transation type (e.g., `Transfer`);
#' - `tx_result` (character): transaction results (e.g. `SUCCESS`);
#' - `net_usage` (character): amount of bandwidth used;
#' - `net_fee` (character): bandwidth fee;
#' - `energy_usage` (character): amount of energy used;
#' - `energy_fee` (character): energy fee;
#' - `block_number` (character);
#' - `block_timestamp` (POSIXct, UTC timezone);
#' - `raw_data` (tibble): a tibble with additional transaction attributes. The
#' exact content of this tibble is contract- and transaction-specific, but
#' typically it contains the following columns:
#'   - `tx_timestamp` (POSIXct, UTC timezone): transaction date and time;
#'   - `amount` (character): the transferred amount of TRC-20;
#'   - `from_address` (character, in `base58check` format): address that
#'   initiated the transaction;
#'   - `to_address` (character, in `base58check` format): the receiving address.
#'
#' If no TRC-20 transactions are found for the specified combination of query
#' parameters, nothing (`NULL`) is returned, with a console message
#' `"No data found"`.
#'
#' @export
#'
#' @examples
#' tx_df <- get_tx_info_by_account_address(
#'   address = "TAUN6FwrnwwmaEqYcckffC7wYmbaS6cBiX",
#'   only_confirmed = TRUE,
#'   only_from = TRUE,
#'   min_timestamp = "1577836800000",
#'   max_timestamp = "1577838600000"
#' )
#' print(tx_df)
get_tx_info_by_account_address <- function(address,
                                           only_confirmed = NULL,
                                           only_unconfirmed = NULL,
                                           only_to = FALSE,
                                           only_from = FALSE,
                                           min_timestamp = 0,
                                           max_timestamp = NULL,
                                           max_attempts = 3L) {
  tronr::validate_arguments(
    arg_address = address,
    arg_only_confirmed = only_confirmed,
    arg_only_unconfirmed = only_unconfirmed,
    arg_only_to = only_to,
    arg_only_from = only_from,
    arg_min_timestamp = min_timestamp,
    arg_max_timestamp = max_timestamp,
    arg_max_attempts = max_attempts
  )

  query_params <- list(
    only_confirmed = tolower(only_confirmed),
    only_unconfirmed = tolower(only_unconfirmed),
    only_to = tolower(only_to),
    only_from = tolower(only_from),
    min_timestamp = min_timestamp,
    max_timestamp = max_timestamp,
    search_internal = tolower(FALSE),
    limit = 200L
  )

  url <- tronr::build_get_request(
    base_url = "https://api.trongrid.io",
    path = c(
      "v1", "accounts",
      address, "transactions"
    ),
    query_parameters = query_params
  )

  data <- tronr::run_paginated_query(url = url, max_attempts = max_attempts)

  if (is.null(data)) {
    return(data)
  }

  result <- dplyr::bind_rows(lapply(data, tronr::parse_tx_info))
  result <- dplyr::bind_cols(address = address, result)

  return(result)
}
