#' Get transactions for an account
#'
#' Returns various bits of information about the transactions of a given account
#'
#' @eval function_params(c("address", "only_confirmed", "only_unconfirmed",
#'                         "only_to", "only_from",
#'                         "min_timestamp", "max_timestamp",
#'                         "max_attempts"))
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
#' @return A nested tibble where each row corresponds to one transaction.
#'     This tibble contains the following columns:
#'
#' - `address` (character): same as the argument `address`, in `base58` format;
#' - `tx_id` (character): transation ID;
#' - `tx_type` (character): transation type (see [here](https://tronscan-org.medium.com/tronscan-class-transaction-b6b3ea681e43)
#' and [here](https://tronscan-org.medium.com/tronscan-class-transaction-b6b3ea681e43)
#' for a list of all possible values and further details);
#' - `tx_result` (character): transation status (e.g., `SUCCESS`);
#' - `net_usage` (character);
#' - `net_fee` (character);
#' - `energy_usage` (character);
#' - `energy_fee` (character);
#' - `block_number` (character);
#' - `block_timestamp` (POSIXct, UTC timezone);
#' - `raw_data` (list): each element of this list contains a tibble with
#' additional transaction attributes (the actual structure of a given tibble
#' will depend on `tx_type`, but among other things it will typically
#' contain `from_address`, `to_address` and transaction `timestamp`);
#'
#' If no transaction are found for the specified combinations of query
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
