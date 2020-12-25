#' Get events by contract address
#'
#' Retrieves events associated with a smart contract
#'
#' @param address (character) - address of the smart contract of interest, in
#'     `base58` (starts with `T`) or `hex` (starts with `41`) format.
#' @param event_name (character) - name of the specific event of interest
#'     (e.g., `Transfer`). Defaults to `NULL`.
#' @param block_number (character) - block number to look within. Defaults to
#'     `NULL`.
#' @param only_confirmed (boolean or `NULL`) - if `NULL` (default) or `FALSE`,
#'     the result is returned irrespective of whether the event's
#'     parent transaction is confirmed. If `TRUE`, only results for confirmed
#'     transactions are returned. Cannot be used simultanously with
#'     the `only_unconfirmed` argument.
#' @param only_unconfirmed (boolean or `NULL`) - if `NULL` (default) or `FALSE`,
#'     the result is returned irrespective of whether the respective event's
#'     parent transaction is confirmed. If `TRUE`, only unconfirmed transactions
#'     are returned. Cannot be used simultanously with the `only_confirmed`
#'     argument.
#' @param min_timestamp (numeric or character) - a Unix timestamp
#'     (_including milliseconds_), which defines the beginning of the
#'     period to retrieve the events from. Defaults to 0.
#' @param max_timestamp (numeric or character) - a Unix timestamp
#'     (_including milliseconds_), which defines the end of the
#'     period to retrieve the events from.
#' @param direction (character) - specifies the direction of temporal ordering
#'     of the results - descending (`desc`) or ascending (`asc`).
#' @param limit (integer) - number of transactions per page. Defaults to 200.
#'     Maximum accepted value is 200 (higher values will be ignored).
#' @param max_attempts (integer, positive) - specifies the maximum
#'     number of additional attempts to call the API if the first attempt fails
#'     (i.e. its call status is different from `200`). Additional attempts are
#'     implemented with an exponential backoff. Defaults to 3.
#'
#' @return A nested tibble where each row corresponds to an event associated
#'     with the transaction of interest. This tibble contains the following
#'     columns:
#' - `tx_id` (character) - same as the argument `tx_id`;
#' - `block_number` (character);
#' - `block_timestamp` (POSIXct, UTC timezone);
#' - `contract_address` (character) adress of the contract that performed the
#' transaction of interest;
#' `event_name` (character) - possible values of this column will depend on
#' the nature of the transaction of interest;
#' `event_data` (list) - each element of this list contains a tibble with
#' additional attributes of the event.
#'
#' If no events are found for the specified combinations of query
#' parameters, nothing (`NULL`) is returned, with a console message
#' `"No data found"`.
#'
#' @details The exact content of `event_data` in the returned result will
#' be contract- and event-specific. Thus, very little processing is done with
#' these data, except for removing redundant attributes and converting all
#' addresses to `base58` format.
#' @export
#'
#' @examples address <- "TKttnV3FSY1iEoAwB4N52WK2DxdV94KpSd"
#' min_timestamp <- "1576317786000"
#' max_timestamp <- "1576317996000"
#' get_events_by_contract_address(address = address,
#'                                min_timestamp = min_timestamp,
#'                                max_timestamp = max_timestamp)
#' get_events_by_contract_address(address = address,
#'                                min_timestamp = min_timestamp,
#'                                max_timestamp = max_timestamp,
#'                                event_name = "Transfer",
#'                                direction = "asc")
#'
#'
get_events_by_contract_address <- function(address,
                                           event_name = NULL,
                                           block_number = NULL,
                                           only_confirmed = NULL,
                                           only_unconfirmed = NULL,
                                           min_timestamp = 0,
                                           max_timestamp,
                                           direction = "desc",
                                           limit = 100L,
                                           max_attempts = 3L) {

  tronr::validate_arguments(arg_address = address,
                            arg_event_name = event_name,
                            arg_block_number = block_number,
                            arg_only_confirmed = only_confirmed,
                            arg_only_unconfirmed = only_unconfirmed,
                            arg_min_timestamp = min_timestamp,
                            arg_max_timestamp = max_timestamp,
                            arg_direction = direction,
                            arg_limit = limit,
                            arg_max_attempts = max_attempts)

  query_params <- list(event_name = event_name,
                       block_number = block_number,
                       only_confirmed = tolower(only_confirmed),
                       only_unconfirmed = tolower(only_unconfirmed),
                       min_block_timestamp = min_timestamp,
                       max_block_timestamp = max_timestamp,
                       order_by = paste("block_timestamp", direction, sep = ","),
                       limit = limit)

  url <- tronr::build_get_request(base_url = "https://api.trongrid.io",
                                  path = c("v1", "contracts",
                                           address, "events"),
                                  query_parameters = query_params)

  data <- tronr::run_paginated_query(url = url, max_attempts = max_attempts)

  if (is.null(data)) {return(data)}

  result <- dplyr::bind_rows(lapply(data, tronr::parse_events_info))
  result <- dplyr::bind_cols(result)

  return(result)

}
