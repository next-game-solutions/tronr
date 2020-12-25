#' Get events by block number
#'
#' Retrieves events that took place during a given block
#'
#' @param block_number (character) - number of the block of interest.
#' @param only_confirmed (boolean or `NULL`) - if `NULL` (default) or `FALSE`,
#'     the result is returned irrespective of whether the event's
#'     parent transaction is confirmed. If `TRUE`, only results for confirmed
#'     transactions are returned.
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
#' @examples r <- get_events_by_block_number(block_number = "15354550")
#' print(r)
#'
get_events_by_block_number <- function(block_number,
                                       only_confirmed = NULL,
                                       limit = 200L,
                                       max_attempts = 3L) {

  tronr::validate_arguments(arg_block_number = block_number,
                            arg_only_confirmed = only_confirmed,
                            arg_limit = limit,
                            arg_max_attempts = max_attempts)

  query_params <- list(only_confirmed = tolower(only_confirmed),
                       limit = limit)

  url <- tronr::build_get_request(base_url = "https://api.trongrid.io",
                                  path = c("v1", "blocks",
                                           block_number, "events"),
                                  query_parameters = query_params)

  data <- tronr::run_paginated_query(url = url, max_attempts = max_attempts)

  result <- dplyr::bind_rows(lapply(data, tronr::parse_events_info))
  result <- dplyr::bind_cols(result)

  return(result)

}
