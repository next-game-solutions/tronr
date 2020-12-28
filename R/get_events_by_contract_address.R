#' Get events by contract address
#'
#' Retrieves events associated with a smart contract
#'
#' @eval function_params(c("address", "event_name", "block_number",
#'                         "only_confirmed", "only_unconfirmed",
#'                         "min_timestamp", "max_timestamp",
#'                         "direction", "max_attempts"))
#'
#' @return A nested tibble where each row corresponds to an event associated
#'     with the account of interest. This tibble contains the following
#'     columns:
#' - `tx_id` (character): transaction ID;
#' - `block_number` (character);
#' - `block_timestamp` (POSIXct, UTC timezone);
#' - `contract_address` (character): adress of the smart contract that implemented
#' the event;
#' - `event_name` (character): possible values of this column are contract-
#' and event-specific;
#' - `event_data` (list): each element of this list contains a named list with
#' additional attributes of the event.
#'
#' If no events are found for the specified combinations of query
#' parameters, nothing (`NULL`) is returned, with a console message
#' `"No data found"`.
#'
#' @details The exact content of `event_data` in the tibble returned by this
#'     function will be contract- and event-specific. Thus, these data are
#'     returned in the form of a named R list as-is, i.e. without any
#'     processing. Users are advised to develop their own, task-specific, logic
#'     to process event attributes.
#'
#' @export
#'
#' @examples
#' address <- "TKttnV3FSY1iEoAwB4N52WK2DxdV94KpSd"
#' min_timestamp <- "1576317786000"
#' max_timestamp <- "1576317996000"
#' get_events_by_contract_address(
#'   address = address,
#'   min_timestamp = min_timestamp,
#'   max_timestamp = max_timestamp
#' )
#' get_events_by_contract_address(
#'   address = address,
#'   min_timestamp = min_timestamp,
#'   max_timestamp = max_timestamp,
#'   event_name = "Transfer",
#'   direction = "asc"
#' )
get_events_by_contract_address <- function(address,
                                           event_name = NULL,
                                           block_number = NULL,
                                           only_confirmed = NULL,
                                           only_unconfirmed = NULL,
                                           min_timestamp = 0,
                                           max_timestamp,
                                           direction = "desc",
                                           max_attempts = 3L) {
  tronr::validate_arguments(
    arg_address = address,
    arg_event_name = event_name,
    arg_block_number = block_number,
    arg_only_confirmed = only_confirmed,
    arg_only_unconfirmed = only_unconfirmed,
    arg_min_timestamp = min_timestamp,
    arg_max_timestamp = max_timestamp,
    arg_direction = direction,
    arg_max_attempts = max_attempts
  )

  query_params <- list(
    event_name = event_name,
    block_number = block_number,
    only_confirmed = tolower(only_confirmed),
    only_unconfirmed = tolower(only_unconfirmed),
    min_block_timestamp = min_timestamp,
    max_block_timestamp = max_timestamp,
    order_by = paste("block_timestamp", direction, sep = ","),
    limit = 200L
  )

  url <- tronr::build_get_request(
    base_url = "https://api.trongrid.io",
    path = c(
      "v1", "contracts",
      address, "events"
    ),
    query_parameters = query_params
  )

  data <- tronr::run_paginated_query(url = url, max_attempts = max_attempts)

  if (is.null(data)) {
    return(data)
  }

  result <- dplyr::bind_rows(lapply(data, tronr::parse_events_info))
  result <- dplyr::bind_cols(result)

  return(result)
}
