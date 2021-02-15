#' Get events by block number
#'
#' Retrieves events that belong to a given block
#'
#' @eval function_params(c("block_number", "only_confirmed", "max_attempts"))
#'
#' @return A nested tibble where each row corresponds to an event associated
#'     with the block of interest. This tibble contains the following
#'     columns:
#' - `tx_id` (character): transaction ID;
#' - `block_number` (character): same as the argument `block_number`;
#' - `block_timestamp` (POSIXct, UTC timezone);
#' - `contract_address` (character): adress of the smart contract that triggered
#' the event;
#' - `event_name` (character): possible values of this column are contract-
#' and event-specific;
#' - `event_data` (list): each element of this list contains a named list with
#' additional attributes of the event.
#'
#' If no events are found for the specified combination of query
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
#' r <- get_events_by_block_number(block_number = "15354550")
#' print(r)
get_events_by_block_number <- function(block_number,
                                       only_confirmed = NULL,
                                       max_attempts = 3L) {
  validate_arguments(
    arg_block_number = block_number,
    arg_only_confirmed = only_confirmed,
    arg_max_attempts = max_attempts
  )

  query_params <- list(
    only_confirmed = tolower(only_confirmed),
    limit = 200L
  )

  url <- build_get_request(
    base_url = "https://api.trongrid.io",
    path = c(
      "v1", "blocks",
      block_number, "events"
    ),
    query_parameters = query_params
  )

  data <- run_paginated_query(url = url, max_attempts = max_attempts)

  if (is.null(data)) {
    message("No data found")
    return(data)
  }

  result <- dplyr::bind_rows(lapply(data, parse_events_info))

  return(result)
}
