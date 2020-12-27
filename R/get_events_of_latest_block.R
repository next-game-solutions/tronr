#' Get events of latest block
#'
#' Retrieves events that took place in the most recent block
#'
#' @eval function_params(c("only_confirmed", "max_attempts"))
#'
#' @return A nested tibble where each row corresponds to an event associated
#'     with the transaction of interest. This tibble contains the following
#'     columns:
#' - `tx_id` (character): same as the argument `tx_id`;
#' - `block_number` (character);
#' - `block_timestamp` (POSIXct, UTC timezone);
#' - `contract_address` (character) adress of the contract that performed the
#' transaction of interest;
#' `event_name` (character): possible values of this column will depend on
#' the nature of the transaction of interest;
#' `event_data` (list): each element of this list contains a tibble with
#' additional attributes of the event.
#'
#' If no events are found for the specified combinations of query
#' parameters, nothing (`NULL`) is returned, with a console message
#' `"No events found for this transaction"`.
#'
#' @details The exact content of `event_data` in the returned result will
#' be contract- and event-specific. Thus, very little processing is done with
#' these data, except for removing redundant attributes and converting all
#' addresses to `base58check` format.
#'
#' @export
#'
#' @examples
#' r <- get_events_of_latest_block()
#' print(r)
get_events_of_latest_block <- function(only_confirmed = FALSE,
                                       max_attempts = 3L) {
  tronr::validate_arguments(
    arg_only_confirmed = only_confirmed,
    arg_max_attempts = max_attempts
  )

  query_params <- list(only_confirmed = tolower(only_confirmed))

  url <- tronr::build_get_request(
    base_url = "https://api.trongrid.io",
    path = c(
      "v1", "blocks",
      "latest", "events"
    ),
    query_parameters = query_params
  )

  data <- tronr::run_paginated_query(url = url, max_attempts = max_attempts)

  if (is.null(data)) {
    return(data)
  }

  result <- dplyr::bind_rows(lapply(data, tronr::parse_events_info))
  return(result)
}
