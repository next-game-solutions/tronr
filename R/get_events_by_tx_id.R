#' Get events by transaction ID
#'
#' Retrieves events associated with a transaction
#'
#' @eval function_params(c("tx_id", "only_confirmed", "only_unconfirmed",
#'                         "max_attempts"))
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
#' tx_id <- "270e992bf22a271008032ec09a51616ed963b74f4d808b0fd74fc82341a81391"
#' r <- get_events_by_tx_id(tx_id)
#' print(r)
get_events_by_tx_id <- function(tx_id,
                                only_confirmed = NULL,
                                only_unconfirmed = NULL,
                                max_attempts = 3L) {
  tronr::validate_arguments(
    arg_tx_id = tx_id,
    arg_only_confirmed = only_confirmed,
    arg_only_unconfirmed = only_unconfirmed,
    arg_max_attempts = max_attempts
  )

  query_params <- list(
    only_confirmed = tolower(only_confirmed),
    only_unconfirmed = tolower(only_unconfirmed)
  )

  url <- tronr::build_get_request(
    base_url = "https://api.trongrid.io",
    path = c(
      "v1", "transactions",
      tx_id, "events"
    ),
    query_parameters = query_params
  )

  r <- tronr::api_request(url = url, max_attempts = max_attempts)

  if (length(r$data) == 0L) {
    message("No data found")
    return(NULL)
  }

  result <- dplyr::bind_rows(lapply(r$data, tronr::parse_events_info))
  return(result)
}
