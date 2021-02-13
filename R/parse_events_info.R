#' Parse event attributes
#'
#' Converts a list with event attributes into a nested tibble
#'
#' @param info A non-empty, named list returned as a result of calling
#'     events-related methods (see [get_events_by_block_number()],
#'     [get_events_by_contract_address()], etc.).
#'
#' @return A nested tibble where each row corresponds to an event associated
#'     with the account of interest. This tibble contains the following
#'     columns:
#' - `tx_id` (character): transaction ID;
#' - `block_number` (character);
#' - `timestamp` (POSIXct, UTC timezone): block timestamp;
#' - `contract_address` (character): adress of the smart contract that triggered
#' the event;
#' - `event_name` (character): possible values of this column are contract-
#' and event-specific;
#' - `event_data` (list): each element of this list contains a named list with
#' additional attributes of the event.
#'
#' @keywords internal
#'
parse_events_info <- function(info) {
  if (!is.list(info)) {
    rlang::abort("`info` must be a list")
  }

  if (length(info) == 0 | is.null(names(info))) {
    rlang::abort("`info` must be a non-empty named list")
  }

  result <- tibble::tibble(
    tx_id = info$transaction_id,
    block_number = as.character(info$block_number),
    timestamp = from_unix_timestamp(info$block_timestamp),
    contract_address = info$contract_address,
    event_name = info$event_name,
    event_data = list(info$result)
  )

  return(result)
}
