#' Parse event attributes
#'
#' Converts a list with event attributes into a nested tibble
#'
#' @param info A list returned as a result of calling the
#'     [events-related methods](https://developers.tron.network/reference#events)
#'     of the TronGird API.
#'
#' @return A nested tibble with one row and the following columns:
#' - `tx_id` (character) - ID of the transaction that a given event is part of;
#' - `block_number` (character);
#' - `block_timestamp` (POSIXct, UTC timezone);
#' - `contract_address` (character) adress of the contract that performed the
#' transaction of interest;
#' `event_name` (character) - possible values of this column will depend on
#' the nature of the transaction of interest;
#' `event_data` (list) - each element of this list contains another list with
#' raw attributes of the event. As the attributes vary wildly
#' among contracts and events, these attributes are stored as is, i.e. without
#' any additional processing. Users are kindly advised to develop their own
#' logic to process the attributes of specific events.
#'
#' @export
#'
#' @examples address <- "TKttnV3FSY1iEoAwB4N52WK2DxdV94KpSd"
#' min_timestamp <- "1576317786000"
#' max_timestamp <- "1576317996000"
#' query_params <- list(min_block_timestamp = min_timestamp,
#'                      max_block_timestamp = max_timestamp)
#' url <- tronr::build_get_request(base_url = "https://api.trongrid.io",
#'                                 path = c("v1", "contracts",
#'                                 address, "events"),
#'                                 query_parameters = query_params)
#' r <- tronr::api_request(url = url, max_attempts = 3L)
#' parse_events_info(r$data[[1]])
#'
parse_events_info <- function(info) {

  if (!is.list(info)) {
    rlang::abort("`info` must be a list")
  }

  result <- tibble::tibble(
    tx_id = info$transaction_id,
    block_number = as.character(gmp::as.bigz(info$block_number)),
    block_timestamp = tronr::from_unix_timestamp(info$block_timestamp),
    contract_address = info$contract_address,
    event_name = info$event_name,
    event_data = list(info$result)
  )

  return(result)

}
