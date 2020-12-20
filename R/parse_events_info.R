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
#' `event_data` (list) - each element of this list contains a tibble with
#' additional attributes of the event. The exact content of this tibble will
#' be contract- and event-specific. Thus, very little processing is done with
#' these data, except for removing redundant attributes and converting all
#' addresses to `base58` format.
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
#' r <- tronr::api_request(url = url, max_attempts = max_attempts)
#' parse_events_info(r$data[[1]])
#'
parse_events_info <- function(info) {

  if (!is.list(info)) {
    rlang::abort("`info` must be a list")
  }

  info$result <- lapply(info$result, function(y){
    if (substr(y, 1, 2) == "0x" &
        tronr::is_address(paste0("41", substr(y, 3, nchar(y))))) {
      return(tronr::convert_address(y))
    } else {return(y)}
  })

  info$result <- lapply(info$result, function(y){
    if (substr(y, 1, 2) == "41" & tronr::is_address(y)) {
      return(tronr::convert_address(y))
    } else {return(y)}
  })

  res <- tibble::as_tibble(info$result)

  res <- res[, nchar(names(res)) > 1]

  result <- tibble::tibble(
    tx_id = info$transaction_id,
    block_number = as.character(gmp::as.bigz(info$block_number)),
    block_timestamp = tronr::from_unix_timestamp(info$block_timestamp),
    contract_address = info$contract_address,
    event_name = info$event_name,
    event_data = list(res)
  )

  return(result)

}
