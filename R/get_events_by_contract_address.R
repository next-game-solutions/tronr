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
#' `"No events found"`.
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

  if (!tronr::is_address(address)) {
    rlang::abort("Provided address is not valid")
  }

  if (!(is.character(event_name) | is.null(event_name))) {
    rlang::abort("`event_name` must be a character value")
  }

  if (!(is.character(block_number) | is.null(block_number))) {
    rlang::abort("`block_number` must be a character value")
  }

  if (!(is.logical(only_confirmed) | is.null(only_confirmed))) {
    rlang::abort("`only_confirmed` must be either boolean or NULL")
  }

  if (!(is.logical(only_unconfirmed) | is.null(only_unconfirmed))) {
    rlang::abort("`only_unconfirmed` must be either boolean or NULL")
  }

  if (is.logical(only_confirmed) & is.logical(only_unconfirmed)) {
    rlang::abort("`only_confirmed` and `only_unconfirmed` cannot be used simultaneously")
  }

  if (!(is.character(min_timestamp) |
        is.numeric(min_timestamp) |
        is.null(min_timestamp)) ) {
    rlang::abort("`min_timestamp` must be either numeric or character or NULL")
  }

  if (!is.null(min_timestamp)) {
    min_dt <- suppressWarnings(as.numeric(min_timestamp) / 1000)
    if (is.na(min_dt)) {
      rlang::abort("`min_timestamp` cannot be coerced to a POSIXct value")
    }
  }

  if (!(is.character(max_timestamp) |
        is.numeric(max_timestamp) |
        is.null(max_timestamp))) {
    rlang::abort("`max_timestamp` must be either numeric or character or NULL")
  }

  if (!is.null(max_timestamp)) {
    max_dt <- suppressWarnings(as.numeric(max_timestamp) / 1000)
    if (is.na(max_dt)) {
      rlang::abort("`max_timestamp` cannot be coerced to a POSIXct value")
    }
  }

  if (!is.character(direction)) {
    rlang::abort("`direction` must be a character value")
  }

  if (!direction %in% c("asc", "desc")) {
    rlang::abort(c("`order_by` must be one of:", c("asc", "desc")))
  }

  if (!(is.integer(limit) & limit > 0)) {
    rlang::abort("`limit` must be a positive integer")
  }

  if (!(is.integer(max_attempts) & max_attempts > 0)) {
    rlang::abort("`max_attempts` must be a positive integer")
  }


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

  data <- list()
  p <- 1
  while (TRUE) {
    message("Reading page ", p, "...")
    r <- tronr::api_request(url = url, max_attempts = max_attempts)
    data <- c(data, r$data)
    if (is.null(r$meta$fingerprint)) {break}
    p <- p + 1
    url <- r$meta$links$`next`
  }

  if (length(data) == 0L) {
    message("No events found")
    return(NULL)
  }

  result <- dplyr::bind_rows(lapply(data, tronr::parse_events_info))
  result <- dplyr::bind_cols(result)

  return(result)

}
