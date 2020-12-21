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
#' `"No events found"`.
#'
#' @details The exact content of `event_data` in the returned result will
#' be contract- and event-specific. Thus, very little processing is done with
#' these data, except for removing redundant attributes and converting all
#' addresses to `base58` format.
#' @export
#'
#' @examples get_events_by_block_number(block_number = "15354550")
#'
get_events_by_block_number <- function(block_number,
                                       only_confirmed = NULL,
                                       limit = 200L,
                                       max_attempts = 3L) {

  if (!is.character(block_number)) {
    rlang::abort("`block_number` must be character value")
  }

  if (!(is.logical(only_confirmed) | is.null(only_confirmed))) {
    rlang::abort("`only_confirmed` must be either boolean or NULL")
  }

  if (!(is.integer(limit) & limit > 0)) {
    rlang::abort("`limit` must be a positive integer")
  }

  if (!(is.integer(max_attempts) & max_attempts > 0)) {
    rlang::abort("`max_attempts` must be a positive integer")
  }


  query_params <- list(only_confirmed = tolower(only_confirmed),
                       limit = limit)

  url <- tronr::build_get_request(base_url = "https://api.trongrid.io",
                                  path = c("v1", "blocks",
                                           block_number, "events"),
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