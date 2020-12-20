#' Get events by transaction ID
#'
#' Returns events associated with a transaction
#'
#' @param tx_id (character) - transaction ID.
#' @param only_confirmed (boolean or `NULL`) - if `NULL` (default) or `FALSE`,
#'     the result is returned irrespective of whether `tx_id` is confirmed.
#'     If `TRUE`, only results for confirmed transactions are returned.
#'     Cannot be used simultanously with the `only_unconfirmed` argument.
#' @param only_unconfirmed (boolean or `NULL`) - if `NULL` (default) or `FALSE`,
#'     the result is returned irrespective of whether `tx_id` is confirmed.
#'     If `TRUE`, only unconfirmed transactions are returned. Cannot be used
#'     simultanously with the `only_confirmed` argument.
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
#' - `caller_contract_address` (character) address (in `base58` format) of the
#' contract that initiated the transaction;
#' - `contract_address` (character) adress of the contract that implemented the
#' transaction of interest;
#' `event_name` (character) - possible values of this column will depend on
#' the nature of the transaction of interest;
#' `event_data` (list) - each element of this list contains a tibble with
#' additional attributes of the event.
#'
#' If no events are found for the specified combinations of query
#' parameters, nothing (`NULL`) is returned, with a console message
#' `"No events found for this transaction"`.
#'
#' @details The exact content of `event_data` in the returned result will
#' be contract- and event-specific. Thus, very little processing is done with
#' these data, except for removing redundant attributes and converting all
#' addresses to `base58` format.
#'
#' @export
#'
#' @examples tx_id <- "270e992bf22a271008032ec09a51616ed963b74f4d808b0fd74fc82341a81391"
#' r <- get_events_by_tx_id(tx_id)
#' print(r)
#'
get_events_by_tx_id <- function(tx_id,
                                only_confirmed = NULL,
                                only_unconfirmed = NULL,
                                max_attempts = 3L) {

  if (!is.character(tx_id)) {
    rlang::abort("`tx_id` must be a character value")
  }

  if (!(is.logical(only_confirmed) | is.null(only_confirmed))) {
    rlang::abort("`only_confirmed` must be either boolean or NULL")
  }

  if (!(is.logical(only_unconfirmed) | is.null(only_unconfirmed))) {
    rlang::abort("`only_unconfirmed` must be either boolean or NULL")
  }

  if (!(is.integer(max_attempts) & max_attempts > 0)) {
    rlang::abort("`max_attempts` must be a positive integer")
  }

  if (is.logical(only_confirmed) & is.logical(only_unconfirmed)) {
    rlang::abort("`only_confirmed` and `only_unconfirmed` cannot be used simultaneously")
  }

  query_params <- list(only_confirmed = tolower(only_confirmed),
                       only_unconfirmed = tolower(only_unconfirmed))

  url <- tronr::build_get_request(base_url = "https://api.trongrid.io",
                                  path = c("v1", "transactions",
                                           tx_id, "events"),
                                  query_parameters = query_params)

  r <- tronr::api_request(url = url, max_attempts = max_attempts)

  if (length(r$data) == 0L) {
    message("No events found for this transaction")
    return(NULL)
  }

  result <- lapply(r$data, function(x){

    x$result <- lapply(x$result, function(y){
      if (substr(y, 1, 2) == "0x" &
          tronr::is_address(paste0("41", substr(y, 3, nchar(y))))) {
        return(tronr::convert_address(y))
      } else {return(y)}
    })

    x$result <- lapply(x$result, function(y){
      if (substr(y, 1, 2) == "41" & tronr::is_address(y)) {
        return(tronr::convert_address(y))
      } else {return(y)}
    })

    res <- tibble::as_tibble(x$result)

    res <- res[, nchar(names(res)) > 1]

    tibble::tibble(
      tx_id = x$transaction_id,
      block_number = as.character(gmp::as.bigz(x$block_number)),
      block_timestamp = tronr::from_unix_timestamp(x$block_timestamp),
      caller_contract_address = x$caller_contract_address,
      contract_address = x$contract_address,
      event_name = x$event_name,
      event_data = list(res)
    )

  })

  return(dplyr::bind_rows(result))

}
