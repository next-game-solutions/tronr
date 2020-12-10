#' Get TRX balance
#'
#' Returns the current TRX balance of an account
#'
#' @param address (character) - address of the account of interest, in
#'     `base58` (starts with `T`) or  `hex` (starts with `41`) format.
#' @param only_confirmed (boolean) - if `TRUE`, account balance will be
#'     returned as of the latest confirmed block, otherwise as of the
#'     latest unconfirmed one. Defaults to `FALSE`.
#' @param max_attempts (integer, positive) - a non-zero integer, maximum
#'     number of additional attempts to call the API if the first attempt fails
#'     (i.e. its call status is different from `200`). Additional attempts are
#'     implemented with an exponential backoff. Defaults to 3.
#'
#' @return A tibble with the following columns:
#' * `request_time` (POSIXct, UTC timezone) - date and time when the API
#'     request was made;
#' * `address` (character) the account address (in `hex` format);
#' * `trx_balance` (character).
#'
#' @details All balances are presented with a precision of 6. This means
#'     that a balance returned by this function needs to be divided by
#'     1 million (after converting with `as.numeric()`) to obtain the actual
#'     amount of TRX.
#'
#' @export
#'
#' @examples
#' r <- get_account_trx_balance("TQjaZ9FD473QBTdUzMLmSyoGB6Yz1CGpux")
#' print(r)
#'
get_account_trx_balance <- function(address,
                                    only_confirmed = FALSE,
                                    max_attempts = 3L) {

  if (!tronr::is_address(address)) {
    rlang::abort("Provided address is not valid")
  }

  if (!is.logical(only_confirmed)) {
    rlang::abort("`only_confirmed` must be boolean")
  }

  if (!(is.integer(max_attempts) & max_attempts > 0)) {
    rlang::abort("`max_attempts` must be a positive integer")
  }


  query_params <- list(only_confirmed = tolower(only_confirmed))

  url <- tronr::build_get_request(base_url = "https://api.trongrid.io",
                                  path = c("v1", "accounts", address),
                                  query_parameters = query_params)

  r <- tronr::api_request(url = url, max_attempts = max_attempts)
  data <- r$data[[1]]

  result <- tibble::tibble(
    request_time = tronr::from_unix_timestamp(r$meta$at, tz = "UTC"),
    address = data$address,
    trx_balance = ifelse(is.null(data$balance),
                         NA_character_,
                         as.character(data$balance))
  )

  return(result)

}
