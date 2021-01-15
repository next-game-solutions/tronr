#' Get account balance
#'
#' Retrieves the current token and bandwidth balances of an account
#'
#' @eval function_params(c("address", "max_attempts"))
#'
#' @return A nested tibble with the following columns:
#' * `request_time` (POSIXct, UTC timezone): date and time of the request;
#' * `address` (character): account address (in `base58check` format);
#' * `name` (character): name of the account (`NA` if absent);
#' * `total_transaction_count`: total number of transactions recorded for
#'     the account as of `request_time`;
#' * `bandwidth` (list): contains a tibble with several variables describing
#'     the energy and bandwidth usage of the account;
#' * `trx_balance` (double): currently available (i.e. not-frozeb) balance of
#'     Tronix (TRX);
#' * `n_trc20` (integer): number of unique TRC-20 tokens currently held by the
#'     account (`0` if absent);
#' * `trc20` (list or `NA` if absent): contains a tibble with `n_trc20` rows
#'     and several variables describing the TRC-20 tokens held by the account;
#' * `n_trc10` (integer): number of unique TRC-10 tokens currently held by the
#'      account (`0` if absent);
#' * `trc10` (list or `NA` if absent): contains a tibble with `n_trc10` rows
#'     and several columns describing the TRC-10 assets held by the account.
#'
#' @seealso [get_account_trx_balance()], [get_account_trc20_balance()] and
#'     [get_account_trc10_balance()].
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' r <- get_account_balance("TQjaZ9FD473QBTdUzMLmSyoGB6Yz1CGpux")
#' print(r)
get_account_balance <- function(address,
                                max_attempts = 3L) {
  validate_arguments(
    arg_address = address,
    arg_max_attempts = max_attempts
  )

  if (substr(address, 1, 2) == 41 | substr(address, 1, 2) == "0x") {
    address <- convert_address(address)
  }

  query_params <- list(address = address)

  url <- build_get_request(
    base_url = "https://apilist.tronscan.org/",
    path = c("api", "account"),
    query_parameters = query_params
  )

  request_time <- Sys.time()
  attr(request_time, "tzone") <- "UTC"

  r <- api_request(url = url, max_attempts = max_attempts)
  r$request_time <- request_time

  result <- dplyr::bind_cols(
    tibble::tibble(request_time = request_time, address = address),
    parse_account_balance_info(info = r)
  )

  return(result)
}
