#' Get TRC-20 token balances
#'
#' Returns the current TRC-20 token balances of an account
#'
#' @eval function_params(c("address", "only_confirmed", "max_attempts"))
#'
#' @return A nested tibble with the following columns:
#' * `request_time` (POSIXct, UTC timezone): date and time when the API
#'     request was made;
#' * `address` (character): the account address, in `base58` format;
#' * `n_trc20` (integer): number of TRC-20 tokens held by `account`;
#' * `trc20_balance` (named list): contains a tibble with `n_trc20` rows and two
#'     columns: `trc20` (`base58check`-formatted address of the token) and
#'     `balance` (a character value, the token's amount).
#'
#' @details TRC-20 is a technical standard used for smart contracts on the
#'     TRON blockchain for implementing tokens with the TRON Virtual Machine
#'     (TVM). If an account holds no TRC-20 tokens (`n_trc20 = 0`),
#'     the `trc20_balance` column in the tibble returned by this function will
#'     contain an `NA` value.
#'
#' All balances are presented with a precision of 6. This means
#'     that a balance returned by this function needs to be divided by
#'     1 million (after converting to `as.numeric`) to get the actual value.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' r <- get_account_trc20_balance("TQjaZ9FD473QBTdUzMLmSyoGB6Yz1CGpux")
#' print(r)
get_account_trc20_balance <- function(address,
                                      only_confirmed = FALSE,
                                      max_attempts = 3L) {
  tronr::validate_arguments(
    arg_address = address,
    arg_only_confirmed = only_confirmed,
    arg_max_attempts = max_attempts
  )

  query_params <- list(only_confirmed = tolower(only_confirmed))

  url <- tronr::build_get_request(
    base_url = "https://api.trongrid.io",
    path = c("v1", "accounts", address),
    query_parameters = query_params
  )

  r <- tronr::api_request(url = url, max_attempts = max_attempts)
  data <- r$data[[1]]

  if (is.null(data$trc20) | length(data$trc20) == 0L) {
    trc20 <- NA_character_
    n_trc20 <- 0L
  } else {
    trc20 <- data$trc20 %>%
      unlist() %>%
      tibble::enframe(name = "trc20", value = "balance") %>%
      dplyr::mutate(balance = as.character(.data$balance))
    n_trc20 <- length(data$trc20)
  }

  result <- tibble::tibble(
    request_time = tronr::from_unix_timestamp(r$meta$at, tz = "UTC"),
    address = tronr::convert_address(data$address),
    n_trc20 = n_trc20,
    trc20_balance = list(trc20)
  )

  return(result)
}
