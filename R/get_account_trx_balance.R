#' Get Tronix balance
#'
#' Returns the current Tronix (TRX) balance of an account
#'
#' @eval function_params(c("address", "only_confirmed", "max_attempts"))
#'
#' @return A tibble with the following columns:
#' * `request_time` (POSIXct, UTC timezone): date and time when the API
#'     request was made;
#' * `address` (character): address of the account, in `base58` format;
#' * `trx_balance` (double): amount of TRX.
#'
#' @details All balances are presented with a precision of 6. This means
#'     that a balance returned by this function needs to be divided by
#'     1 million to obtain the actual amount of TRX.
#'
#' @export
#'
#' @examples
#' r <- get_account_trx_balance("TQjaZ9FD473QBTdUzMLmSyoGB6Yz1CGpux")
#' print(r)
get_account_trx_balance <- function(address,
                                    only_confirmed = FALSE,
                                    max_attempts = 3L) {
  validate_arguments(
    arg_address = address,
    arg_only_confirmed = only_confirmed,
    arg_max_attempts = max_attempts
  )

  query_params <- list(only_confirmed = tolower(only_confirmed))

  url <- build_get_request(
    base_url = "https://api.trongrid.io",
    path = c("v1", "accounts", address),
    query_parameters = query_params
  )

  r <- api_request(url = url, max_attempts = max_attempts)
  data <- r$data[[1]]

  result <- tibble::tibble(
    request_time = from_unix_timestamp(r$meta$at, tz = "UTC"),
    address = convert_address(data$address),
    trx_balance = ifelse(is.null(data$balance),
      NA_real_,
      apply_decimal(as.numeric(data$balance), 6)
    )
  )

  return(result)
}
