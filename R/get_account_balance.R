#' Get TRX and TRC-20 balances
#'
#' Returns the current TRX and TRC-20 token balances of an account
#'
#' @eval function_params(c("address", "only_confirmed",
#'                         "detailed_info", "max_attempts"))
#'
#' @return A tibble with the following columns:
#' * `request_time` (POSIXct, UTC timezone) - date and time when the API
#'     request was made;
#' * `address` (character) - the account address (in `hex` format);
#' * `trx_balance` (character);
#' * `n_trc20` (integer) - number of unique TRC-20 tokens currently held by the
#'     account;
#' * `trc20_balance` (list) - contains a tibble with `n_trc20` rows and two
#'     columns: `trc20` (`base58`-formatted address of the token) and `balance`
#'     (a character value, amount of the respective TRC-20 token).
#' * `n_trc10` (integer) - number of unique TRC-10 tokens currently held by the
#'      account;
#' * `trc10_balance` (list) - contains a tibble with `n_trc10` rows and several
#'     columns describing the TRC-10 assets held by the account. The number of
#'     these columns depends on the value of the `detailed_info` argument
#'     (see above).
#'
#' @details This function returns all token balances held by the account. For
#'     balances of individual tokens see `get_account_trx_balace()`,
#'     `get_account_trc20_balance()` and `get_account_trc10_balance()`.
#'
#' Balances of TRX and TRC-20 tokens are presented with a precision of 6. This
#'     means that these balances are to be divided by 1 million
#'     (after converting to `as.numeric`) to obtain the actual values.
#'     Precisions of the TRC-10 assets can vary. Use
#'     `detailed_info = TRUE` to retrieve these precisions (see
#'     column `precision` in the tibble stored in `trc10_balance` of
#'     the object returned by this function).
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
                                only_confirmed = FALSE,
                                detailed_info = FALSE,
                                max_attempts = 3L) {
  tronr::validate_arguments(
    arg_address = address,
    arg_only_confirmed = only_confirmed,
    arg_detailed_info = detailed_info,
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

  if (is.null(data$trc20) | length(data$trc20) == 0) {
    trc20 <- NA_character_
    n_trc20 <- 0L
  } else {
    trc20 <- data$trc20 %>%
      unlist() %>%
      tibble::enframe(name = "trc20", value = "balance") %>%
      dplyr::mutate(balance = as.character(.data$balance))

    n_trc20 <- length(data$trc20)
  }

  if (is.null(data$assetV2) | length(data$assetV2) == 0) {
    trc10 <- NA_character_
    n_trc10 <- 0L
  } else {
    trc10 <- lapply(data$assetV2, function(x) {
      tronr::get_asset_by_id(
        asset_id = x$key,
        detailed_info = detailed_info
      ) %>%
        dplyr::mutate(balance = as.character(gmp::as.bigz(x$value)))
    }) %>%
      dplyr::bind_rows() %>%
      dplyr::mutate(precision = as.integer(.data$precision))

    n_trc10 <- length(data$assetV2)
  }


  result <- tibble::tibble(
    request_time = tronr::from_unix_timestamp(r$meta$at, tz = "UTC"),
    address = tronr::convert_address(data$address),
    trx_balance = ifelse(is.null(data$balance),
      NA_character_,
      as.character(data$balance)
    ),
    n_trc20 = n_trc20,
    trc20_balance = list(trc20),
    n_trc10 = n_trc10,
    trc10_balance = list(trc10)
  )

  return(result)
}
