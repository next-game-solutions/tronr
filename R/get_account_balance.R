#' Get TRX and TRC-20 balances
#'
#' Returns the current TRX and TRC-20 token balances of an account
#'
#' @param address A character value - address of the account of interest, in
#'     `base58` (starts with `T`) or `hex` (starts with `41`) format.
#' @param only_confirmed A boolean value. If `TRUE`, account balance will be
#'     returned as of the latest confirmed block, otherwise as of the
#'     latest unconfirmed one. Defaults to `FALSE`.
#' @param detailed_trc10_info A boolean value. If `FALSE` (default), only basic
#'     information about the TRC-10 token assets will be returned. If `TRUE`,
#'     an extended information will be returned.
#' @param max_attempts A non-zero, positive integer specifying the maximum
#'     number of additional attempts to call the API if the first attempt fails
#'     (i.e. its call status is different from `200`). Additional attempts are
#'     implemented with an exponential backoff. Defaults to 3.
#'
#' @return A tibble with the following columns:
#' * `request_time`: date and time  (UTC timezone) when the API
#'     request was made;
#' * `address`: a character value indicating the account address
#'     (in `hex` format);
#' * `trx_balance`: a character value.
#' * `n_trc20`: number of unique TRC-20 tokens currently held by the account;
#' * `trc20_balance`: a list that contains a tibble with two columns:
#'     `trc20` (`base58`-formatted address of the token) and `balance`
#'     (a character value, amount of the respective TRC-20 token).
#' * `n_trc10`: number of unique TRC-10 tokens currently held by the account;
#' * `trc10_balance`: a list that contains a tibble with several columns
#'     describing the TRC-10 assets held by the account. The number of these
#'     columns depends on the value of the `detailed_trc10_info` argument
#'     (see above).
#' @details This function returns all token balances held by the account. For
#'     individual token balance see functions `get_account_trx_balace()`,
#'     `get_account_trc20_balance()` and `get_account_trc10_balance()`.
#'
#' Balances of TRX and TRC-20 tokens are presented with a precision of 6. This
#'     means that such balances need to be divided by 1 million
#'     (after converting to `as.numeric`) to obtain the actual values.
#'     Presisions of the TRC-10 assets can vary. Use
#'     `detailed_trc10_info = TRUE` retrieve these precisions (see column
#'     `precision` in the tibble stored in `trc10_balance` of the
#'     object returned by this function).
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' r <- get_account_balance("TQjaZ9FD473QBTdUzMLmSyoGB6Yz1CGpux")
#' print(r)
#'
get_account_balance <- function(address,
                                only_confirmed = FALSE,
                                detailed_trc10_info = FALSE,
                                max_attempts = 3L) {

  stopifnot(is.character(address))
  stopifnot(is.logical(only_confirmed))
  stopifnot(is.logical(detailed_trc10_info))
  stopifnot(is.integer(max_attempts) & max_attempts > 0)

  query_params <- list(only_confirmed = tolower(only_confirmed))

  url <- tronr::build_get_request(base_url = "https://api.trongrid.io",
                                  path = c("v1", "accounts", address),
                                  query_parameters = query_params)

  r <- tronr::api_request(url = url, max_attempts = max_attempts)
  data <- r$data[[1]]

  if (is.null(data$trc20) | length(data$trc20) == 0) {

    trc20 <- as.character(NA)
    n_trc20 <- 0

  } else {

    trc20 <- data$trc20 %>% unlist() %>%
      tibble::enframe(name = "trc20", value = "balance") %>%
      dplyr::mutate(balance = as.character(.data$balance))
    n_trc20 <- length(data$trc20)

  }

  if (is.null(data$assetV2) | length(data$assetV2) == 0) {

    trc10 <- as.character(NA)
    n_trc10 <- 0

  } else {

    trc10 <- lapply(data$assetV2, function(x){
        tronr::get_asset_by_id(id = x$key,
                               detailed_info = detailed_trc10_info) %>%
          dplyr::mutate(balance = as.character(gmp::as.bigz(x$value)))
      }) %>%
      dplyr::bind_rows()

    n_trc10 <- length(data$assetV2)
  }


  result <- tibble::tibble(
    request_time = tronr::from_unix_timestamp(r$meta$at, tz = "UTC"),
    address = data$address,
    trx_balance = as.character(ifelse(is.null(data$balance),
                                      NA,
                                      data$balance)),
    n_trc20 = n_trc20,
    trc20_balance = list(trc20),
    n_trc10 = n_trc10,
    trc10_balance = list(trc10)
  )

  return(result)

}
