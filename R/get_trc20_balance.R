#' Get TRC-20 token balances
#'
#' Returns the current TRC-20 token balances of an account
#'
#' @param address A character value - address of the account of interest, in
#'     `base58` (starts with `T`) or  `hex` (starts with `41`) format.
#' @param only_confirmed A boolean value. If `TRUE`, account balance will be
#'     returned as of the latest confirmed block, otherwise as of the
#'     latest unconfirmed one. Defaults to `FALSE`.
#' @param max_attempts A non-zero, positive integer specifying the maximum
#'     number of attempts to call the API if the first attempt fails
#'     (i.e. its call status is different from `200`). Additional attempts are
#'     implemented with an exponential backoff. Defaults to 3.
#'
#' @return A nested tibble with the following columns:
#' * `request_time`: date and time  (UTC timezone) when the API
#'     request was made;
#' * `address`: a character value indicating the account address
#'     (in `hex` format);
#' * `trc20_balance`: a list that contains a tibble with two columns:
#'     `trc20` (`base58`-formatted address of the token) and `balance`
#'     (a character value, amount of the respective token).
#'
#' @details TRC-20 is a technical standard used for smart contracts on the
#'     TRON blockchain for implementing tokens with the TRON Virtual Machine
#'     (TVM). If an account holds no TRC-20 tokens, the `trc20_balance` column
#'     in the tibble returned by this function will contain an `NA` value.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' r <- get_trc20_balance("TQjaZ9FD473QBTdUzMLmSyoGB6Yz1CGpux")
#' print(r)
#'
get_trc20_balance <- function(address,
                              only_confirmed = FALSE,
                              max_attempts = 3L) {

  stopifnot(is.character(address))
  stopifnot(is.logical(only_confirmed))
  stopifnot(is.integer(max_attempts) & max_attempts > 0)

  base_url <- "https://api.trongrid.io"
  url <- httr::modify_url(base_url, path = c("v1", "accounts", address))
  url <- httr::parse_url(url)
  url$query <- list(only_confirmed = tolower(only_confirmed))
  url <- httr::build_url(url)

  r <- tronr::api_request(url = url, max_attempts = max_attempts)
  data <- r$data[[1]]

  if (is.null(data$trc20) | length(data$trc20) == 0) {
    trc20 <- NA
  } else {
    trc20 <- data$trc20 %>% unlist() %>%
      tibble::enframe(name = "trc20",
                      value = "balance") %>%
      dplyr::mutate(balance = as.character(.data$balance))
  }

  result <- tibble::tibble(
    request_time = tronr::from_unix_timestamp(r$meta$at, tz = "UTC"),
    address = data$address,
    trc20_balance = list(trc20)
  )

  return(result)

}
