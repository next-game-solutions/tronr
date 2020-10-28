#' Get TRX balance
#'
#' Returns the current TRX balance of an account
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
#' @return A tibble with the following columns:
#'     - `request_time`: date and time  (UTC timezone) when the API
#'     request was made;
#'     - `address`: a character value indicating the account address
#'     (in `hex` format);
#'     - `trx_balance`: a character value.
#' @export
#'
#' @examples
#'
#' r <- get_trx_balance("TQjaZ9FD473QBTdUzMLmSyoGB6Yz1CGpux")
#' # print(r)
#'
get_trx_balance <- function(address,
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

  result <- tibble::tibble(
    request_time = tronr::from_unix_timestamp(r$meta$at, tz = "UTC"),
    address = data$address,
    trx_balance = as.character(ifelse(is.null(data$balance),
                                      NA,
                                      data$balance))
  )

  return(result)

}
