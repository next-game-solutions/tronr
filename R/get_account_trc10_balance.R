#' Get TRC-10 token balances
#'
#' Returns information on the current TRC-10 assets held by an account
#'
#' @param address A character value - address of the account of interest, in
#'     `base58` (starts with `T`) or  `hex` (starts with `41`) format.
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
#' @return A nested tibble with the following columns:
#' * `request_time`: date and time  (UTC timezone) when the API
#'     request was made;
#' * `address`: a character value indicating the account address
#'     (in `hex` format);
#' * `trc10_balance`: a list with tibble that contains information on the
#'     TRC-10 assets held by the account. The actual content of this tibble
#'     will depend on the argument `detailed_trc10_info` (see above).
#'
#' @details TRC-10 is a technical standard used by system contracts to
#'     implement tokens. If an account holds no TRC-10 tokens,
#'     the `trc20_balance` column in the resultant tibble will contain an
#'     `NA` value.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' r <- get_account_trc10_balance("TQjaZ9FD473QBTdUzMLmSyoGB6Yz1CGpux")
#' print(r)
#'
get_account_trc10_balance <- function(address,
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
    n_trc10 = n_trc10,
    trc10_balance = list(trc10)
  )

  return(result)

}
