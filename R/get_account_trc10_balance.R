#' Get TRC-10 token balances
#'
#' Returns information on the current TRC-10 assets held by an account
#'
#' @param address (character) - address of the account of interest, in
#'     `base58` (starts with `T`) or  `hex` (starts with `41`) format.
#' @param only_confirmed (boolean) - if `TRUE`, account balance will be
#'     returned as of the latest confirmed block, otherwise as of the
#'     latest unconfirmed one. Defaults to `FALSE`.
#' @param detailed_trc10_info (boolean) - if `FALSE` (default), only basic
#'     information about the TRC-10 token assets will be returned. If `TRUE`,
#'     an extended information will be returned.
#' @param max_attempts (integer, positive) - a non-zero integer specifying the
#'      maximum number of additional attempts to call the API if the first
#'      attempt fails (i.e. its call status is different from `200`).
#'      Additional attempts are implemented with an exponential backoff.
#'      Defaults to 3.
#'
#' @return A nested tibble with the following columns:
#' * `request_time` (POSIXct, UTC timezone) - date and time when the API
#'     request was made;
#' * `address` (character) - account address, in `base58` format;
#' * `n_trc10` (integer) - number of TRC-10 tokens held by `address`;
#' * `trc10_balance` (list) - contains a tibble with `n_trc20` rows and
#'     several attributes of the TRC-10 assets held by `address`. The actual
#'     content of this tibble will depend on the argument `detailed_trc10_info`
#'     (see above).
#'
#' @details TRC-10 is a technical standard used by system contracts to
#'     implement tokens. If an account holds no TRC-10 tokens (`n_trc10 = 0`),
#'     the `trc10_balance` column in the resultant tibble will contain an
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

  if (!tronr::is_address(address)) {
    rlang::abort("Provided address is not valid")
  }

  if (!is.logical(only_confirmed)) {
    rlang::abort("`only_confirmed` must be boolean")
  }

  if (!is.logical(detailed_trc10_info)) {
    rlang::abort("`detailed_trc10_info` must be boolean")
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

  if (is.null(data$assetV2) | length(data$assetV2) == 0L) {

    trc10 <- NA_character_
    n_trc10 <- 0L

  } else {

    trc10 <- lapply(data$assetV2, function(x){
      tronr::get_asset_by_id(id = x$key,
                             detailed_info = detailed_trc10_info) %>%
        dplyr::mutate(balance = as.character(gmp::as.bigz(x$value)),
                      owner_address = tronr::convert_address(.data$owner_address))
    }) %>%
      dplyr::bind_rows()

    n_trc10 <- length(data$assetV2)
  }

  result <- tibble::tibble(
    request_time = tronr::from_unix_timestamp(r$meta$at, tz = "UTC"),
    address = tronr::convert_address(data$address),
    n_trc10 = n_trc10,
    trc10_balance = list(trc10)
  )

  return(result)

}
