#' Get TRC-10 token balances
#'
#' Returns information on the current TRC-10 assets held by an account
#'
#' @eval function_params(c("address", "only_confirmed",
#'                         "detailed_info", "max_attempts"))
#'
#' @return A nested tibble with the following columns:
#' * `request_time` (POSIXct, UTC timezone): date and time when the API
#'     request was made;
#' * `address` (character): account address, in `base58` format;
#' * `n_trc10` (double): number of unique TRC-10 assets held by `address`;
#' * `trc10_balance` (list): contains a tibble with `n_trc20` rows and
#'     several attributes of the TRC-10 assets held by `address`. The actual
#'     content of this tibble will depend on the argument `detailed_info`
#'     (see [get_asset_by_id()] for details).
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
get_account_trc10_balance <- function(address,
                                      only_confirmed = NULL,
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

  if (is.null(data$assetV2) | length(data$assetV2) == 0L) {
    trc10 <- NA_real_
    n_trc10 <- 0
  } else {
    trc10 <- lapply(data$assetV2, function(x) {
      tronr::get_asset_by_id(
        asset_id = x$key,
        detailed_info = detailed_info
      ) %>%
        dplyr::mutate(
          balance = x$value,
          owner_address = tronr::convert_address(.data$owner_address)
        )
    }) %>%
      dplyr::bind_rows()

    n_trc10 <- as.numeric(length(data$assetV2))
  }

  result <- tibble::tibble(
    request_time = tronr::from_unix_timestamp(r$meta$at, tz = "UTC"),
    address = tronr::convert_address(data$address),
    n_trc10 = n_trc10,
    trc10_balance = list(trc10)
  )

  return(result)
}
