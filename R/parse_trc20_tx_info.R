#' Parse TRC20 transaction attributes
#'
#' Converts a list with transaction attributes into a nested tibble
#'
#' @param info A list returned as a result of calling the
#'     ["get TRC20 transaction info by account address"](https://developers.tron.network/reference#transaction-information-by-account-address)
#'     method.
#'
#' @details This is a non-public function that is used internally to simplify
#'     syntax of `get_trc20_tx_info_by_account_address()`.
#'
#' @return A nested tibble with one row and the following columns:
#'
#' - `tx_id` (character) - transation ID;
#' - `tx_type` (character) - transation type (e.g., `"Transfer"`);
#' - `block_timestamp` (POSIXct);
#' - `from_address` (character);
#' - `to_address` (character);
#' - `trc20_symbol` (character) - an abbreviated name of the TRC20 token;
#' - `trc20_name` (character) - a common name of the TRC20 token;
#' - `trc20_contract_address` (character);
#' - `precision` (integer) - precision of the `amount` values;
#' - `amount` (character) - transaction amount of the TRC20 token.
#'
#' @export
#'
#' @examples query_params <- list(min_timestamp = "1604188800000",
#'                                max_timestamp = "1604189100000",
#'                                limit = 20L)
#' address <- "TAUN6FwrnwwmaEqYcckffC7wYmbaS6cBiX"
#' url <- tronr::build_get_request(base_url = "https://api.trongrid.io",
#'                                 path = c("v1", "accounts",
#'                                 address, "transactions", "trc20"),
#'                                 query_parameters = query_params)
#' r <- tronr::api_request(url = url, max_attempts = 3L)
#' tx_info <- r$data[[1]]
#' tx_info_parsed <- parse_trc20_tx_info(tx_info)
#' print(tx_info_parsed)
#'
parse_trc20_tx_info <- function(info) {

  stopifnot(is.list(info))

  res <- tibble::tibble(

    tx_id = info$transaction_id,
    tx_type = info$type,
    block_timestamp = tronr::from_unix_timestamp(info$block_timestamp),
    from_address = info$from,
    to_address = info$to,
    trc20_symbol = info$token_info$symbol,
    trc20_name = info$token_info$name,
    trc20_contract_address = info$token_info$address,
    precision = info$token_info$decimals,
    amount = as.character(gmp::as.bigz(info$value))

  )

  return(res)

}
