#' Parse transaction attributes
#'
#' Converts a list with transaction attributes into a nested tibble
#'
#' @param info A non-empty, named list returned as a result of calling the
#'     ["get transaction info by account address"](https://developers.tron.network/reference#transaction-information-by-account-address)
#'     method.
#'
#' @return A nested tibble with one row and the following columns:
#'
#' - `address` (character): same as the argument `address`;
#' - `tx_id` (character): transation ID;
#' - `tx_type` (character): transation type (see [here](https://tronscan-org.medium.com/tronscan-class-transaction-b6b3ea681e43)
#' and [here](https://tronscan-org.medium.com/tronscan-class-transaction-b6b3ea681e43)
#' for a list of all possible values and further details);
#' - `tx_result` (character): transation status (e.g., `SUCCESS`);
#' - `net_usage` (character);
#' - `net_fee` (character);
#' - `energy_usage` (character);
#' - `energy_fee` (character);
#' - `block_number` (character);
#' - `block_timestamp` (POSIXct, UTC timezone);
#' - `raw_data` (list) - each element of this list contains a tibble with
#' additional transaction attributes (the actual structure of a given tibble
#' will depend on `tx_type`, but among other things it will typically
#' contain `tx_timestamp` (POSIXct, UTC timezone), `amount` (character),
#' `from_address` (character, `base58check`-formatted), and
#' `to_address` (character, `base58check`-formatted).
#'
#' @importFrom magrittr %>%
#'
#' @export
#'
#' @examples
#' query_params <- list(
#'   only_confirmed = tolower(TRUE),
#'   only_to = tolower(TRUE),
#'   min_timestamp = "1577836800000",
#'   max_timestamp = "1577838600000",
#'   limit = 100L
#' )
#' address <- "TAUN6FwrnwwmaEqYcckffC7wYmbaS6cBiX"
#' url <- tronr::build_get_request(
#'   base_url = "https://api.trongrid.io",
#'   path = c(
#'     "v1", "accounts",
#'     address, "transactions"
#'   ),
#'   query_parameters = query_params
#' )
#' r <- tronr::api_request(url = url, max_attempts = 3L)
#' tx_info <- r$data[[1]]
#' tx_info_parsed <- parse_tx_info(tx_info)
#' print(tx_info_parsed)
parse_tx_info <- function(info) {
  if (!is.list(info)) {
    rlang::abort("`info` must be a list")
  }

  null_checker <- function(x) {
    ifelse(!is.null(x), as.character(x), NA_character_)
  }

  if (is.null(info$raw_data$timestamp)) {
    tx_timestamp <- NA
  } else {
    tx_timestamp <- info$raw_data$timestamp %>%
      gmp::as.bigz() %>%
      as.character() %>%
      tronr::from_unix_timestamp()
  }

  raw_data <- info$raw_data$contract[[1]]$parameter$value %>%
    tibble::as_tibble() %>%
    dplyr::mutate_if(is.numeric, .funs = ~ as.character(gmp::as.bigz(.))) %>%
    dplyr::mutate(timestamp = tx_timestamp) %>%
    dplyr::relocate("timestamp")

  names(raw_data)[grepl("call_value", names(raw_data))] <- "amount"
  names(raw_data)[grepl("asset_name", names(raw_data))] <- "asset_id"
  names(raw_data)[grepl("timestamp", names(raw_data))] <- "tx_timestamp"
  names(raw_data)[grepl("owner_address", names(raw_data))] <- "from_address"
  names(raw_data)[grepl("_address", names(raw_data)) &
    !grepl("from_address", names(raw_data))] <- "to_address"

  raw_data$from_address <- tronr::convert_address(raw_data$from_address)
  raw_data$to_address <- tronr::convert_address(raw_data$to_address)


  if ("data" %in% names(raw_data)) {
    raw_data$data <- NULL
  }

  tx_result <- null_checker(info$ret[[1]]$contractRet)

  net_usage <- null_checker(info$net_usage)

  net_fee <- null_checker(info$net_fee)

  energy_usage <- null_checker(info$energy_usage)

  energy_fee <- null_checker(info$energy_fee)

  res <- tibble::tibble(
    tx_id = info$txID,
    tx_type = info$raw_data$contract[[1]]$type,
    tx_result = tx_result,
    net_usage = net_usage,
    net_fee = net_fee,
    energy_usage = energy_usage,
    energy_fee = energy_fee,
    block_number = info$blockNumber %>% gmp::as.bigz() %>% as.character(),
    block_timestamp = tronr::from_unix_timestamp(info$block_timestamp),
    raw_data = list(raw_data)
  )

  return(res)
}
