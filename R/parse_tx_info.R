#' Parse transaction attributes
#'
#' Converts a list with transaction attributes into a nested tibble
#'
#' @param info A list returned as a result of calling the
#'     "get transaction info by account address" method.
#'
#' @details This is a non-public function that is used to simplify syntax of
#'     the `get_tx_info_by_account_address()` function.
#'
#' @return A nested tibble where each row corresponds to one transaction.
#' @export
#'
#' @examples query_params <- list(only_confirmed = tolower(TRUE),
#'                                only_to = tolower(TRUE),
#'                                min_timestamp = "1577836800000",
#'                                max_timestamp = "1577838600000",
#'                                limit = 100L)
#' address <- "TAUN6FwrnwwmaEqYcckffC7wYmbaS6cBiX"
#' url <- tronr::build_get_request(base_url = "https://api.trongrid.io",
#'                                 path = c("v1", "accounts",
#'                                 address, "transactions"),
#'                                 query_parameters = query_params)
#' r <- tronr::api_request(url = url, max_attempts = max_attempts)
#' tx_info <- r$data
#' tx_info_parsed <- parse_tx_info(tx_info)
#' print(tx_info_parsed)
#'
parse_tx_info <- function(info) {

  stopifnot(is.list(info))

  null_checker <- function(x) {
    ifelse(!is.null(x), as.character(x), NA_character_)
  }

  raw_data <- info$raw_data$contract[[1]]$parameter$value %>%
    tibble::as_tibble() %>%
    dplyr::mutate_if(., is.numeric, .funs = ~ as.character(gmp::as.bigz(.)))
  names(raw_data)[grepl("owner_address", names(raw_data))] <- "from_address"
  names(raw_data)[grepl("owner_address", names(raw_data)) &
                    !grepl("from_address", names(raw_data))] <- "to_address"


  tx_result <- null_checker(info$ret[[1]]$contractRet)

  net_usage = null_checker(info$net_usage)

  net_fee = null_checker(info$net_fee)

  energy_usage = null_checker(info$energy_usage)

  energy_fee = null_checker(info$energy_fee)

  internal_txs <- ifelse(length(info$internal_transactions) != 0L,
                        info$internal_transactions,
                        NA_character_)

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
    raw_data = list(raw_data),
    internal_txs = internal_txs

  )

  return(res)

}
