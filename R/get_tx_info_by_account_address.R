#' Get transactions for an account
#'
#' Returns a list of transactions associated with an account, and their attributes
#'
#' @eval function_params(c("address", "min_timestamp", "max_timestamp",
#'                         "add_contract_data", "max_attempts"))
#'
#' @details Some addresses have a very high load of transactions going
#'     through them. Users are, therefore, advised to choose `min_timestamp` and
#'     `max_timestamp` wisely as the query covering a large period of time may
#'     take a long time to run, or the Tronscan API calls may get denied
#'     altogether. Chunking the time range of interest into smaller periods can
#'     help, although users will have to implement their own logic for that.
#'
#' @return A nested tibble where each row corresponds to one transaction.
#'     A detailed description of the content of this tibble can be found in the
#'     help file for [get_tx_info_by_id()].
#'
#' @export
#'
#' @examples
#' tx_df <- get_tx_info_by_account_address(
#'   address = "TAUN6FwrnwwmaEqYcckffC7wYmbaS6cBiX",
#'   min_timestamp = "1577836800000",
#'   max_timestamp = "1577838600000"
#' )
#' print(tx_df)
get_tx_info_by_account_address <- function(address,
                                           min_timestamp = 0,
                                           max_timestamp = NULL,
                                           add_contract_data = TRUE,
                                           max_attempts = 3L) {
  validate_arguments(
    arg_address = address,
    arg_min_timestamp = min_timestamp,
    arg_max_timestamp = max_timestamp,
    arg_add_contract_data = add_contract_data,
    arg_max_attempts = max_attempts
  )

  if (substr(address, 1, 2) == "41" | substr(address, 1, 2) == "0x") {
    address <- convert_address(address)
  }

  data <- run_paginated_tronscan_query(
    base_url = "https://apilist.tronscan.org/",
    path = c("api", "transaction"),
    params = list(
      sort = "timestamp",
      limit = 25,
      count = tolower(TRUE),
      start_timestamp = min_timestamp,
      end_timestamp = max_timestamp,
      address = address
    ),
    max_attempts = max_attempts
  )

  hashes <- unlist(lapply(data, function(x){x$hash}))

  pb <- progress::progress_bar$new(
    total = length(hashes),
    clear = TRUE,
    force = TRUE
  )
  pb$tick(0)

  result <- list()

  for (i in 1:length(hashes)) {

    tx <- get_tx_info_by_id(tx_id = hashes[i],
                            add_contract_data = add_contract_data,
                            max_attempts = max_attempts)

    result[[i]] <- tx

    pb$tick()

  }

  pb$finished <- TRUE

  result <- dplyr::bind_rows(result)

  result <- tibble::tibble(address, result) %>%
    dplyr::relocate(.data$address, .after = .data$request_time) %>%
    dplyr::arrange(.data$timestamp)

  return(result)
}
