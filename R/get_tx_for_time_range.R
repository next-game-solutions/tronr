#' Get transactions for a time range
#'
#' Returns transactions that took place within a user-specified period of time
#'
#' @eval function_params(c("min_timestamp", "max_timestamp",
#'                         "add_contract_data", "max_attempts"))
#'
#' @return A nested tibble where each row corresponds to one transaction.
#'     A detailed description of the content of this tibble can be found in the
#'     help file for [get_tx_info_by_id()].
#'
#' @details The number of transactions that take place on the TRON blockchain
#'     is very large, and thus users are advised to choose `min_timestamp` and
#'     `max_timestamp` wisely. If the requested time range is too large, the
#'     maximum number of transactions returned by the underlying Tronscan API
#'     will be _capped_ at 10000, and the processing time may become
#'     prohibitively long. Chunking the time range of interest into
#'     smaller periods can help to avoid gaps in data in such cases. However,
#'     users would have to implement their own logic for that.
#'
#' @export
#'
#' @examples
#' \donttest{
#' min_timestamp <- "1577836800000"
#' max_timestamp <- "1577836803000"
#' tx_df <- get_tx_for_time_range(min_timestamp, max_timestamp)
#' print(tx_df)
#' }
get_tx_for_time_range <- function(min_timestamp = 0,
                                  max_timestamp = NULL,
                                  add_contract_data = TRUE,
                                  max_attempts = 3L) {
  validate_arguments(
    arg_min_timestamp = min_timestamp,
    arg_max_timestamp = max_timestamp,
    arg_max_attempts = max_attempts,
    arg_add_contract_data = add_contract_data
  )

  data <- run_paginated_tronscan_query(
    base_url = "https://apilist.tronscan.org/",
    path = c("api", "transaction"),
    params = list(
      sort = "timestamp",
      limit = 25,
      count = tolower(TRUE),
      start_timestamp = min_timestamp,
      end_timestamp = max_timestamp
    ),
    show_spinner = TRUE,
    max_attempts = max_attempts
  )

  hashes <- unlist(lapply(data, function(x) {
    x$hash
  }))

  pb <- progress::progress_bar$new(
    total = length(hashes),
    clear = TRUE,
    force = TRUE,
    format = "Processing data... [:bar] :percent"
  )
  pb$tick(0)

  result <- list()

  for (i in 1:length(hashes)) {
    tx <- get_tx_info_by_id(
      tx_id = hashes[i],
      add_contract_data = add_contract_data,
      max_attempts = max_attempts
    )

    result[[i]] <- tx

    pb$tick()
  }

  pb$finished <- TRUE

  result <- dplyr::bind_rows(result)

  return(result)
}
