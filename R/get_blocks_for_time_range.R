#' Get blocks for a time range
#'
#' Retrives attributes of blocks produced within a user-specified time range
#'
#' @eval function_params(c("min_timestamp", "max_timestamp", "max_attempts"))
#'
#' @return A tibble with the following columns:
#' * `block_number` (character);
#' * `timestamp` (POSIXct): date and time when the block was produced;
#' * `hash` (character): hash ID of the block;
#' * `parent_hash` (character): hash ID of the parent block;
#' * `tx_trie_root` (character): [trie](https://en.wikipedia.org/wiki/Trie) root
#' ID of the transaction that produced the block;
#' * `confirmed` (boolean): indicator of whether the block is confirmed;
#' * `revert` (boolean): indicator of the whether the block-producing
#' transaction was reverted;
#' * `size` (integer): block size (in bytes);
#' * `witness_address` (character): `base58check`-formatted address of the
#' account that produced the block;
#' * `witness_name` (character): common name of the account that produced the
#' block;
#' * `tx_count`(integer): number of transactions associated with the block;
#' * `net_usage` (integer): total bandwidth consumed by the block's transactions;
#' * `energy_usage` (integer): total energy used by the block's transactions.
#'
#' @export
#'
#' @examples
#' r <- get_blocks_for_time_range(
#'   min_timestamp = "1551715200000",
#'   max_timestamp = "1551715210000"
#' )
#' print(r)
get_blocks_for_time_range <- function(min_timestamp,
                                      max_timestamp,
                                      max_attempts = 3L) {
  validate_arguments(
    arg_min_timestamp = min_timestamp,
    arg_max_timestamp = max_timestamp,
    arg_max_attempts = max_attempts
  )

  data <- run_paginated_tronscan_query(
    base_url = "https://apilist.tronscan.org/",
    path = c("api", "block"),
    params = list(
      sort = "-number",
      limit = 25,
      start_timestamp = min_timestamp,
      end_timestamp = max_timestamp
    ),
    max_attempts = max_attempts
  )

  result <- lapply(data, function(x) {
    names(x) <- snakecase::to_snake_case(names(x))

    tibble::as_tibble(x) %>%
      dplyr::mutate(
        number = as.character(.data$number),
        timestamp = from_unix_timestamp(.data$timestamp)
      ) %>%
      dplyr::rename(
        tx_count = .data$nr_of_trx,
        block_number = .data$number
      )
  }) %>%
    dplyr::bind_rows() %>%
    dplyr::select(
      .data$block_number,
      .data$timestamp,
      .data$hash,
      .data$parent_hash,
      .data$tx_trie_root,
      .data$confirmed,
      .data$revert,
      .data$size,
      .data$witness_address,
      .data$witness_name,
      .data$tx_count,
      .data$net_usage,
      .data$energy_usage
    )

  return(result)
}
