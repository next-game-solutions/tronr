#' Get blocks for a time range
#'
#' Retrives attributes of the blocks produced within a user-specified time range
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
      limit = 50,
      start_timestamp = min_timestamp,
      end_timestamp = max_timestamp
    ),
    max_attempts = max_attempts
  )

  result <- lapply(data, function(x) {
    tibble::as_tibble(x) %>%
      dplyr::mutate(
        number = as.character(.data$number),
        timestamp = from_unix_timestamp(.data$timestamp)
      ) %>%
      dplyr::rename(
        tx_count = .data$nrOfTrx,
        block_number = .data$number
      )
  }) %>%
    dplyr::bind_rows() %>%
    dplyr::select(
      block_number = .data$block_number,
      timestamp = .data$timestamp,
      hash = .data$hash,
      parent_hash = .data$parentHash,
      tx_trie_root = .data$txTrieRoot,
      confirmed = .data$confirmed,
      revert = .data$revert,
      size = .data$size,
      witness_address = .data$witnessAddress,
      witness_name = .data$witnessName,
      tx_count = .data$tx_count,
      net_usage = .data$netUsage,
      energy_usage = .data$energyUsage
    )

  return(result)
}
