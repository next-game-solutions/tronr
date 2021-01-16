#' Get the latest block
#'
#' Retrieves information on the latest block and its transactions
#'
#' @eval function_params(c("max_attempts"))
#'
#' @return A nested tibble with the following columns:
#' * `block_number` (integer): number of the latest block;
#' * `timestamp` (POSIXct): date and time when the block was created;
#' * `hash` (character): hash of the block;
#' * `parent_hash` (character): hash of the parent block;
#' * `tx_trie_root` (character): hash of the
#' [trie](https://en.wikipedia.org/wiki/Trie) root of the block creation
#' transaction;
#' * `confirmed` (boolean): an indicator of whether this block has been
#' confirmed;
#' * `size` (integer): size of the block (in bytes);
#' * `witness_address` (character): address of the block's creator;
#' * `tx_count` (integer): number of transactions associated with this block;
#' * `tx` (list): a list with one element that contains a tibble with basic
#' info on the transactions associated with this block: `tx_id` (character) -
#' transaction ID, `from_address` (character) - address of the account that
#' initiated the transaction, and `to_address` (character) - address of the
#' recieving account. All addresses are presented in the `base58check` format.
#'
#' @details Additional details on the block producing process can be found
#'     in the [official documentation](https://tronprotocol.github.io/documentation-en/introduction/dpos/#dpos).
#'
#' @export
#'
#' @examples r <- get_latest_block()
#' print(r)
#'
get_latest_block <- function(max_attempts = 3L) {
  validate_arguments(arg_max_attempts = max_attempts)

  url <- build_get_request(
    base_url = "https://apilist.tronscan.org/",
    path = c("api", "block", "latest"),
    query_parameters = list()
  )

  r <- api_request(url = url, max_attempts = max_attempts)

  names(r) <- snakecase::to_snake_case(names(r))
  r$witness_id <- NULL

  result <- tibble::as_tibble(r) %>%
    dplyr::mutate(timestamp = from_unix_timestamp(.data$timestamp)) %>%
    dplyr::rename(
      tx_count = .data$nr_of_trx,
      block_number = .data$number
    )

  tx_data <- run_paginated_tronscan_query(
    base_url = "https://apilist.tronscan.org/",
    path = c("api", "transaction"),
    params = list(
      sort = "-timestamp",
      count = tolower(TRUE),
      limit = 25,
      block = result$block_number
    )
  )

  tx <- lapply(tx_data, function(x) {
    tibble::tibble(
      tx_id = x$hash,
      from_address = ifelse(nchar(x$ownerAddress) == 0,
        NA_character_,
        x$ownerAddress
      ),
      to_address = ifelse(nchar(x$toAddress) == 0,
        NA_character_,
        x$toAddress
      )
    )
  }) %>%
    dplyr::bind_rows()

  result <- tibble::tibble(result, tx = list(tx)) %>%
    dplyr::select(
      .data$block_number,
      .data$timestamp,
      .data$hash,
      .data$parent_hash,
      .data$tx_trie_root,
      .data$confirmed,
      .data$size,
      .data$witness_address,
      .data$tx_count,
      .data$tx
    )

  return(result)
}
