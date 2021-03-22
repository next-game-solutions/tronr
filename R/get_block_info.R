#' Get block attributes
#'
#' Retrieves information on a block and its transactions
#'
#' @param latest (boolean): indicator of whether the latest block's information
#'     should be retrieved (`TRUE` by default).
#' @param block_number (character): number of the block to retrieve the
#'     information for (`NULL` by default). If `latest = TRUE`, this argument is
#'     ignored.
#' @eval function_params(c("max_attempts"))
#'
#' @return A nested tibble with the following columns:
#' * `block_number` (integer): number of the block;
#' * `timestamp` (POSIXct): date and time when the block was created;
#' * `hash` (character): hash ID of the block;
#' * `parent_hash` (character): hash ID of the parent block;
#' * `tx_trie_root` (character): hash ID of the
#' [trie](https://en.wikipedia.org/wiki/Trie) root of the block creation
#' transaction;
#' * `confirmed` (boolean): an indicator of whether this block has been
#' confirmed;
#' * `size` (integer): size of the block (in bytes);
#' * `witness_address` (character): address of the block's creator;
#' * `tx_count` (integer): number of transactions associated with this block;
#' * `tx` (list): a list with one element that contains a tibble with basic
#' info on transactions associated with this block: `tx_id` (character) -
#' transaction ID, `contract_type` (character) - type of the contract that
#' performed this transaction, `from_address` (character) - address of the account
#' that initiated the transaction, and `to_address` (character) - address of the
#' recieving account. All addresses are presented in the `base58check` format.
#'
#' @details Additional details on the block producing process can be found
#'     in the [official documentation](https://tronprotocol.github.io/documentation-en/introduction/dpos/#dpos).
#'
#' @export
#'
#' @examples
#' # latest block
#' r1 <- get_block_info(latest = TRUE)
#' print(r1)
#'
#' # specific block:
#' r2 <- get_block_info(latest = FALSE, block_number = "26810333")
#' print(r2)
get_block_info <- function(latest = TRUE,
                           block_number = NULL,
                           max_attempts = 3L) {
  if (!is.logical(latest)) {
    rlang::abort("`latest` must be a boolean value")
  }

  if (!latest & !is.character(block_number)) {
    rlang::abort("`block_number` must be a character value")
  }

  if (!is.null(block_number) & latest) {
    rlang::abort("`latest = TRUE` and `block_number` cannot be used simultanously")
  }

  validate_arguments(arg_max_timestamp = max_attempts)

  if (latest) {
    url <- build_get_request(
      base_url = "https://apilist.tronscan.org/",
      path = c("api", "block", "latest"),
      query_parameters = list()
    )

    request_time <- Sys.time()
    attr(request_time, "tzone") <- "UTC"

    r <- api_request(url = url, max_attempts = max_attempts)
    names(r) <- snakecase::to_snake_case(names(r))

    result <- tibble::as_tibble(r) %>%
      dplyr::mutate(
        number = as.character(.data$number),
        timestamp = from_unix_timestamp(.data$timestamp)
      ) %>%
      dplyr::rename(
        tx_count = .data$nr_of_trx,
        block_number = .data$number
      )
  } else {
    url <- build_get_request(
      base_url = "https://apilist.tronscan.org/",
      path = c("api", "block"),
      query_parameters = list(number = block_number)
    )

    request_time <- Sys.time()
    attr(request_time, "tzone") <- "UTC"

    r <- api_request(url = url, max_attempts = max_attempts)
    data <- r$data[[1]]
    names(data) <- snakecase::to_snake_case(names(data))

    result <- tibble::as_tibble(data) %>%
      dplyr::mutate(
        number = as.character(.data$number),
        timestamp = from_unix_timestamp(.data$timestamp)
      ) %>%
      dplyr::rename(
        tx_count = .data$nr_of_trx,
        block_number = .data$number
      )
  }

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
      contract_type = convert_contract_type_id(x$contractType),
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

  result <- tibble::tibble(
    request_time = request_time,
    result,
    tx = list(tx)
  ) %>%
    dplyr::select(
      .data$request_time,
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
