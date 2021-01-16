get_block_info <- function(latest = TRUE,
                           block_number = NULL,
                           max_attempts = 3L) {

  if (!is.logical(latest)) {
    rlang::abort("`latest` must be a boolean value")
  }

  if (!latest & !is.character(block_number)) {
    rlang::abort("`block_number` must be a character value")
  }

  validate_arguments(arg_max_timestamp = max_attempts)

  if (latest) {

    url <- build_get_request(
      base_url = "https://apilist.tronscan.org/",
      path = c("api", "block", "latest"),
      query_parameters = list()
    )

    r <- api_request(url = url, max_attempts = max_attempts)

    names(r) <- snakecase::to_snake_case(names(r))

    result <- tibble::as_tibble(r) %>%
      dplyr::mutate(number = as.character(.data$number),
                    timestamp = from_unix_timestamp(.data$timestamp)) %>%
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
      dplyr::mutate(number = as.character(.data$number),
                    timestamp = from_unix_timestamp(.data$timestamp)) %>%
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
