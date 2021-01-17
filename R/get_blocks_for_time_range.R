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
    )
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
