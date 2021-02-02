get_trc10_transfers <- function(owner_address = NULL,
                                min_timestamp,
                                max_timestamp,
                                max_attempts = 3L) {
  validate_arguments(
    arg_address = owner_address,
    arg_min_timestamp = min_timestamp,
    arg_max_timestamp = max_timestamp,
    arg_max_attempts = max_attempts
  )

  if (!is.null(owner_address) && (
    substr(owner_address, 1, 2) == "41" | substr(owner_address, 1, 2) == "0x")
  ) {
    owner_address <- convert_address(owner_address)
  }

  data <- run_paginated_tronscan_query(
    base_url = "https://apilist.tronscan.org/",
    path = c("api", "asset", "transfer"),
    params = list(
      sort = "timestamp",
      limit = 25,
      start_timestamp = min_timestamp,
      end_timestamp = max_timestamp,
      issueAddress = owner_address
    ),
    show_spinner = TRUE,
    max_attempts = max_attempts
  )

  pb <- progress::progress_bar$new(
    total = NA,
    clear = TRUE,
    force = TRUE,
    format = ":spin Processing data... Elapsed time: :elapsedfull"
  )
  pb$tick(0)

  result <- lapply(data, function(x) {
    names(x) <- snakecase::to_snake_case(names(x))

    r <- tibble::tibble(
      tx_id = x$transaction_hash,
      block_number = as.character(x$block_id),
      timestamp = from_unix_timestamp(x$timestamp),
      token_id = x$token_name,
      from_address = x$transfer_from_address,
      to_address = x$transfer_to_address,
      is_contract_from_address = x$from_address_is_contract,
      is_contract_to_address = x$to_address_is_contract,
      contract_result = x$contract_ret,
      confirmed = x$confirmed,
      amount = x$amount
    )

    pb$tick()

    return(r)
  }) %>%
    dplyr::bind_rows()

  pb$finished <- TRUE

  token_ids <- unique(result$token_id)

  token_info_result <- list()

  for (i in 1:length(token_ids)) {
    if (token_ids[i] == "_") {
      token_info_result[[i]] <- tibble::tibble(
        token_id = "_",
        token_name = "Tronix",
        token_abbr = "TRX",
        owner_address = "...",
        precision = 6L
      )
    } else {
      token <- suppressMessages(
        get_trc10_token_description(
          token_id = token_ids[i],
          detailed_info = FALSE,
          max_attempts = max_attempts
        )
      )

      if (is.null(token)) {
        token_info_result[[i]] <- tibble::tibble(
          token_id = token_ids[i],
          token_name = NA_character_,
          token_abbr = NA_character_,
          owner_address = "...",
          precision = 0L
        )
      } else {
        token_info_result[[i]] <- token
      }
    }
  }

  token_info_result <- dplyr::bind_rows(token_info_result)

  result <- dplyr::left_join(result,
    token_info_result,
    by = "token_id"
  ) %>%
    dplyr::mutate(
      amount = apply_decimal(.data$amount, .data$precision),
      token_id = ifelse(.data$token_id == "_",
        "TRX", .data$token_id
      )
    ) %>%
    dplyr::mutate(amount = ifelse(
      is.na(.data$token_name) & is.na(.data$token_abbr),
      NA, amount
    )) %>%
    dplyr::relocate(.data$token_id, .after = .data$amount) %>%
    dplyr::select(
      -.data$precision,
      -.data$owner_address
    )

  return(result)
}
