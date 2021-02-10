#' Get TRC-10 transfers
#'
#' Returns TRC-10 transfer transactions for a specific combination of time range, token and account
#'
#' @eval function_params(c("owner_address", "related_address",
#'                         "min_timestamp", "max_timestamp", "max_attempts"))
#'
#' @details If `owner_address = NULL`, all TRC-10 asset transfers will be
#'     returned, including Tronix (TRX) (which is technically a TRC-10 token).
#'     However, if `owner_address` is specified, only results for the
#'     corresponding token will be returned. This argument accepts only one
#'     address at a time.
#'
#'     If `related_address` is specified, the returned TRC-10 asset transfers
#'     will be in relation to that specific `related_address`, and will include
#'     both incoming or outgoing transfers.
#'
#' The number of transfers that take place on the TRON blockchain can be
#'     very large, and thus users are advised to choose `min_timestamp` and
#'     `max_timestamp` wisely. If the requested time range is too large, the
#'     maximum number of transactions returned by the underlying Tronscan API
#'     will be _capped_ at 10000, and the processing time may become
#'     prohibitively long. Chunking the time range of interest into
#'     smaller periods can help to avoid gaps in data in such cases. However,
#'     users would have to implement their own logic for that.
#'
#' @return Tibble with the following colums (see [get_tx_info_by_id()] and
#' [get_trc10_token_description()] for their definitions):
#' * `tx_id` (character);
#' * `block_number` (character);
#' * `timestamp` (POSIXct, UTC timezone);
#' * `from_address` (character);
#' * `to_address` (character);
#' * `is_contract_from_address` (boolean);
#' * `is_contract_to_address` (boolean);
#' * `contract_result` (character);
#' * `confirmed` (boolean);
#' * `amount` (double);
#' * `token_id` (character);
#' * `token_name` (character);
#' * `token_abbr` (character).
#'
#' @export
#'
#' @examples
#' # Results contain all TRC-10 transfers that took place
#' # within the specified time range:
#' r1 <- get_trc10_transfers(
#'   min_timestamp = "1577837400000",
#'   max_timestamp = "1577837430000"
#' )
#' print(r1)
#'
#' # Results contain all TRC-10 transfers to/from a specific account
#' # that took place within the specified time range:
#' r2 <- get_trc10_transfers(
#'   related_address = "TMaBqmMRekKZMQEq3u3QrJpGDwPYZZo87V",
#'   min_timestamp = "1577837400000",
#'   max_timestamp = "1577837430000"
#' )
#' print(r2)
#'
#' # Results contain transfers of a specific token
#' # that took place within the specified time range:
#' r3 <- get_trc10_transfers(
#'   owner_address = "TF5Bn4cJCT6GVeUgyCN4rBhDg42KBrpAjg",
#'   min_timestamp = "1577837400000",
#'   max_timestamp = "1577837430000"
#' )
#' print(r3)
#'
#' # Results contain transfers of a specific token to/from a specifc address
#' # that took place within the specified time range:
#' r4 <- get_trc10_transfers(
#'   owner_address = "THLLMnsEKEci5e5dJHnW28QQU8AujGhSoK",
#'   related_address = "TBhjJyuXnadzLX1s3yHFduZgpCeWEzda5u",
#'   min_timestamp = "1577837400000",
#'   max_timestamp = "1577837430000"
#' )
#' print(r4)
get_trc10_transfers <- function(owner_address = NULL,
                                related_address = NULL,
                                min_timestamp,
                                max_timestamp,
                                max_attempts = 3L) {
  validate_arguments(
    arg_address = owner_address,
    arg_related_address = related_address,
    arg_min_timestamp = min_timestamp,
    arg_max_timestamp = max_timestamp,
    arg_max_attempts = max_attempts
  )

  if (!is.null(owner_address) & length(owner_address) > 1L) {
    rlang::abort("`owner_address` only accepts vectors of length 1")
  }

  if (!is.null(owner_address) && (
    substr(owner_address, 1, 2) == "41" |
      substr(owner_address, 1, 2) == "0x")
  ) {
    owner_address <- convert_address(owner_address)
  }

  if (!is.null(related_address) & length(related_address) > 1L) {
    rlang::abort("`related_address` only accepts vectors of length 1")
  }

  if (!is.null(related_address) && (
    substr(related_address, 1, 2) == "41" |
      substr(related_address, 1, 2) == "0x")
  ) {
    related_address <- convert_address(related_address)
  }

  data <- run_paginated_tronscan_query(
    base_url = "https://apilist.tronscan.org/",
    path = c("api", "asset", "transfer"),
    params = list(
      sort = "timestamp",
      limit = 25,
      start_timestamp = min_timestamp,
      end_timestamp = max_timestamp,
      issueAddress = owner_address,
      relatedAddress = related_address
    ),
    show_spinner = TRUE,
    max_attempts = max_attempts
  )

  if (length(data) == 0) {
    message("\nNo data found")
    return(NULL)
  }

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
        token_owner_address = "...",
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
          token_owner_address = "...",
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
      NA, .data$amount
    )) %>%
    dplyr::relocate(.data$token_id, .after = .data$amount) %>%
    dplyr::select(
      -.data$precision,
      -.data$token_owner_address
    )

  return(result)
}
