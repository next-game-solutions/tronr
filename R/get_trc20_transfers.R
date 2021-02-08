#' Get TRC-20 transfers
#'
#' Returns TRC-20 transfer transactions for a user-specified time range
#'
#' @eval function_params(c("contract_address", "min_timestamp", "max_timestamp",
#'                         "max_attempts"))
#'
#' @details If `contract_address = NULL`, all TRC-20 token transfers will be
#'     returned for the requested time range. However, if `contract_address` is
#'     specified, only results for the corresponding token will be returned.
#'     This argument accepts only one address at a time.
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
#' [get_trc20_token_description()] for their definitions):
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
#' * `token_contract_address` (character);
#' * `token_name` (character);
#' * `token_abbr` (character).
#'
#' @export
#'
#' @examples
#' # Results contain all TRC-20 transfers that took place
#' # within the specified time range:
#' r1 <- get_trc20_transfers(
#'   min_timestamp = "1609459860000",
#'   max_timestamp = "1609459875000"
#' )
#' print(r1)
#'
#' # Results contain transfers of a specific token:
#' r2 <- r <- get_trc20_transfers(
#'   contract_address = "TR7NHqjeKQxGTCi8q8ZY4pL8otSzgjLj6t",
#'   min_timestamp = "1609459860000",
#'   max_timestamp = "1609459875000"
#' )
#' print(r2)
get_trc20_transfers <- function(contract_address = NULL,
                                min_timestamp,
                                max_timestamp,
                                max_attempts = 3L) {
  validate_arguments(
    arg_contract_address = contract_address,
    arg_min_timestamp = min_timestamp,
    arg_max_timestamp = max_timestamp,
    arg_max_attempts = max_attempts
  )

  if (!is.null(contract_address) & length(contract_address) > 1L) {
    rlang::abort("`contract_address` only accepts vectors of length 1")
  }

  if (!is.null(contract_address) && (
    substr(contract_address, 1, 2) == "41" |
    substr(contract_address, 1, 2) == "0x")
  ) {
    contract_address <- convert_address(contract_address)
  }

  data <- run_paginated_tronscan_query(
    base_url = "https://apilist.tronscan.org/",
    path = c("api", "token_trc20", "transfers"),
    params = list(
      limit = 20,
      start_timestamp = min_timestamp,
      end_timestamp = max_timestamp,
      contract_address = contract_address
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

    if ("token_info" %in% names(x) &
        is.list(x$token_info) &
        length(x$token_info) != 0) {
      token_name <- x$token_info$tokenName
      token_abbr <- x$token_info$tokenAbbr
      token_decimal <- as.numeric(x$token_info$tokenDecimal)
    } else {
      token_name <- NA_character_
      token_abbr <- NA_character_
      token_decimal <- 0
    }

    r <- tibble::tibble(
      tx_id = x$transaction_id,
      block_number = as.character(x$block),
      timestamp = from_unix_timestamp(x$block_ts),
      from_address = x$from_address,
      to_address = x$to_address,
      is_contract_from_address = x$from_address_is_contract,
      is_contract_to_address = x$to_address_is_contract,
      contract_result = x$contract_ret,
      confirmed = x$confirmed,
      amount = apply_decimal(as.numeric(x$quant), token_decimal),
      token_contract_address = x$contract_address,
      token_name = token_name,
      token_abbr = token_name
    )

    pb$tick()

    return(r)
  }) %>%
    dplyr::bind_rows()

  pb$finished <- TRUE

  return(result)
}
