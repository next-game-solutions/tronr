get_tx_info_by_id <- function(id,
                              add_raw_data = FALSE,
                              max_attempts = 3L) {
  validate_arguments(arg_max_attempts = max_attempts)

  url <- build_get_request(
    base_url = "https://apilist.tronscan.org/",
    path = c("api", "transaction-info"),
    query_parameters = list(hash = id)
  )

  request_time <- Sys.time()
  attr(request_time, "tzone") <- "UTC"

  r <- api_request(url = url, max_attempts = max_attempts)
  names(r) <- snakecase::to_snake_case(names(r))

  costs <- tibble::as_tibble(r$cost)

  contract_data_names <-
    names(r$contract_data) <-
    snakecase::to_snake_case(names(r$contract_data))

  null_checker <- function(x) {ifelse(is.null(x), NA, x)}



  # TRX amount transfered as part of the main transaction:
  contract_type <- convert_contract_type_id(r$contract_type)

  if (contract_type == "TransferAssetContract") {

    token_info <- r$contract_data$token_info
    names(token_info) <-
      snakecase::to_snake_case(names(r$contract_data$token_info))

    trc10_transfer <- tibble::tibble(
      token_id = r$contract_data$asset_name,

      token_name = null_checker(token_info$token_name),

      token_abbr = null_checker(token_info$token_abbr),

      vip = null_checker(token_info$vip),

      amount = r$contract_data$amount,

      token_decimal = null_checker(token_info$token_decimal)
    )

  } else {
    trc10_transfer <- NA
  }

  if (contract_type == "TransferContract") {
    trx_transfer <- as.numeric(r$contract_data$amount)
  }









  names(r$contract_data) <- snakecase::to_snake_case(names(r$contract_data))
  contract_data_names <- names(r$contract_data)

  if ("call_value" %in% contract_data_names) {
    trx_transfer <- r$contract_data$call_value
  }

  if ("amount" %in% contract_data_names &
      length(r$contract_data$token_info) != 0) {
    names(r$contract_data$token_info) <-
      snakecase::to_snake_case(names(r$contract_data$token_info))
    trx_transfer <- ifelse(r$contract_data$token_info$token_id == "_",
                           r$contract_data$amount, 0)
  }

  if ("amount" %in% contract_data_names &
      length(r$contract_data$token_info) == 0) {
    trx_transfer <- r$contract_data$amount
  }





  # TRC-10 amount transfered as part of the main transaction:
  if (all(c("asset_name", "amount") %in% names(r$contract_data))) {
    names(r$contract_data) <-
      snakecase::to_snake_case(names(r$contract_data))
    names(r$contract_data$token_info) <-
      snakecase::to_snake_case(names(r$contract_data$token_info))
    token_info <- r$contract_data$token_info
    null_checker <- function(x) {ifelse(is.null(x), NA, x)}
    trc10_transfer <- tibble::tibble(
      token_id = r$contract_data$asset_name,
      token_name = null_checker(token_info$token_name),
      token_abbr = null_checker(token_info$token_abbr),
      vip = null_checker(token_info$vip),
      amount = r$contract_data$amount,
      token_decimal = null_checker(token_info$token_decimal)
    )
    trc10_transfer <- list(trc10_transfer)
  } else {
    trc10_transfer <- NA
  }

  result <- tibble::tibble(
    request_time = request_time,

    tx_id = r$hash,

    block_number = as.character(r$block),

    timestamp = from_unix_timestamp(r$timestamp),

    contract_result = r$contract_ret,

    confirmed = r$confirmed,

    confirmations_count = r$confirmations,

    contract_type = convert_contract_type_id(r$contract_type),

    from_address = r$owner_address,

    to_address = ifelse(nchar(r$to_address) != 0,
      r$to_address, NA
    ),

    is_contract_from_address = r$contract_map[r$owner_address],

    is_contract_to_address = ifelse(is.na(to_address),
      NA, r$contract_map[r$to_address]
    ),

    costs = list(costs),

    trx_transfer = trx_transfer,

    trc10_transfer = trc10_transfer
  )

  if (add_raw_data) {
    if (length(r$info) == 0) {
      raw_data <- r$contract_data
    } else {
      raw_data <- c(r$contract_data, r$info)
    }

    result <- dplyr::mutate(
      result,
      raw_data = list(raw_data)
    )
  }

  return(result)
}
