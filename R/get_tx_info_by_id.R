get_tx_info_by_id <- function(id,
                              add_contract_data = TRUE,
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

  contract_data_names <-
    names(r$contract_data) <-
    snakecase::to_snake_case(names(r$contract_data))

  null_checker <- function(x) {ifelse(is.null(x), NA, x)}


  # Core info:
  request_time = request_time

  tx_id = r$hash

  block_number = as.character(r$block)

  timestamp = from_unix_timestamp(r$timestamp)

  contract_result = r$contract_ret

  confirmed = r$confirmed

  confirmations_count = r$confirmations

  contract_type = convert_contract_type_id(r$contract_type)

  from_address = r$owner_address

  to_address = ifelse(nchar(r$to_address) != 0, r$to_address, NA)

  is_contract_from_address = unlist(r$contract_map[r$owner_address])

  is_contract_to_address = ifelse(is.na(to_address),
                                  NA,
                                  unlist(r$contract_map[r$to_address])
  )

  costs <- list(tibble::as_tibble(r$cost))


  # TransferAssetContract - used ONLY to transfer TRC-10 tokens:
  if (contract_type == "TransferAssetContract") {

    trx_transfer <- 0
    trc20_transfer <- NA

    if ("token_info" %in% contract_data_names &&
        length(r$contract_data$token_info) != 0) {

      token_info <- r$contract_data$token_info
      names(token_info) <- snakecase::to_snake_case(names(token_info))

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

  }


  # TransferContract - used ONLY to transfer TRX:
  if (contract_type == "TransferContract") {
    trx_transfer <- as.numeric(r$contract_data$amount)
    trc10_transfer <- NA
    trc20_transfer <- NA
  }


  # TriggerSmartContract:
  if (contract_type == "TriggerSmartContract") {

    trx_transfer <- ifelse(!"call_value" %in% names(r$contract_data) ||
                             nchar(r$contract_data$call_value) == 0,
                           0,
                           r$contract_data$call_value)

    trc10_transfer <- NA

    if (length(r$trc_20_transfer_info) != 0) {

      trc20_transfer <- lapply(r$trc_20_transfer_info, function(x){
        tibble::tibble(
          token_name = x$name,
          token_abbr = x$symbol,
          token_contract = x$contract_address,
          from_address = x$from_address,
          to_address = x$to_address,
          vip = x$vip,
          amount = as.numeric(x[grepl(pattern = "amount", names(x))]),
          token_decimal = x$decimals
        )
      }) %>%
        dplyr::bind_rows()

      trc20_transfer <- list(trc20_transfer)

    } else {
      trc20_transfer <- NA
    }

  }


  if (!contract_type %in% c("TransferAssetContract",
                            "TransferContract",
                            "TriggerSmartContract")) {
    trx_transfer <- 0
    trc10_transfer <- NA
    trc20_transfer <- NA

  }


  if (is.list(r$internal_transactions) & length(r$internal_transactions) != 0) {

    int_tx <- r$internal_transactions %>% unlist(., recursive = FALSE)

    int_tx <- lapply(int_tx, function(x){
      tibble::tibble(
        internal_tx_id = x$hash,
        contract = x$contract,
        block = x$block,
        from_address = x$caller_address,
        to_address = x$transfer_to_address,
        confirmed = x$confirmed,
        rejected = x$rejected,
        token_id = x$token_list[[1]]$token_id,
        token_name = x$token_list[[1]]$tokenInfo$tokenName,
        token_abbr = x$token_list[[1]]$tokenInfo$tokenAbbr,
        vip = x$token_list[[1]]$tokenInfo$vip,
        amount = x$token_list[[1]]$call_value,
        token_decimal = x$token_list[[1]]$tokenInfo$tokenDecimal
      )
    }) %>%
      dplyr::bind_rows() %>%
      dplyr::mutate(token_id = ifelse(token_id == "_",
                                      "TRX", token_id),
                    token_abbr = ifelse(token_abbr == "trx",
                                        "TRX", token_abbr),
                    token_name = ifelse(token_name == "trx",
                                        "TRX", token_name))

    int_tx <- list(int_tx)

  } else {
    int_tx <- NA
  }


  if (is.list(r$info) & length(r$info) != 0) {
    info <- r$info
  } else {
    info <- NA
  }


  result <- tibble::tibble(
    request_time,
    tx_id,
    block_number,
    timestamp,
    contract_result,
    confirmed,
    confirmations_count,
    contract_type,
    from_address,
    to_address,
    is_contract_from_address,
    is_contract_to_address,
    costs,
    trx_transfer,
    trc10_transfer,
    trc20_transfer,
    int_tx,
    info = info
  )

  if (add_contract_data) {

    result <- dplyr::mutate(
      result,
      contract_data = list(r$contract_data)
    )

  }

  return(result)

}
