#' Parse transaction attributes
#'
#' Converts a list with transaction attributes into a nested tibble
#'
#' @param info (list): a non-empty, named list returned as a result of calling the
#'     [`/api/transaction-info`](https://github.com/tronscan/tronscan-frontend/blob/dev2019/document/api.md)
#'     endpoint of the Tronscan API.
#'
#' @return A nested tibble with one row and several columns that contain
#'     transaction attributes. See [`get_tx_info_by_id()`] for details.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @keywords internal
#'
parse_tx_info <- function(info) {
  if (!is.list(info) || length(names(info)) == 0) {
    rlang::abort("`info` must be a named list")
  }

  null_checker <- function(x) {
    ifelse(!is.null(x), x, NA)
  }

  info_names <- names(info)

  # Core info:
  tx_id <- info$hash

  block_number <- as.character(info$block)

  timestamp <- from_unix_timestamp(info$timestamp)

  contract_result <- info$contractRet

  confirmed <- info$confirmed

  confirmations_count <- info$confirmations

  contract_type <- convert_contract_type_id(info$contractType)

  from_address <- info$ownerAddress

  to_address <- ifelse(nchar(info$toAddress) != 0, info$toAddress, NA)

  is_contract_from_address <- unlist(info$contract_map[info$ownerAddress])

  is_contract_to_address <- ifelse(is.na(to_address),
    to_address,
    unlist(info$contract_map[info$toAddress])
  )

  costs <- list(tibble::as_tibble(info$cost))

  sr_confirm_list <- lapply(info$srConfirmList, function(x) {
    tibble::as_tibble(x) %>%
      dplyr::mutate(block = as.character(.data$block)) %>%
      dplyr::rename(block_number = .data$block) %>%
      dplyr::select(-.data$url)
  }) %>%
    dplyr::bind_rows() %>%
    list()

  # TransferAssetContract - used only to transfer TRC-10 tokens:
  if (contract_type == "TransferAssetContract") {
    trx_transfer <- 0

    if ("tokenInfo" %in% names(info$contractData) &
      is.list(info$contractData$tokenInfo) &
      length(info$contractData$tokenInfo) != 0) {
      token_info <- info$contractData$tokenInfo

      trc10_transfer <- tibble::tibble(
        token_id = as.character(info$contractData$asset_name),
        token_name = null_checker(token_info$tokenName),
        token_abbr = null_checker(token_info$tokenAbbr),
        vip = null_checker(token_info$vip),
        amount = apply_decimal(
          info$contractData$amount,
          token_info$tokenDecimal
        )
      )

      trc10_transfer <- list(trc10_transfer)
    } else {
      trc10_transfer <- NA
    }
  }


  # TransferContract - used only to transfer TRX:
  if (contract_type == "TransferContract") {
    trx_transfer <- apply_decimal(as.numeric(info$contractData$amount), 6)
    trc10_transfer <- NA
  }


  # TriggerSmartContract:
  if (contract_type == "TriggerSmartContract") {
    trx_transfer <- ifelse(!"call_value" %in% names(info$contractData) ||
      nchar(info$contractData$call_value) == 0,
    0,
    apply_decimal(as.numeric(info$contractData$call_value), 6)
    )

    trc10_transfer <- NA
  }


  if (!contract_type %in% c(
    "TransferAssetContract",
    "TransferContract",
    "TriggerSmartContract"
  )
  ) {
    trx_transfer <- 0
    trc10_transfer <- NA
  }


  if ("trc20TransferInfo" %in% names(info) &
    is.list(info$trc20TransferInfo) &
    length(info$trc20TransferInfo) != 0) {
    trc20_transfer <- lapply(info$trc20TransferInfo, function(x) {
      tibble::tibble(
        token_name = x$name,
        token_abbr = x$symbol,
        token_contract = x$contract_address,
        from_address = x$from_address,
        to_address = x$to_address,
        is_contract_from_address = unlist(info$contract_map[x$from_address]),
        is_contract_to_address = unlist(info$contract_map[x$to_address]),
        vip = x$vip,
        amount = apply_decimal(
          as.numeric(x[grepl("amount", names(x))]),
          x$decimals
        )
      )
    }) %>%
      dplyr::bind_rows()

    trc20_transfer <- list(trc20_transfer)
  } else {
    trc20_transfer <- NA
  }


  if ("internal_transactions" %in% names(info) &
    is.list(info$internal_transactions) &
    length(info$internal_transactions) != 0) {
    int_tx <- unlist(info$internal_transactions, recursive = FALSE)

    internal_tx <- lapply(int_tx, function(x) {
      tibble::tibble(
        internal_tx_id = x$hash,
        from_address = x$caller_address,
        to_address = x$transfer_to_address,
        is_contract_from_address = unlist(info$contract_map[x$caller_address]),

        is_contract_to_address = unlist(info$contract_map[x$transfer_to_address]),
        confirmed = x$confirmed,
        rejected = x$rejected,
        token_id = x$token_list[[1]]$token_id,
        token_name = x$token_list[[1]]$tokenInfo$tokenName,
        token_abbr = x$token_list[[1]]$tokenInfo$tokenAbbr,
        vip = x$token_list[[1]]$tokenInfo$vip,
        amount = apply_decimal(
          x$token_list[[1]]$call_value,
          x$token_list[[1]]$tokenInfo$tokenDecimal
        )
      )
    }) %>%
      dplyr::bind_rows() %>%
      dplyr::mutate(
        token_id = ifelse(.data$token_id == "_",
          "TRX", .data$token_id
        ),
        token_abbr = ifelse(.data$token_abbr == "trx",
          "TRX", .data$token_abbr
        ),
        token_name = ifelse(.data$token_name == "trx",
          "Tronix", .data$token_name
        )
      )

    internal_tx <- list(internal_tx)
  } else {
    internal_tx <- NA
  }


  if (is.list(info$info) & length(info$info) != 0) {
    tx_info <- list(info$info)
  } else {
    tx_info <- NA
  }


  result <- tibble::tibble(
    tx_id,
    block_number,
    timestamp,
    contract_result,
    confirmed,
    confirmations_count,
    sr_confirm_list,
    contract_type,
    from_address,
    to_address,
    is_contract_from_address,
    is_contract_to_address,
    costs,
    trx_transfer,
    trc10_transfer,
    trc20_transfer,
    internal_tx,
    info = tx_info
  )

  return(result)
}
