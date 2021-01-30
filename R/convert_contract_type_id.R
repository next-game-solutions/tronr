#' Convert contract type ID
#'
#' Converts integer-encoded contract type IDs to human-readable names
#'
#' @param id (numeric or character): a vector of numeric or coercible-to-numeric
#'     values to be mapped to `contractType` names. Cannot contain `NA` values.
#'
#' @return A charcter vector with `contractType` names.
#'
#' @details Transaction attributes returned by the Tronscan API contain
#'     `contractType`, an integer-encoded type of transaction. The dictionary
#'     used by this function to map these integers to human-readable names
#'     is taken from the official TRON Protocol
#'     [documentation](https://github.com/tronprotocol/documentation/blob/master/English_Documentation/TRON_Virtual_Machine/TRC10_TRX_TRANSFER_INTRODUCTION_FOR_EXCHANGES.md).
#'
#' @export
#'
#' @examples
#' convert_contract_type_id(c(1, 2, 31))
convert_contract_type_id <- function(id) {
  if (any(is.na(id))) {
    rlang::abort("`id` cannot contain NA values")
  }

  if (!is.numeric(id) && any(is.na(suppressWarnings(as.numeric(id))))) {
    rlang::abort("`id` must be a numeric or coercible-to-numeric value")
  }

  dict <- list(
    `0` = "AccountCreateContract",
    `1` = "TransferContract",
    `2` = "TransferAssetContract",
    `3` = "VoteAssetContract",
    `4` = "VoteWitnessContract",
    `5` = "WitnessCreateContract",
    `6` = "AssetIssueContract",
    `8` = "WitnessUpdateContract",
    `9` = "ParticipateAssetIssueContract",
    `10` = "AccountUpdateContract",
    `11` = "FreezeBalanceContract",
    `12` = "UnfreezeBalanceContract",
    `13` = "WithdrawBalanceContract",
    `14` = "UnfreezeAssetContract",
    `15` = "UpdateAssetContract",
    `16` = "ProposalCreateContract",
    `17` = "ProposalApproveContract",
    `18` = "ProposalDeleteContract",
    `19` = "SetAccountIdContract",
    `20` = "CustomContract",
    `21` = "BuyStorageContract",
    `22` = "BuyStorageBytesContract",
    `23` = "SellStorageContract",
    `30` = "CreateSmartContract",
    `31` = "TriggerSmartContract",
    `32` = "GetContract",
    `33` = "UpdateSettingContract",
    `41` = "ExchangeCreateContract",
    `42` = "ExchangeInjectContract",
    `43` = "ExchangeWithdrawContract",
    `44` = "ExchangeTransactionContract",
    `45` = "UpdateEnergyLimitContract",
    `46` = "AccountPermissionUpdateContract",
    `47` = "PermissionAddKeyContract",
    `48` = "PermissionUpdateKeyContract",
    `49` = "PermissionDeleteKeyContract"
  )

  id <- as.character(id)

  result <- ifelse(id %in% names(dict),
    dict[id], id
  )

  return(as.character(result))
}
