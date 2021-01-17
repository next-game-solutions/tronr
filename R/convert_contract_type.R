convert_contract_type <- function(id) {

  if (any(is.na(id))) {
    rlang::abort("`id` cannot contain NA values")
  }

  if (!is.numeric(id) && is.na(suppressWarnings(as.numeric(id)))) {
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
                   dict[id], NA_character_)

  return(as.character(result))

}
