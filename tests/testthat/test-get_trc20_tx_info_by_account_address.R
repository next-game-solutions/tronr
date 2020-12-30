tx_df <- get_trc20_tx_info_by_account_address(
  address = "TAUN6FwrnwwmaEqYcckffC7wYmbaS6cBiX",
  min_timestamp = "1604188800000",
  max_timestamp = "1604189100000"
)

test_that("get_trc20_tx_info_by_account_address returns correct object", {
  expect_is(tx_df, "tbl")

  expect_equal(nrow(tx_df), 9L)
  expect_equal(ncol(tx_df), 11L)

  expect_named(tx_df, c(
    "address",
    "tx_id",
    "tx_type",
    "block_timestamp",
    "from_address",
    "to_address",
    "trc20_symbol",
    "trc20_name",
    "trc20_contract_address",
    "precision",
    "amount"
  ))

  expect_is(tx_df$block_timestamp, "POSIXct")

  expect_equal(tx_df$amount[1], 4588373258)
  expect_equal(
    tx_df$from_address[1],
    "TAUN6FwrnwwmaEqYcckffC7wYmbaS6cBiX"
  )
  expect_equal(
    tx_df$to_address[1],
    "TXapQNRb5ZAssLsVDgDTBGEC8XNQPbXMaA"
  )
})
