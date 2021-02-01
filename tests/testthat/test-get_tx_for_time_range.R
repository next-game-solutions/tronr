min_timestamp <- "1577836800000"
max_timestamp <- "1577836803000"
tx_df <- get_tx_for_time_range(min_timestamp, max_timestamp)

test_that("get_tx_info_by_account_address returns correct object", {
  expect_equal(nrow(tx_df), 41L)
  expect_equal(ncol(tx_df), 20L)

  expect_named(tx_df, expected = c(
    "request_time",
    "tx_id",
    "block_number",
    "timestamp",
    "contract_result",
    "confirmed",
    "confirmations_count",
    "sr_confirm_list",
    "contract_type",
    "from_address",
    "to_address",
    "is_contract_from_address",
    "is_contract_to_address",
    "costs",
    "trx_transfer",
    "trc10_transfer",
    "trc20_transfer",
    "internal_tx",
    "info",
    "contract_data"
  ))

  expect_true(all(is.na(tx_df$trc20_transfer)))
  expect_true(all(is.na(tx_df$info)))
  expect_true(all(tx_df$contract_result == "SUCCESS"))
  expect_true(all(tx_df$confirmed))
  expect_true(!all(tx_df$is_contract_from_address))
  expect_true(!all(tx_df$is_contract_to_address))

  expect_equal(round(sum(tx_df$trx_transfer)), 3038)
  expect_equal(min(as.numeric(tx_df$block_number)), 15860581L)
  expect_equal(max(as.numeric(tx_df$block_number)), 15860581L)

  trc10 <- dplyr::bind_rows(tx_df$trc10_transfer)
  expect_equal(nrow(trc10), 2L)
})
