tx_df <- get_tx_info_by_account_address(
  address = "TAUN6FwrnwwmaEqYcckffC7wYmbaS6cBiX",
  only_confirmed = TRUE,
  only_from = TRUE,
  min_timestamp = "1577836800000",
  max_timestamp = "1577838600000",
  limit = 50L
)


test_that("get_tx_info_by_account_address throws errors as expected", {

  # address:
  expect_error(
    get_tx_info_by_account_address(address = as.factor("a"))
  )

  # only_confirmed:
  expect_error(
    get_tx_info_by_account_address(
      address = "TAUN6FwrnwwmaEqYcckffC7wYmbaS6cBiX",
      only_confirmed = 1L)
  )

  # only_unconfirmed:
  expect_error(
    get_tx_info_by_account_address(
      address = "TAUN6FwrnwwmaEqYcckffC7wYmbaS6cBiX",
      only_unconfirmed = 1L)
  )

  # only_to:
  expect_error(
    get_tx_info_by_account_address(
      address = "TAUN6FwrnwwmaEqYcckffC7wYmbaS6cBiX",
      only_to = 1L)
  )

  # only_from:
  expect_error(
    get_tx_info_by_account_address(
      address = "TAUN6FwrnwwmaEqYcckffC7wYmbaS6cBiX",
      only_from = 1L)
  )

  # min_timestamp:
  expect_error(
    get_tx_info_by_account_address(
      address = "TAUN6FwrnwwmaEqYcckffC7wYmbaS6cBiX",
      min_timestamp = TRUE)
  )

  expect_error(
    get_tx_info_by_account_address(
      address = "TAUN6FwrnwwmaEqYcckffC7wYmbaS6cBiX",
      min_timestamp = "abcd")
  )

  # max_timestamp:
  expect_error(
    get_tx_info_by_account_address(
      address = "TAUN6FwrnwwmaEqYcckffC7wYmbaS6cBiX",
      max_timestamp = TRUE)
  )

  expect_error(
    get_tx_info_by_account_address(
      address = "TAUN6FwrnwwmaEqYcckffC7wYmbaS6cBiX",
      max_timestamp = "abcd")
  )

  # limit
  expect_error(
    get_tx_info_by_account_address(
      address = "TAUN6FwrnwwmaEqYcckffC7wYmbaS6cBiX",
      limit = "50")
  )

  # max_attempts
  expect_error(
    get_tx_info_by_account_address(
      address = "TAUN6FwrnwwmaEqYcckffC7wYmbaS6cBiX",
      max_attempts = "50")
  )

  expect_error(
    get_tx_info_by_account_address(
      address = "TAUN6FwrnwwmaEqYcckffC7wYmbaS6cBiX",
      max_attempts = -1L)
  )

})


test_that("get_tx_info_by_account_address returns correct object", {

  expect_equal(nrow(tx_df), 9L)
  expect_equal(ncol(tx_df), 11L)

  expect_named(tx_df, expected = c("address",
                                   "tx_id",
                                   "tx_type",
                                   "tx_result",
                                   "net_usage",
                                   "net_fee",
                                   "energy_usage",
                                   "energy_fee",
                                   "block_number",
                                   "block_timestamp",
                                   "raw_data"))

  expect_true(all(tx_df$tx_type == "TransferContract"))
  expect_true(all(tx_df$tx_result == "SUCCESS"))
  expect_equal(sum(as.numeric(tx_df$net_usage)), 0)
  expect_equal(sum(as.numeric(tx_df$energy_usage)), 0)
  expect_equal(min(as.numeric(tx_df$block_number)), 15860591L)
  expect_equal(max(as.numeric(tx_df$block_number)), 15860895L)

  expect_is(tx_df$raw_data, "list")
  expect_named(tx_df$raw_data[[1]], c("tx_timestamp",
                                      "amount",
                                      "from_address",
                                      "to_address"))

  expect_is(tx_df$block_timestamp, "POSIXct")
  expect_is(tx_df$raw_data[[1]]$tx_timestamp, "POSIXct")

})
