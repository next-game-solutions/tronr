tx_df <- get_trc20_tx_info_by_account_address(
  address = "TAUN6FwrnwwmaEqYcckffC7wYmbaS6cBiX",
  min_timestamp = "1604188800000",
  max_timestamp = "1604189100000",
  limit = 20L)


test_that("get_trc20_tx_info_by_account_address throws errors as expected", {

  # address:
  expect_error(
    get_trc20_tx_info_by_account_address(address = as.factor("a"))
  )

  # only_confirmed:
  expect_error(
    get_trc20_tx_info_by_account_address(
      address = "TAUN6FwrnwwmaEqYcckffC7wYmbaS6cBiX",
      only_confirmed = 1L)
  )

  # only_unconfirmed:
  expect_error(
    get_trc20_tx_info_by_account_address(
      address = "TAUN6FwrnwwmaEqYcckffC7wYmbaS6cBiX",
      only_unconfirmed = 1L)
  )

  # only_to:
  expect_error(
    get_trc20_tx_info_by_account_address(
      address = "TAUN6FwrnwwmaEqYcckffC7wYmbaS6cBiX",
      only_to = 1L)
  )

  # only_from:
  expect_error(
    get_trc20_tx_info_by_account_address(
      address = "TAUN6FwrnwwmaEqYcckffC7wYmbaS6cBiX",
      only_from = 1L)
  )

  # min_timestamp:
  expect_error(
    get_trc20_tx_info_by_account_address(
      address = "TAUN6FwrnwwmaEqYcckffC7wYmbaS6cBiX",
      min_timestamp = TRUE)
  )

  expect_error(
    get_trc20_tx_info_by_account_address(
      address = "TAUN6FwrnwwmaEqYcckffC7wYmbaS6cBiX",
      min_timestamp = "abcd")
  )

  # max_timestamp:
  expect_error(
    get_trc20_tx_info_by_account_address(
      address = "TAUN6FwrnwwmaEqYcckffC7wYmbaS6cBiX",
      max_timestamp = TRUE)
  )

  expect_error(
    get_trc20_tx_info_by_account_address(
      address = "TAUN6FwrnwwmaEqYcckffC7wYmbaS6cBiX",
      max_timestamp = "abcd")
  )

  # limit
  expect_error(
    get_trc20_tx_info_by_account_address(
      address = "TAUN6FwrnwwmaEqYcckffC7wYmbaS6cBiX",
      limit = "50")
  )

  # max_attempts
  expect_error(
    get_trc20_tx_info_by_account_address(
      address = "TAUN6FwrnwwmaEqYcckffC7wYmbaS6cBiX",
      max_attempts = "50")
  )

  # contract_address
  expect_error(
    get_trc20_tx_info_by_account_address(
      address = "TAUN6FwrnwwmaEqYcckffC7wYmbaS6cBiX",
      contract_address = 1L)
  )

  expect_error(
    get_trc20_tx_info_by_account_address(
      address = "TAUN6FwrnwwmaEqYcckffC7wYmbaS6cBiX",
      max_attempts = -1L)
  )

})



test_that("get_trc20_tx_info_by_account_address returns correct object", {

  expect_is(tx_df, "tbl")

  expect_equal(nrow(tx_df), 9L)
  expect_equal(ncol(tx_df), 10L)

  expect_named(tx_df, c("tx_id",
                        "tx_type",
                        "block_timestamp",
                        "from_address",
                        "to_address",
                        "trc20_symbol",
                        "trc20_name",
                        "trc20_contract_address",
                        "precision",
                        "amount"))

  expect_is(tx_df$block_timestamp, "POSIXct")

  expect_equal(tx_df$amount[1], "4588373258")
  expect_equal(tx_df$from_address[1],
               "TAUN6FwrnwwmaEqYcckffC7wYmbaS6cBiX")
  expect_equal(tx_df$to_address[1],
               "TXapQNRb5ZAssLsVDgDTBGEC8XNQPbXMaA")

})
