test_that("parse_tx_info throws an error as expected", {
  expect_error(parse_tx_info(info = 1:3))
})


test_that("parse_tx_info returns correct object for a TRC-20 transaction", {
  id <- "dca447279bc2fd3c10325e442746f9a42938e25bac33bc277b3e7720027aaaf2"

  url <- build_get_request(
    base_url = "https://apilist.tronscan.org/",
    path = c("api", "transaction-info"),
    query_parameters = list(hash = id)
  )

  r <- api_request(url = url, max_attempts = 3L)

  tx_parsed <- parse_tx_info(info = r)

  expect_named(tx_parsed, expected = c(
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
    "info"
  ))

  expect_is(tx_parsed$timestamp, class = "POSIXct")

  expect_is(tx_parsed$block_number, "character")

  expect_is(tx_parsed$sr_confirm_list, "list")

  expect_is(tx_parsed$costs, "list")

  expect_is(tx_parsed$trc20_transfer, "list")

  expect_equal(tx_parsed$trc10_transfer, NA)

  expect_equal(tx_parsed$trx_transfer, 30000)

  expect_is(tx_parsed$internal_tx, "list")

  expect_equal(tx_parsed$info, NA)

  expect_named(tx_parsed$sr_confirm_list[[1]], expected = c(
    "address",
    "name",
    "block_number"
  ))

  expect_named(tx_parsed$trc20_transfer[[1]], expected = c(
    "token_name",
    "token_abbr",
    "token_contract",
    "from_address",
    "to_address",
    "is_contract_from_address",
    "is_contract_to_address",
    "vip",
    "amount"
  ))

  expect_named(tx_parsed$internal_tx[[1]], expected = c(
    "internal_tx_id",
    "from_address",
    "to_address",
    "is_contract_from_address",
    "is_contract_to_address",
    "confirmed",
    "rejected",
    "token_id",
    "token_name",
    "token_abbr",
    "vip",
    "amount"
  ))
})

test_that("parse_tx_info returns correct object for a TransferAssetContract transaction", {
  id <- "e79c8b1d5e394e75a5d89332f62cc582fa95f3bdb6d3cbfd299509f12fa8781d"

  url <- build_get_request(
    base_url = "https://apilist.tronscan.org/",
    path = c("api", "transaction-info"),
    query_parameters = list(hash = id)
  )

  r <- api_request(url = url, max_attempts = 3L)

  tx_parsed <- parse_tx_info(info = r)

  expect_named(tx_parsed, expected = c(
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
    "info"
  ))

  expect_is(tx_parsed$timestamp, class = "POSIXct")

  expect_is(tx_parsed$block_number, "character")

  expect_is(tx_parsed$sr_confirm_list, "list")

  expect_is(tx_parsed$costs, "list")

  expect_equal(tx_parsed$trc20_transfer, NA)

  expect_is(tx_parsed$trc10_transfer, "list")

  expect_equal(tx_parsed$trx_transfer, 0)

  expect_equal(tx_parsed$internal_tx, NA)

  expect_equal(tx_parsed$info, NA)

  expect_named(tx_parsed$sr_confirm_list[[1]], expected = c(
    "address",
    "name",
    "block_number"
  ))

  expect_named(tx_parsed$trc10_transfer[[1]], expected = c(
    "token_id",
    "token_name",
    "token_abbr",
    "vip",
    "amount"
  ))
})
