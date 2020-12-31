query_params <- list(
  min_timestamp = "1604188800000",
  max_timestamp = "1604189100000",
  limit = 20L
)
address <- "TAUN6FwrnwwmaEqYcckffC7wYmbaS6cBiX"
url <- tronr::build_get_request(
  base_url = "https://api.trongrid.io",
  path = c(
    "v1", "accounts",
    address, "transactions", "trc20"
  ),
  query_parameters = query_params
)
r <- tronr::api_request(url = url, max_attempts = 3L)
tx_info <- r$data[[1]]
tx_info_parsed <- parse_trc20_tx_info(tx_info)


test_that("parse_trc20_tx_info throws an error as expected", {
  expect_error(parse_trc20_tx_info(1:3))
})


test_that("parse_trc20_tx_info returns correct object", {
  expect_is(tx_info_parsed, "tbl")

  expect_equal(nrow(tx_info_parsed), 1L)
  expect_equal(ncol(tx_info_parsed), 10L)

  expect_named(tx_info_parsed, c(
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

  expect_is(tx_info_parsed$block_timestamp, "POSIXct")

  expect_equal(tx_info_parsed$amount, 4588373258)
  expect_equal(
    tx_info_parsed$from_address,
    "TAUN6FwrnwwmaEqYcckffC7wYmbaS6cBiX"
  )
  expect_equal(
    tx_info_parsed$to_address,
    "TXapQNRb5ZAssLsVDgDTBGEC8XNQPbXMaA"
  )
})
