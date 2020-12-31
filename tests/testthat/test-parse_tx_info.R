query_params <- list(
  only_confirmed = tolower(TRUE),
  only_to = tolower(TRUE),
  min_timestamp = "1577836800000",
  max_timestamp = "1577838600000",
  limit = 100L
)
address <- "TAUN6FwrnwwmaEqYcckffC7wYmbaS6cBiX"
url <- tronr::build_get_request(
  base_url = "https://api.trongrid.io",
  path = c(
    "v1", "accounts",
    address, "transactions"
  ),
  query_parameters = query_params
)
r <- tronr::api_request(url = url, max_attempts = 3L)
tx_info <- r$data[[1]]


test_that("parse_tx_info throws an error as expected", {
  expect_error(tronr::parse_tx_info(info = 1:3))
})


test_that("parse_tx_info returns an object of correct type", {
  tx_parsed <- tronr::parse_tx_info(info = tx_info)

  expect_named(tx_parsed, expected = c(
    "tx_id",
    "tx_type",
    "tx_result",
    "net_usage",
    "net_fee",
    "energy_usage",
    "energy_fee",
    "block_number",
    "block_timestamp",
    "raw_data"
  ))

  expect_is(tx_parsed$block_timestamp, class = "POSIXct")

  expect_named(tx_parsed$raw_data[[1]], expected = c(
    "tx_timestamp",
    "amount",
    "from_address",
    "to_address"
  ))


  expect_is(tx_parsed$raw_data[[1]]$tx_timestamp, class = "POSIXct")
  expect_type(tx_parsed$tx_id, "character")
  expect_type(tx_parsed$tx_type, "character")
  expect_type(tx_parsed$tx_result, "character")
  expect_type(tx_parsed$net_usage, "double")
  expect_type(tx_parsed$net_fee, "double")
  expect_type(tx_parsed$energy_usage, "double")
  expect_type(tx_parsed$energy_fee, "double")
  expect_type(tx_parsed$block_number, "character")
  expect_type(tx_parsed$block_timestamp, "double")
  expect_is(tx_parsed$raw_data, class = "list")
  expect_is(tx_parsed$raw_data[[1]], class = "tbl")
})
