test_that("get_tx_info_by_id returns correct object", {

  # This is a minimal set of tests for this function as most of the other bits
  # are being tested in test-parse_tx_info.R

  id <- "dca447279bc2fd3c10325e442746f9a42938e25bac33bc277b3e7720027aaaf2"

  expect_error(get_tx_info_by_id(
    tx_id = 123345,
    add_contract_data = TRUE,
    max_attempts = 3L
  ))

  expect_error(get_tx_info_by_id(
    tx_id = id,
    add_contract_data = "TRUE",
    max_attempts = 3L
  ))

  expect_error(get_tx_info_by_id(
    tx_id = id,
    add_contract_data = TRUE,
    max_attempts = "3"
  ))

  result <- get_tx_info_by_id(
    tx_id = id,
    add_contract_data = TRUE,
    max_attempts = 3L
  )

  expect_named(result, expected = c(
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

  expect_s3_class(result$request_time, "POSIXct")

  expect_is(result$contract_data, "list")
})
