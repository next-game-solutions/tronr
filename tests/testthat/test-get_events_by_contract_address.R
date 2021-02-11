address <- "TKttnV3FSY1iEoAwB4N52WK2DxdV94KpSd"
min_timestamp <- "1576317786000"
max_timestamp <- "1576317996000"

result1 <- get_events_by_contract_address(
  address = address,
  min_timestamp = min_timestamp,
  max_timestamp = max_timestamp
)

result2 <- get_events_by_contract_address(
  address = address,
  min_timestamp = min_timestamp,
  max_timestamp = max_timestamp,
  event_name = "Transfer"
)

test_that("get_events_by_contract_address returns correct object", {
  expect_s3_class(result1, "tbl")
  expect_s3_class(result2, "tbl")

  expect_equal(names(result1), names(result2))
  expect_named(result1, c(
    "tx_id", "block_number", "timestamp",
    "contract_address", "event_name", "event_data"
  ))

  expect_equal(ncol(result1), 6L)
  expect_equal(nrow(result1), 17L)

  expect_equal(ncol(result2), 6L)
  expect_equal(nrow(result2), 12L)

  expect_s3_class(result1$timestamp, "POSIXct")
  expect_s3_class(result2$timestamp, "POSIXct")

  expect_is(result1$event_data, "list")
  expect_is(result2$event_data, "list")

  expect_true(all(result2$event_name == "Transfer"))
})
