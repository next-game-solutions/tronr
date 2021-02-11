address <- "TKttnV3FSY1iEoAwB4N52WK2DxdV94KpSd"
min_timestamp <- "1576317786000"
max_timestamp <- "1576317996000"
query_params <- list(
  min_block_timestamp = min_timestamp,
  max_block_timestamp = max_timestamp
)
url <- tronr::build_get_request(
  base_url = "https://api.trongrid.io",
  path = c(
    "v1", "contracts",
    address, "events"
  ),
  query_parameters = query_params
)
r <- tronr::api_request(url = url, max_attempts = 3L)
info <- r$data[[1]]


test_that("parse_events_info throws errors as expected", {
  expect_error(parse_events_info(info = c("a", "b", "c")))
})


test_that("parse_events_info returns correct object", {
  result <- parse_events_info(info = info)

  expect_s3_class(result, "tbl")
  expect_equal(nrow(result), 1L)
  expect_equal(ncol(result), 6L)

  expect_named(result, expected = c(
    "tx_id", "block_number",
    "timestamp", "contract_address",
    "event_name", "event_data"
  ))

  expect_s3_class(result$timestamp, "POSIXct")
  expect_equal(result$contract_address[1], "TKttnV3FSY1iEoAwB4N52WK2DxdV94KpSd")
  expect_equal(result$event_name[1], "Transfer")

  expect_is(result$event_data[[1]], "list")
  expect_equal(
    result$event_data[[1]][[1]],
    "0x1d0f4031f9e3eeeb727b10e462ab0e59ee06a2a6"
  )
  expect_equal(result$event_data[[1]][[3]], "22522")
})
