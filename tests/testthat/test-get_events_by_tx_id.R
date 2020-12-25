tx_id <- "270e992bf22a271008032ec09a51616ed963b74f4d808b0fd74fc82341a81391"
r <- get_events_by_tx_id(tx_id)

test_that("get_event_by_tx_id return correct object", {

  expect_s3_class(r, "tbl")
  expect_is(r$event_data[[1]], "list")

  expect_named(r, expected = c("tx_id",
                               "block_number",
                               "block_timestamp",
                               "contract_address",
                               "event_name",
                               "event_data"))

  expect_equal(nrow(r), 5L)
  expect_equal(ncol(r), 6L)
  expect_equal(length(r$event_data[[1]]), 6L)
  expect_s3_class(r$block_timestamp, "POSIXct")

})
