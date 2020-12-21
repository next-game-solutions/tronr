tx_id <- "270e992bf22a271008032ec09a51616ed963b74f4d808b0fd74fc82341a81391"
r <- get_events_by_tx_id(tx_id)

test_that("get_event_by_tx_id throws errors as exected", {

  expect_error(get_events_by_tx_id(tx_id = 1L,
                                   only_confirmed = NULL,
                                   only_unconfirmed = NULL,
                                   max_attempts = 3L))

  expect_error(get_events_by_tx_id(tx_id = tx_id,
                                   only_confirmed = "TRUE",
                                   only_unconfirmed = NULL,
                                   max_attempts = 3L))

  expect_error(get_events_by_tx_id(tx_id = tx_id,
                                   only_confirmed = NULL,
                                   only_unconfirmed = "TRUE",
                                   max_attempts = 3L))

  expect_error(get_events_by_tx_id(tx_id = tx_id,
                                   only_confirmed = NULL,
                                   only_unconfirmed = NULL,
                                   max_attempts = "3"))

  expect_error(get_events_by_tx_id(tx_id = tx_id,
                                   only_confirmed = TRUE,
                                   only_unconfirmed = FALSE,
                                   max_attempts = 3L))

})


test_that("get_event_by_tx_id return correct object", {

  expect_s3_class(r, "tbl")
  expect_s3_class(r$event_data[[1]], "tbl")

  expect_named(r, expected = c("tx_id",
                               "block_number",
                               "block_timestamp",
                               "contract_address",
                               "event_name",
                               "event_data"))

  expect_equal(nrow(r), 5L)
  expect_equal(ncol(r), 6L)

  expect_equal(nrow(r$event_data[[1]]), 1L)
  expect_equal(ncol(r$event_data[[1]]), 3L)

  expect_s3_class(r$block_timestamp, "POSIXct")

  expect_equal(r$event_data[[1]]$customerAddress,
               "TJUKKvt67SsbjuXBycdEoQAv3YkwMNMWLp")
  expect_equal(r$event_data[[1]]$referralAddress,
               "TBh8fAj5R5o3HNfnjXusamFLMBY9azACnq")
  expect_type(r$event_data[[1]]$tron, "character")

})
