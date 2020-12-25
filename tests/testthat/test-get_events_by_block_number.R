test_that("get_events_by_block_number returns object of correct type", {

  result <- get_events_by_block_number(block_number = "15354550")

  expect_s3_class(result, "tbl")
  expect_equal(nrow(result), 23L)
  expect_equal(ncol(result), 6L)

  expect_true(all(result$block_number == "15354550"))
  expect_s3_class(result$block_timestamp, "POSIXct")
  expect_is(result$event_data, "list")
  expect_named(result$event_data[[1]], c("_minedDice", "_index",
                                         "_bettor", "_multiplier",
                                         "_round", "_amount"))

})
