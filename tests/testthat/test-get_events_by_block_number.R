test_that("get_events_by_block_number returns object of correct type", {

  result <- get_events_of_latest_block(only_confirmed = TRUE,
                                       max_attempts = 3L)


  expect_true(inherits(result, "tbl") | is.null(result))

  if (inherits(result, "tbl")) {

    expect_equal(ncol(result), 6L)
    expect_s3_class(result$block_timestamp, "POSIXct")
    expect_true(tronr::is_address(result$contract_address[1]))
    expect_is(result$event_data, "list")
    expect_is(result$event_data[1], "list")
    expect_is(result$event_data[[1]], "list")

  }

})
