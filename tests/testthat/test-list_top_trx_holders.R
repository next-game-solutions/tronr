test_that("list_top_trx_holders returns correct objects", {
  expect_error(list_top_trx_holders("10"))
  expect_error(list_top_trx_holders(c(10, 20)))

  result <- list_top_trx_holders(5)

  expect_s3_class(result, "tbl")
  expect_named(result, c(
    "request_time",
    "address",
    "trx_balance",
    "total_tx",
    "tron_power"
  ))
  expect_equal(nrow(result), 5)
  expect_is(result$request_time, "POSIXct")
  expect_is(result$address, "character")
  expect_is(result$trx_balance, "numeric")
  expect_is(result$total_tx, "integer")
  expect_is(result$tron_power, "numeric")
})
