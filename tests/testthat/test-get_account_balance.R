correct_address <- "41a1f60e12be07004934ead89620f1ba0bebbe128e"
correct_address2 <- "TQjaZ9FD473QBTdUzMLmSyoGB6Yz1CGpux"

test_that("get_account_balance returns correct objects", {
  result <- get_account_balance(
    address = correct_address,
    max_attempts = 3L
  )

  result2 <- get_account_balance(
    address = correct_address2,
    max_attempts = 3L
  )

  expect_s3_class(result, "tbl")
  expect_s3_class(result2, "tbl")

  expect_equal(names(result), names(result2))

  expect_named(result, c(
    "request_time", "address", "name",
    "total_transaction_count", "bandwidth", "trx_balance",
    "n_trc20", "trc20", "n_trc10", "trc10"
  ))

  expect_s3_class(result$request_time, "POSIXct")
  expect_type(result$address, "character")
  expect_type(result$name, "character")
  expect_type(result$total_transaction_count, "integer")
  expect_type(result$bandwidth, "list")
  expect_type(result$trx_balance, "double")
  expect_type(result$n_trc20, "integer")
  expect_type(result$trc20, "list")
  expect_s3_class(result$trc20[[1]], "tbl")
  expect_type(result$n_trc10, "integer")
  expect_type(result$trc10, "list")
  expect_s3_class(result$trc10[[1]], "tbl")

  expect_true(substr(result$address, 1, 1) == "T" &&
    tronr::is_address(result$address))
})
