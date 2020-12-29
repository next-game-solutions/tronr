correct_address <- "41a1f60e12be07004934ead89620f1ba0bebbe128e"
correct_address2 <- "TQjaZ9FD473QBTdUzMLmSyoGB6Yz1CGpux"

test_that("get_account_trx_balance returns correct objects", {
  result <- get_account_trx_balance(
    address = correct_address,
    only_confirmed = TRUE,
    max_attempts = 3L
  )

  result2 <- get_account_trx_balance(
    address = correct_address2,
    only_confirmed = TRUE,
    max_attempts = 3L
  )

  expect_s3_class(result, "tbl")
  expect_s3_class(result2, "tbl")

  expect_equal(names(result), names(result2))

  expect_named(result,
    expected = c("request_time", "address", "trx_balance")
  )

  expect_s3_class(result$request_time, "POSIXct")
  expect_type(result$address, "character")
  expect_type(result$trx_balance, "character")

  expect_true(substr(result$address, 1, 1) == "T" &&
    tronr::is_address(result$address))
})
