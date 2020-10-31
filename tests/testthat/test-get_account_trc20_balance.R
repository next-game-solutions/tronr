correct_address <- "41a1f60e12be07004934ead89620f1ba0bebbe128e"
correct_address2 <- "TQjaZ9FD473QBTdUzMLmSyoGB6Yz1CGpux"
wrong_address <- "abc"

test_that("get_account_trc20_balance throws errors as expected", {

  expect_error(get_account_trc20_balance(address = 1L,
                                         only_confirmed = FALSE,
                                         max_attempts = 3L))

  expect_error(get_account_trc20_balance(address = correct_address,
                                         only_confirmed = 1L,
                                         max_attempts = 3L))

  expect_error(get_account_trc20_balance(address = correct_address,
                                         only_confirmed = FALSE,
                                         max_attempts = -1))

  expect_error(suppressMessages(
    get_account_trc20_balance(address = wrong_address,
                              only_confirmed = FALSE,
                              max_attempts = 1L)))
})


test_that("get_account_trc20_balance returns correct objects", {

  result <- get_account_trc20_balance(address = correct_address,
                                      only_confirmed = TRUE,
                                      max_attempts = 3L)

  result2 <- get_account_trc20_balance(address = correct_address2,
                                       only_confirmed = TRUE,
                                       max_attempts = 3L)

  expect_s3_class(result, "tbl")
  expect_s3_class(result2, "tbl")

  expect_equal(names(result), names(result2))

  expect_named(result,
               expected = c("request_time", "address",
                            "n_trc20", "trc20_balance"))

  expect_s3_class(result$request_time, "POSIXct")
  expect_type(result$address, "character")
  expect_type(result$trc20_balance, "list")
  expect_s3_class(result$trc20_balance[[1]], "tbl")
  expect_type(result$trc20_balance[[1]]$trc20, "character")
  expect_type(result$trc20_balance[[1]]$balance, "character")

})
