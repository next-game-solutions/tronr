correct_address <- "41a1f60e12be07004934ead89620f1ba0bebbe128e"
correct_address2 <- "TQjaZ9FD473QBTdUzMLmSyoGB6Yz1CGpux"
wrong_address <- "abc"

test_that("get_account_trc10_balance throws errors as expected", {

  expect_error(get_account_trc10_balance(address = 1L,
                                         only_confirmed = FALSE,
                                         detailed_trc10_info = FALSE,
                                         max_attempts = 3L))

  expect_error(get_account_trc10_balance(address = correct_address,
                                         only_confirmed = 1L,
                                         detailed_trc10_info = FALSE,
                                         max_attempts = 3L))

  expect_error(get_account_trc10_balance(address = correct_address,
                                         only_confirmed = FALSE,
                                         detailed_trc10_info = 1L,
                                         max_attempts = 3L))
  expect_error(get_account_trc10_balance(address = correct_address,
                                         only_confirmed = FALSE,
                                         detailed_trc10_info = FALSE,
                                         max_attempts = -1L))

  expect_error(suppressMessages(
    get_account_trc10_balance(address = wrong_address,
                              only_confirmed = FALSE,
                              detailed_trc10_info = FALSE,
                              max_attempts = 1L)))
})


test_that("get_account_trc10_balance returns correct objects with basic info", {

  result <- get_account_trc10_balance(address = correct_address,
                                      only_confirmed = TRUE,
                                      detailed_trc10_info = FALSE,
                                      max_attempts = 3L)

  result2 <- get_account_trc10_balance(address = correct_address2,
                                       only_confirmed = TRUE,
                                       detailed_trc10_info = FALSE,
                                       max_attempts = 3L)

  expect_s3_class(result, "tbl")
  expect_s3_class(result2, "tbl")

  expect_equal(names(result), names(result2))

  expect_named(result,
               expected = c("request_time", "address",
                            "n_trc10", "trc10_balance"))

  expect_s3_class(result$request_time, "POSIXct")
  expect_type(result$address, "character")
  expect_type(result$trc10_balance, "list")
  expect_s3_class(result$trc10_balance[[1]], "tbl")
  expect_equal(ncol(result$trc10_balance[[1]]), 6L)

  expect_named(result$trc10_balance[[1]],
               expected = c("asset_id", "owner_address", "abbr",
                            "name", "precision", "balance"))
})


test_that("get_account_trc10_balance returns correct objects with extended info", {

  result <- get_account_trc10_balance(address = correct_address,
                                      only_confirmed = TRUE,
                                      detailed_trc10_info = TRUE,
                                      max_attempts = 3L)

  result2 <- get_account_trc10_balance(address = correct_address2,
                                       only_confirmed = TRUE,
                                       detailed_trc10_info = TRUE,
                                       max_attempts = 3L)

  expect_s3_class(result, "tbl")
  expect_s3_class(result2, "tbl")

  expect_equal(names(result), names(result2))

  expect_named(result,
               expected = c("request_time", "address",
                            "n_trc10", "trc10_balance"))

  expect_s3_class(result$request_time, "POSIXct")
  expect_type(result$address, "character")
  expect_type(result$trc10_balance, "list")
  expect_s3_class(result$trc10_balance[[1]], "tbl")
  expect_equal(ncol(result$trc10_balance[[1]]), 13L)

  expect_named(result$trc10_balance[[1]],
               expected = c("asset_id", "owner_address", "abbr",
                            "name", "description", "url", "precision",
                            "total_supply", "num", "trx_num",
                            "ico_start_time", "ico_end_time", "balance"))

  expect_true(substr(result$address, 1, 1) == "T" &&
                tronr::is_address(result$address))
})

