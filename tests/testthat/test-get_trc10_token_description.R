correct_token_name <- "BitTorrent"
correct_token_id <- "1002000"

test_that("get_asset_by_id returns correct objects with basic info", {
  expect_error(get_trc10_token_description(
    token_id = "abcde",
    token_name = NULL,
    detailed_info = FALSE,
    max_attempts = 3L
  ))

  result1 <- get_trc10_token_description(
    token_id = correct_token_id,
    token_name = NULL,
    detailed_info = FALSE,
    max_attempts = 3L
  )

  result2 <- get_trc10_token_description(
    token_id = NULL,
    token_name = correct_token_name,
    detailed_info = FALSE,
    max_attempts = 3L
  )

  expect_s3_class(result1, "tbl")
  expect_s3_class(result2, "tbl")

  expect_equal(names(result1), names(result2))

  expect_named(result1, expected = c(
    "token_id", "token_name",
    "token_abbr", "token_owner_address", "precision"
  ))

  expect_true(substr(result1$token_owner_address, 1, 1) == "T" &&
    tronr::is_address(result1$token_owner_address))

  expect_true(substr(result2$token_owner_address, 1, 1) == "T" &&
    tronr::is_address(result2$token_owner_address))

  expect_equal(result1$token_id, "1002000")
  expect_equal(result1$token_name, "BitTorrent")
  expect_equal(result1$token_abbr, "BTT")
  expect_equal(result1$token_owner_address, "TF5Bn4cJCT6GVeUgyCN4rBhDg42KBrpAjg")
  expect_equal(result1$precision, 6)
})


test_that("get_asset_by_id returns correct objects with detailed info", {
  result1 <- get_trc10_token_description(
    token_id = correct_token_id,
    token_name = NULL,
    detailed_info = TRUE,
    max_attempts = 3L
  )

  result2 <- get_trc10_token_description(
    token_id = NULL,
    token_name = correct_token_name,
    detailed_info = TRUE,
    max_attempts = 3L
  )

  expect_s3_class(result1, "tbl")
  expect_s3_class(result2, "tbl")

  expect_equal(names(result1), names(result2))

  expect_named(result1, expected = c(
    "request_time",
    "token_id",
    "token_name", "token_abbr",
    "token_owner_address",
    "reputation",
    "vip",
    "description",
    "date_created",
    "ico_start_time",
    "ico_end_time",
    "url",
    "github",
    "total_supply",
    "amount_issued",
    "issued_percentage",
    "precision",
    "number_of_holders",
    "total_tx",
    "price_in_trx"
  ))

  expect_s3_class(result1$date_created, "POSIXct")
  expect_s3_class(result1$ico_start_time, "POSIXct")
  expect_s3_class(result1$ico_end_time, "POSIXct")

  expect_true(substr(result1$token_owner_address, 1, 1) == "T" &
    tronr::is_address(result1$token_owner_address))

  expect_true(substr(result2$token_owner_address, 1, 1) == "T" &
    tronr::is_address(result2$token_owner_address))

  expect_equal(result1$token_id, "1002000")
  expect_equal(result1$token_name, "BitTorrent")
  expect_equal(result1$token_abbr, "BTT")
  expect_equal(result1$token_owner_address, "TF5Bn4cJCT6GVeUgyCN4rBhDg42KBrpAjg")
  expect_equal(result1$precision, 6)
  expect_is(result1$amount_issued, "numeric")
  expect_is(result1$issued_percentage, "numeric")
  expect_is(result1$number_of_holders, "integer")
  expect_is(result1$total_tx, "integer")
  expect_is(result1$price_in_trx, "numeric")
})
