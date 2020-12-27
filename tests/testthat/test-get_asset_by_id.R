correct_address_id <- "41a71707dfdaed89ffda2fa0125c74be1988d63481"
correct_asset_id <- "1002762"

test_that("get_asset_by_id returns correct objects with basic info", {
  result1 <- get_asset_by_id(
    asset_id = correct_address_id,
    only_confirmed = TRUE,
    detailed_info = FALSE,
    max_attempts = 3L
  )

  result2 <- get_asset_by_id(
    asset_id = correct_asset_id,
    only_confirmed = FALSE,
    detailed_info = FALSE,
    max_attempts = 3L
  )

  expect_s3_class(result1, "tbl")
  expect_s3_class(result2, "tbl")

  expect_equal(names(result1), names(result2))

  expect_named(result1, expected = c(
    "asset_id", "owner_address",
    "abbr", "name", "precision"
  ))

  expect_type(result1$precision, "integer")

  expect_true(substr(result1$owner_address, 1, 1) == "T" &&
    tronr::is_address(result1$owner_address))

  expect_true(substr(result2$owner_address, 1, 1) == "T" &&
    tronr::is_address(result2$owner_address))
})


test_that("get_asset_by_id returns correct objects with detailed info", {
  result1 <- get_asset_by_id(
    asset_id = correct_address_id,
    only_confirmed = TRUE,
    detailed_info = TRUE,
    max_attempts = 3L
  )

  result2 <- get_asset_by_id(
    asset_id = correct_asset_id,
    only_confirmed = FALSE,
    detailed_info = TRUE,
    max_attempts = 3L
  )

  expect_s3_class(result1, "tbl")
  expect_s3_class(result2, "tbl")

  expect_equal(names(result1), names(result2))

  expect_named(result1, expected = c(
    "asset_id", "owner_address", "abbr",
    "name", "description",
    "url", "precision", "total_supply",
    "num", "trx_num", "ico_start_time",
    "ico_end_time"
  ))

  expect_type(result1$precision, "integer")
  expect_s3_class(result1$ico_start_time, "POSIXct")
  expect_s3_class(result1$ico_end_time, "POSIXct")

  expect_true(substr(result1$owner_address, 1, 1) == "T" &&
    tronr::is_address(result1$owner_address))

  expect_true(substr(result2$owner_address, 1, 1) == "T" &&
    tronr::is_address(result2$owner_address))
})
