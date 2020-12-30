test_that("get_assets_by_name returns correct object", {
  result <- get_assets_by_name(
    asset_name = "Tronix",
    order_by = "total_supply",
    direction = "desc",
    only_confirmed = FALSE,
    max_attempts = 3L
  )


  expect_s3_class(result, "tbl")

  expect_named(result, expected = c(
    "request_time", "asset_id",
    "owner_address", "abbr",
    "asset_name", "precision",
    "description", "url", "total_supply",
    "num", "trx_num", "ico_start_time",
    "ico_end_time", "vote_score"
  ))

  expect_s3_class(result$request_time, "POSIXct")
  expect_type(result$asset_id, "character")
  expect_type(result$owner_address, "character")
  expect_type(result$abbr, "character")
  expect_type(result$asset_name, "character")
  expect_type(result$precision, "double")
  expect_type(result$description, "character")
  expect_type(result$url, "character")
  expect_type(result$num, "double")
  expect_type(result$trx_num, "double")
  expect_type(result$total_supply, "double")
  expect_s3_class(result$ico_start_time, "POSIXct")
  expect_s3_class(result$ico_end_time, "POSIXct")

  expect_true(substr(result$owner_address[1], 1, 1) == "T" &&
                tronr::is_address(result$owner_address[1]))
})
