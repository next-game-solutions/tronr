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
  expect_s3_class(result$ico_start_time, "POSIXct")
  expect_s3_class(result$ico_end_time, "POSIXct")
  expect_true(inherits(result$total_supply, "character"))
})
