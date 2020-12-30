test_that("list_all_assets_on_chain returns correct object", {
  result <- list_all_assets_on_chain(
    order_by = "total_supply",
    direction = "desc",
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
  expect_type(result$total_supply, "double")
})
