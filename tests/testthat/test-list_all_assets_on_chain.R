test_that("list_all_assets_on_chain throws errors as expected", {

  expect_error(list_all_assets_on_chain(order_by = 1L,
                                        direction = "desc",
                                        max_attempts = 3L))

  expect_error(list_all_assets_on_chain(order_by = "total",
                                        direction = "desc",
                                        max_attempts = 3L))

  expect_error(list_all_assets_on_chain(order_by = "total_supply",
                                        direction = "descending",
                                        max_attempts = 3L))

  expect_error(list_all_assets_on_chain(order_by = "total_supply",
                                        direction = 1L,
                                        max_attempts = 3L))

  expect_error(list_all_assets_on_chain(order_by = "total_supply",
                                        direction = "desc",
                                        max_attempts = -1))
})



test_that("list_all_assets_on_chain returns correct object", {

  result <- list_all_assets_on_chain(order_by = "total_supply",
                                     direction = "desc",
                                     max_attempts = 3L)


  expect_s3_class(result, "tbl")

  expect_named(result, expected = c("request_time", "asset_id",
                                    "owner_address",  "abbr",
                                    "asset_name", "precision",
                                    "description", "url", "total_supply",
                                    "num", "trx_num", "ico_start_time",
                                    "ico_end_time", "vote_score"))

  expect_s3_class(result$request_time, "POSIXct")
  expect_s3_class(result$ico_start_time, "POSIXct")
  expect_s3_class(result$ico_end_time, "POSIXct")
  expect_true(inherits(result$total_supply, "character"))

})

