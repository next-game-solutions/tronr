test_that("get_trx_market_data_for_time_range returns correct objects", {
  from_date <- "1609495210000"
  to_date <- "1609533900000"

  expect_error(get_trx_market_data_for_time_range(
    vs_currency = "abcdef",
    min_timestamp = from_date,
    max_timestamp = to_date
  ))

  result <- get_trx_market_data_for_time_range(
    vs_currency = "usd",
    min_timestamp = from_date,
    max_timestamp = to_date
  )

  expect_named(result, c(
    "timestamp", "vs_currency", "price",
    "total_trading_vol", "market_cap"
  ))

  expect_s3_class(result$timestamp, "POSIXct")
  expect_type(result$vs_currency, "character")
  expect_type(result$price, "double")
  expect_type(result$total_trading_vol, "double")
  expect_type(result$market_cap, "double")
})
