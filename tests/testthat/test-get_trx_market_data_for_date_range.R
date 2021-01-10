test_that("get_trx_market_data_for_date_range returns correct objects", {
  from_date <- as.POSIXct(Sys.Date() - 1 - 14, tz = "UTC")
  to_date <- as.POSIXct(Sys.Date() - 1, tz = "UTC")

  expect_error(get_trx_market_data_for_date_range(
    vs_currency = "abcdef",
    from_date = from_date,
    to_date = to_date
  ))

  expect_error(get_trx_market_data_for_date_range(
    vs_currency = "usd",
    from_date = "from_date",
    to_date = to_date
  ))

  expect_error(get_trx_market_data_for_date_range(
    vs_currency = "usd",
    from_date = from_date,
    to_date = "to_date"
  ))

  result <- get_trx_market_data_for_date_range(
    vs_currency = "usd",
    from_date = from_date,
    to_date = to_date
  )

  expect_named(result, c(
    "datetime", "vs_currency", "price",
    "total_trading_vol", "market_cap"
  ))

  expect_s3_class(result$datetime, "POSIXct")
  expect_type(result$vs_currency, "character")
  expect_type(result$price, "double")
  expect_type(result$total_trading_vol, "double")
  expect_type(result$market_cap, "double")

})
