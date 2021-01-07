test_that("get_trx_market_data_for_last_n_days returns correct objects", {
  expect_error(get_trx_market_data_for_last_n_days(
    days = 7,
    vs_currency = "abcde"
  ))

  expect_error(get_trx_market_data_for_last_n_days(vs_currency = "eur"))

  expect_error(get_trx_market_data_for_last_n_days(
    days = 7,
    vs_currency = "eur",
    interval = "monthly"
  ))

  result <- get_trx_market_data_for_last_n_days(
    vs_currency = "gbp",
    days = 7
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
