test_that("get_trx_ohlc_data_for_last_n_days returns correct objects", {
  expect_error(get_trx_ohlc_data_for_last_n_days(
    days = 7,
    vs_currency = "abcde"
  ))

  expect_error(get_trx_ohlc_data_for_last_n_days(vs_currency = "eur"))

  result <- get_trx_ohlc_data_for_last_n_days(
    vs_currency = "eur",
    days = 7
  )

  expect_named(result, c(
    "datetime", "vs_currency", "price_open",
    "price_high", "price_low", "price_close"
  ))

  expect_s3_class(result$datetime, "POSIXct")
  expect_type(result$vs_currency, "character")
  expect_type(result$price_open, "double")
  expect_type(result$price_high, "double")
  expect_type(result$price_low, "double")
  expect_type(result$price_close, "double")
})
