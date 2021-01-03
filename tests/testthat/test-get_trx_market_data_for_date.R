test_that("get_trx_market_data_for_date returns correct objects", {
  expect_null(suppressMessages(
    get_trx_market_data_for_date(date = "2017-11-08")
  ))

  expect_null(suppressMessages(
    get_trx_market_data_for_date(date = Sys.Date() + 100)
  ))

  result <- get_trx_market_data_for_date(
    date = Sys.Date() - 30,
    vs_currencies = c("usd", "eur", "gbp")
  )

  expect_s3_class(result, "tbl")

  expect_named(result, c(
    "date", "vs_currency",
    "market_cap", "total_trading_vol", "price"
  ))

  expect_s3_class(result$date, "Date")
  expect_type(result$vs_currency, "character")
  expect_type(result$market_cap, "double")
  expect_type(result$total_trading_vol, "double")
  expect_type(result$price, "double")
  expect_true(all(result$vs_currency %in% c("usd", "eur", "gbp")))
})
