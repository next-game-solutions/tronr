test_that("get_current_trx_price returns correct objects", {

  expect_error(get_current_trx_price(c("usd", "abcdefg")))

  result1 <- get_current_trx_price("usd")
  result2 <- get_current_trx_price(c("usd", "eur", "gbp"))
  result3 <- get_current_trx_price(vs_currencies = "usd",
                                   include_market_cap = TRUE,
                                   include_24h_vol = TRUE,
                                   include_24h_change = TRUE)

  expect_named(result1, c("trx_price", "vs_currency", "last_updated_at"))
  expect_equal(nrow(result1), 1L)

  expect_named(result2, c("trx_price", "vs_currency", "last_updated_at"))
  expect_equal(nrow(result2), 3L)

  expect_named(result3, c("trx_price", "vs_currency", "market_cap",
                          "vol_24h", "price_percent_change_24h",
                          "last_updated_at"))
  expect_equal(nrow(result3), 1L)

  expect_type(result3$trx_price, "double")
  expect_type(result3$vs_currency, "character")
  expect_type(result3$market_cap, "double")
  expect_type(result3$vol_24h, "double")
  expect_type(result3$price_percent_change_24h, "double")
  expect_s3_class(result3$last_updated_at, "POSIXct")

})
