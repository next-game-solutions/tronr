test_that("get_supported_coingecko_currencies returns correct object", {
  result <- get_supported_coingecko_currencies()

  expect_is(result, "character")
  expect_true(is.vector(result))
  expect_true(length(result) > 0)
  expect_true(all(c("usd", "gbp", "eur", "btc") %in% result))
})
