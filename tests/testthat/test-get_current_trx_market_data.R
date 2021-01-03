test_that("get_current_trx_market_data returns correct object", {
  result <- get_current_trx_market_data(vs_currencies = c("usd", "gbp"))

  expect_s3_class(result, "tbl")
  expect_equal(nrow(result), 2L)

  expect_named(result, c(
    "last_updated_at", "total_supply",
    "circulating_supply", "vs_currency",
    "market_cap", "market_cap_rank",
    "market_cap_change_24h",
    "market_cap_percentage_change_24h",
    "total_trading_vol_24h", "current_price",
    "price_high_24h", "price_low_24h",
    "price_change_24h", "price_percentage_change_1h",
    "price_percentage_change_24h",
    "price_percentage_change_7d",
    "price_percentage_change_14d",
    "price_percentage_change_30d",
    "price_percentage_change_60d",
    "price_percentage_change_200d",
    "price_percentage_change_1y",
    "ath", "ath_change_percentage", "ath_date",
    "atl", "atl_change_percentage", "atl_date",
    "coingecko_rank", "coingecko_score", "developer_score",
    "community_score", "liquidity_score",
    "public_interest_score",
    "sentiment_votes_up_percentage",
    "sentiment_votes_down_percentage"
  ))

  expect_s3_class(result$last_updated_at, "POSIXct")
  expect_s3_class(result$ath_date, "POSIXct")
  expect_s3_class(result$atl_date, "POSIXct")

  expect_equal(c("usd", "gbp"), result$vs_currency)
})
