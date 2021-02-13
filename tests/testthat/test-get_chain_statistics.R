test_that("get_chain_statistics returns correct objects", {
  expect_error(get_chain_statistics(days = "7"))
  expect_error(get_chain_statistics(days = NA))
  expect_error(get_chain_statistics(days = c(1, 3)))
  expect_error(get_chain_statistics(days = 3, include_current_date = "FALSE"))

  result1 <- get_chain_statistics(days = 3, include_current_date = FALSE)
  result2 <- get_chain_statistics(days = 3, include_current_date = TRUE)

  expect_equal(names(result1), names(result2))

  expect_s3_class(result1, "tbl")
  expect_s3_class(result2, "tbl")

  expect_named(result1, c(
    "date",
    "avg_block_time",
    "avg_block_size",
    "total_blocks",
    "new_blocks",
    "chain_size",
    "total_addresses",
    "active_addresses",
    "new_addresses",
    "addresses_with_trx",
    "total_trc10_tokens",
    "total_trc20_tokens",
    "new_trc10_tokens",
    "new_trc20_tokens",
    "new_tx",
    "trx_transfer_tx",
    "trc10_transfer_tx",
    "freeze_tx",
    "vote_tx",
    "shielded_tx",
    "other_tx",
    "contract_triggers",
    "energy_usage",
    "net_usage"
  ))

  expect_is(result1$date, "Date")
  expect_is(result2$date, "Date")

  expect_true(max(result1$date) < Sys.Date())
  expect_true(max(result2$date) == Sys.Date())
})
