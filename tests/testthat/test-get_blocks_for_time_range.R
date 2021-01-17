test_that("get_blocks_for_time_range returns correct object", {
  result <- get_blocks_for_time_range(
    min_timestamp = "1551715200000",
    max_timestamp = "1551715210000",
    max_attempts = 3L
  )

  expect_s3_class(result, "tbl")
  expect_named(result, c(
    "block_number", "timestamp",
    "hash", "parent_hash",
    "tx_trie_root", "confirmed",
    "revert", "size",
    "witness_address", "witness_name",
    "tx_count", "net_usage",
    "energy_usage"
  ))
  expect_equal(nrow(result), 4L)
  expect_equal(
    min(result$timestamp),
    as.POSIXct("2019-03-04 16:00:00", tz = "UTC")
  )
  expect_equal(
    max(result$timestamp),
    as.POSIXct("2019-03-04 16:00:09", tz = "UTC")
  )
})
