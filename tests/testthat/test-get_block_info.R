test_that("get_block_info returns correct object", {
  expect_error(get_block_info(latest = NULL))
  expect_error(get_block_info(latest = FALSE))

  result_latest <- get_block_info(latest = TRUE)
  result_block <- get_block_info(latest = FALSE, block_number = "26810333")

  expect_equal(names(result_latest), names(result_block))

  expect_s3_class(result_latest, "tbl")
  expect_s3_class(result_block, "tbl")

  expect_named(result_latest, c(
    "request_time",
    "block_number", "timestamp",
    "hash", "parent_hash",
    "tx_trie_root", "confirmed",
    "size", "witness_address",
    "tx_count", "tx"
  ))

  expect_s3_class(result_latest$tx[[1]], "tbl")
  expect_s3_class(result_block$tx[[1]], "tbl")

  expect_equal(names(result_latest$tx[[1]]), names(result_block$tx[[1]]))

  expect_named(
    result_latest$tx[[1]],
    c("tx_id", "contract_type", "from_address", "to_address")
  )


  expect_equal(result_latest$tx_count, nrow(result_latest$tx[[1]]))
  expect_equal(result_block$tx_count, nrow(result_block$tx[[1]]))
})
