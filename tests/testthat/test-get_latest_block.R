test_that("get_latest_block returns correct object", {

  r <- get_latest_block(max_attempts = 3L)

  expect_s3_class(r, "tbl")

  expect_named(r, c("block_number", "timestamp",
                    "hash", "parent_hash",
                    "tx_trie_root", "confirmed",
                    "size", "witness_address",
                    "tx_count", "tx"))

  expect_s3_class(r$tx[[1]], "tbl")

  expect_named(r$tx[[1]], c("tx_id", "from_address", "to_address"))

})
