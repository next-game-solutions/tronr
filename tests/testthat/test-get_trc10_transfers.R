test_that("get_trc10_transfers returns correct objects", {
  expect_error(get_trc10_transfers(
    owner_address = c(
      "THLLMnsEKEci5e5dJHnW28QQU8AujGhSoK",
      "THLLMnsEKEci5e5dJHnW28QQU8AujGhSoK"
    ),
    min_timestamp = "1577837400000",
    max_timestamp = "1577837440000"
  ))

  expect_error(get_trc10_transfers(
    owner_address = NULL,
    related_address = c(
      "THLLMnsEKEci5e5dJHnW28QQU8AujGhSoK",
      "THLLMnsEKEci5e5dJHnW28QQU8AujGhSoK"
    ),
    min_timestamp = "1577837400000",
    max_timestamp = "1577837440000"
  ))


  r1 <- get_trc10_transfers(
    min_timestamp = "1577837400000",
    max_timestamp = "1577837430000"
  )

  r2 <- get_trc10_transfers(
    related_address = "TMaBqmMRekKZMQEq3u3QrJpGDwPYZZo87V",
    min_timestamp = "1577837400000",
    max_timestamp = "1577837430000"
  )

  r3 <- get_trc10_transfers(
    owner_address = "TF5Bn4cJCT6GVeUgyCN4rBhDg42KBrpAjg",
    min_timestamp = "1577837400000",
    max_timestamp = "1577837430000"
  )

  r4 <- get_trc10_transfers(
    owner_address = "THLLMnsEKEci5e5dJHnW28QQU8AujGhSoK",
    related_address = "TBhjJyuXnadzLX1s3yHFduZgpCeWEzda5u",
    min_timestamp = "1577837400000",
    max_timestamp = "1577837430000"
  )

  expect_equal(names(r1), names(r2), names(r3), names(r4))

  expect_named(r1, c(
    "tx_id",
    "block_number",
    "timestamp",
    "from_address",
    "to_address",
    "is_contract_from_address",
    "is_contract_to_address",
    "contract_result",
    "confirmed",
    "amount",
    "token_id",
    "token_name",
    "token_abbr"
  ))

  expect_true(length(unique(r1$token_id)) > 1L)
  expect_true(length(unique(r2$token_id)) == 1L)
  expect_true(length(unique(r3$token_id)) == 1L)
  expect_true(length(unique(r4$token_id)) == 1L)

  expect_equal(nrow(r1), 15L)
  expect_equal(nrow(r2), 2L)
  expect_equal(nrow(r3), 1L)
  expect_equal(nrow(r4), 1L)
})
