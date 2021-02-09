test_that("get_trc10_transfers returns correct objects", {

  expect_error(get_trc10_transfers(
    owner_address = c("THLLMnsEKEci5e5dJHnW28QQU8AujGhSoK",
                      "THLLMnsEKEci5e5dJHnW28QQU8AujGhSoK"),
    min_timestamp = "1577837400000",
    max_timestamp = "1577837440000"
  ))


  r1 <- get_trc10_transfers(
    min_timestamp = "1577837400000",
    max_timestamp = "1577837430000"
  )

  r2 <- get_trc10_transfers(
    owner_address = "THLLMnsEKEci5e5dJHnW28QQU8AujGhSoK",
    min_timestamp = "1577837400000",
    max_timestamp = "1577837430000"
  )

  expect_equal(names(r1), names(r2))

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

})
