test_that("get_trc20_transfers returns correct objects", {
  expect_error(get_trc20_transfers(
    contract_address = c(
      "THLLMnsEKEci5e5dJHnW28QQU8AujGhSoK",
      "THLLMnsEKEci5e5dJHnW28QQU8AujGhSoK"
    ),
    min_timestamp = "1577837400000",
    max_timestamp = "1577837440000"
  ))

  expect_error(get_trc20_transfers(
    contract_address = NULL,
    related_address = c(
      "THLLMnsEKEci5e5dJHnW28QQU8AujGhSoK",
      "THLLMnsEKEci5e5dJHnW28QQU8AujGhSoK"
    ),
    min_timestamp = "1577837400000",
    max_timestamp = "1577837440000"
  ))

  r1 <- get_trc20_transfers(
    min_timestamp = "1609459860000",
    max_timestamp = "1609459865000"
  )

  r2 <- get_trc20_transfers(
    related_address = "TFQyXjQXUuBCFTXP9K72wTinqbsDUD5EDL",
    min_timestamp = "1609459860000",
    max_timestamp = "1609459865000"
  )

  r3 <- get_trc20_transfers(
    contract_address = "TR7NHqjeKQxGTCi8q8ZY4pL8otSzgjLj6t",
    min_timestamp = "1609459860000",
    max_timestamp = "1609459865000"
  )

  r4 <- get_trc20_transfers(
    contract_address = "TR7NHqjeKQxGTCi8q8ZY4pL8otSzgjLj6t",
    related_address = "TQn9Y2khEsLJW1ChVWFMSMeRDow5KcbLSE",
    min_timestamp = "1609459860000",
    max_timestamp = "1609459865000"
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
    "token_contract_address",
    "token_name",
    "token_abbr"
  ))

  expect_true(length(unique(r1$token_contract_address)) == 3L)
  expect_true(length(unique(r2$token_contract_address)) == 1L)
  expect_true(length(unique(r3$token_contract_address)) == 1L)
  expect_true(length(unique(r4$token_contract_address)) == 1L)

  expect_equal(nrow(r1), 7L)
  expect_equal(nrow(r2), 1L)
  expect_equal(nrow(r3), 8L)
  expect_equal(nrow(r4), 2L)
})
