test_that("get_trc20_transfers returns correct objects", {
  expect_error(get_trc20_transfers(
    contract_address = "abcs",
    min_timestamp = "1609459860000",
    max_timestamp = "1609459865000",
    max_attempts = 3L
  ))

  r1 <- get_trc20_transfers(
    min_timestamp = "1609459860000",
    max_timestamp = "1609459865000"
  )

  r2 <- get_trc20_transfers(
    contract_address = "TR7NHqjeKQxGTCi8q8ZY4pL8otSzgjLj6t",
    min_timestamp = "1609459860000",
    max_timestamp = "1609459865000"
  )

  expect_true(length(unique(r2$token_contract_address)) == 1L)
  expect_equal(length(unique(r1$token_contract_address)), 3L)

  expect_true(all(substr(r1$token_contract_address, 1, 1) == "T") &
    all(tronr::is_address(r1$token_contract_address)))

  expect_true(all(substr(r1$from_address, 1, 1) == "T") &
    all(tronr::is_address(r1$from_address)))

  expect_is(r1$amount, "numeric")
  expect_is(r2$amount, "numeric")

  expect_equal(round(sum(r1$amount)), 10433)
  expect_equal(round(sum(r2$amount)), 15547)
})
