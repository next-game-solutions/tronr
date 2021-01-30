test_that("apply_decimal does the math right", {
  expect_error(apply_decimal(amount = "1", decimal = 6))

  expect_error(apply_decimal(amount = 1, decimal = "6"))

  amounts <- c(30000555, 110500655)
  decimals <- c(6, 8)
  result <- apply_decimal(amounts, decimals)

  expect_equal(length(result), 2)

  expect_equal(result, c(30.000555, 1.10500655))
})
