test_that("convert_contract_type returns correct results", {

  expect_error(convert_contract_type("a"))
  expect_error(convert_contract_type(NA))

  result1 <- convert_contract_type(c(1, 100, 30))
  result2 <- convert_contract_type(c("1", "100", "30"))

  expect_identical(result1, result2)
  expect_equal(result1, c("TransferContract", NA, "CreateSmartContract"))
})
