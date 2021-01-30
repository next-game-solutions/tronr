test_that("convert_contract_type_id returns correct results", {
  expect_error(convert_contract_type_id("a"))
  expect_error(convert_contract_type_id(NA))

  result1 <- convert_contract_type_id(c(1, 100, 30))
  result2 <- convert_contract_type_id(c("1", "100", "30"))

  expect_identical(result1, result2)
  expect_equal(result1, c("TransferContract", "100", "CreateSmartContract"))
})
