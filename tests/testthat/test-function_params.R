test_that("function_params works as expected", {
  expect_true(is.na(function_params(arguments = "abcs")))
  expect_type(function_params(arguments = "address"), "character")
  expect_true(grepl("@param", function_params(arguments = c("address"))))
  expect_true(grepl("address", function_params(arguments = c("address"))))
})
