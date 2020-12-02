test_that("convert_address throws errors", {

  expect_error(convert_address(5))
  expect_error(convert_address("abc"))

})

test_that("convert_address returns correct values", {

  hex_address <- "41357a7401a0f0c2d4a44a1881a0c622f15d986291"
  base58_address <- "TEqyWRKCzREYC2bK2fc3j7pp8XjAa6tJK1"

  expect_equal(convert_address(hex_address), base58_address)
  expect_equal(convert_address(base58_address), hex_address)

})
