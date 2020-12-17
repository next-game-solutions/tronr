test_that("convert_address throws errors", {

  expect_error(convert_address(5))
  expect_error(convert_address("abc"))
  expect_error(convert_address())

})

test_that("convert_address returns correct values", {

  hex_address <- "41357a7401a0f0c2d4a44a1881a0c622f15d986291"
  base58_address <- "TEqyWRKCzREYC2bK2fc3j7pp8XjAa6tJK1"
  hex_0x_address <- "0xb92b8c4f17fb2e9f9265caf48928f73e790a55da"

  expect_equal(convert_address(hex_address), base58_address)
  expect_equal(convert_address(base58_address), hex_address)
  expect_equal(convert_address(hex_0x_address),
               "TSrJBKbSrLyULVLRSbkJzqXovFdcyDL7hJ")

})
