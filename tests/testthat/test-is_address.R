test_that("is_address returns correct values", {

  expect_true(is_address("41357a7401a0f0c2d4a44a1881a0c622f15d986291"))
  expect_true(is_address("TEqyWRKCzREYC2bK2fc3j7pp8XjAa6tJK1"))
  expect_false(is_address("abc"))

})
