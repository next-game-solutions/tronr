test_that("to_unix_timestamp() throws errors when expected", {
  expect_error(to_unix_timestamp("abc"))
  expect_error(to_unix_timestamp(1))
  expect_error(to_unix_timestamp("2010-01-01"))
  expect_error(to_unix_timestamp("2010-01-01 00:00:70"))
  expect_error(to_unix_timestamp("00:00:10"))
})

test_that("to_unix_timestamp() returns correct values", {
  expect_identical(
    to_unix_timestamp("1970-01-01 00:00:01"),
    as.character(1000)
  )
  expect_identical(
    to_unix_timestamp("1970-01-02 00:00:00"),
    as.character(60 * 60 * 24 * 1000)
  )
  expect_identical(
    to_unix_timestamp("2021-03-12 00:30:00 UTC"),
    "1615509000000"
  )
})
