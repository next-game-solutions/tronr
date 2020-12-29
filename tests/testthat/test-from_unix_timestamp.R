test_that("from_unix_timestamp() throws an error as expected", {
  expect_error(from_unix_timestamp(as.factor("abc")))
})

test_that("from_unix_timestamp() returns correct values", {
  expect_equal(
    from_unix_timestamp("0", tz = "UTC"),
    as.POSIXlt("1970-01-01 00:00:00", tz = "UTC")
  )
  expect_equal(
    from_unix_timestamp(60 * 60 * 24 * 1000, tz = "UTC"),
    as.POSIXlt("1970-01-02 00:00:00", tz = "UTC")
  )
})
