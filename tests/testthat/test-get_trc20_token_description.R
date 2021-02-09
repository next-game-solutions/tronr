contract_address = "TLa2f6VPqDgRE67v1736s7bJ8Ray5wYjU7"

test_that("get_trc20_token_description returns correct objects with basic info", {

  expect_error(get_trc20_token_description(
    contract_address = "abcde",
    detailed_info = FALSE,
    max_attempts = 3L
  ))

  result <- get_trc20_token_description(
    contract_address = contract_address,
    detailed_info = FALSE,
    max_attempts = 3L
  )

  expect_s3_class(result, "tbl")

  expect_named(result, expected = c(
    "token_name", "token_abbr",
    "token_contract_address",
    "token_owner_address",
    "precision"
  ))

  expect_true(substr(result$token_owner_address, 1, 1) == "T" &&
                tronr::is_address(result$token_owner_address))

  expect_true(substr(result$token_contract_address, 1, 1) == "T" &&
                tronr::is_address(result$token_contract_address))

  expect_equal(result$token_name, "WINK")
  expect_equal(result$token_abbr, "WIN")
  expect_equal(result$token_owner_address, "TKeyfKMAicwhWysvgktW568i3KyRNT8AyT")
  expect_equal(result$token_contract_address, "TLa2f6VPqDgRE67v1736s7bJ8Ray5wYjU7")
  expect_equal(result$precision, 6)

})


test_that("get_trc20_token_description returns correct objects with extended info", {

  result <- get_trc20_token_description(
    contract_address = contract_address,
    detailed_info = TRUE,
    max_attempts = 3L
  )

  expect_s3_class(result, "tbl")

  expect_named(result, expected = c(
    "request_time",
    "token_name",
    "token_abbr",
    "token_contract_address",
    "token_owner_address",
    "vip",
    "description",
    "date_created",
    "url",
    "total_supply",
    "precision",
    "number_of_holders",
    "total_tx",
    "price_in_trx"
  ))

  expect_s3_class(result$date_created, "POSIXct")
  expect_s3_class(result$request_time, "POSIXct")

  expect_true(substr(result$token_owner_address, 1, 1) == "T" &
                tronr::is_address(result$token_owner_address))

  expect_true(substr(result$token_contract_address, 1, 1) == "T" &
                tronr::is_address(result$token_contract_address))

})
