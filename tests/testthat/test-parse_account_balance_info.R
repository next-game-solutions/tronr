test_that("parse_account_balance_info returns correct objects", {
  address <- "TQjaZ9FD473QBTdUzMLmSyoGB6Yz1CGpux"

  query_params <- list(address = address)

  url <- build_get_request(
    base_url = "https://apilist.tronscan.org/",
    path = c("api", "account"),
    query_parameters = query_params
  )

  request_time <- Sys.time()
  attr(request_time, "tzone") <- "UTC"

  r <- api_request(url = url, max_attempts = 3L)

  result <- parse_account_balance_info(r)

  expect_s3_class(result, "tbl")

  expect_named(
    result,
    c(
      "name", "total_tx",
      "bandwidth", "trx_balance",
      "n_trc20", "trc20", "n_trc10", "trc10"
    )
  )

  expect_named(
    result$trc20[[1]],
    c(
      "token_contract_address",
      "token_name",
      "token_abbr",
      "token_type",
      "balance",
      "token_price_in_trx",
      "vip"
    )
  )

  expect_named(
    result$trc10[[1]],
    c(
      "token_id",
      "token_name",
      "token_abbr",
      "token_type",
      "vip",
      "token_owner_address",
      "balance",
      "token_price_in_trx"
    )
  )

  expect_s3_class(result$bandwidth[[1]], "tbl")
  expect_s3_class(result$trc10[[1]], "tbl")
  expect_s3_class(result$trc20[[1]], "tbl")

  expect_equal(ncol(result$bandwidth[[1]]), 20L)
  expect_equal(ncol(result$trc10[[1]]), 8L)
  expect_equal(ncol(result$trc20[[1]]), 7L)
})
