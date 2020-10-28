test_that("api_request throws errors as exected", {

  base_url <- "https://api.trongrid.io"
  wrong_address <- "abc"
  url <- httr::modify_url(base_url, path = c("v1", "accounts", wrong_address))
  url <- httr::parse_url(url)
  url$query <- list(only_confirmed = tolower(only_confirmed))
  url <- httr::build_url(url)

  expect_error(api_request(url = as.factor("abc"), max_attempts = 3L))
  expect_error(api_request(url = url, max_attempts = 0))
  expect_error(suppressMessages(api_request(url = url, max_attempts = 1L)))

})


test_that("api_request returns correct objects", {

  base_url <- "https://api.trongrid.io"
  address <- "TQjaZ9FD473QBTdUzMLmSyoGB6Yz1CGpux"
  url <- httr::modify_url(base_url, path = c("v1", "accounts", address))
  url <- httr::parse_url(url)
  url$query <- list(only_confirmed = tolower(only_confirmed))
  url <- httr::build_url(url)

  r <- api_request(url = url, max_attempts = 3L)
  data <- r$data[[1]]

  expect_type(r, "list")
  expect_named(r, expected = c("success", "meta", "data"))
  expect_true(all(c("address", "balance") %in% names(data)))
  expect_named(r$meta, c("at", "page_size"))
  expect_true(r$success)

})
