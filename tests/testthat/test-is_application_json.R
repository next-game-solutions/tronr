test_that("is_application_json() throws an error when its input is not of class `response`", {

  expect_error(is_application_json("abcd"))

})

test_that("is_application_json() returns correct value", {

  account_address <- "TQjaZ9FD473QBTdUzMLmSyoGB6Yz1CGpux"
  url <- httr::modify_url(url = "https://api.trongrid.io",
                          path = c("v1/accounts", account_address))
  r <- httr::GET(url)
  expect_true(is_application_json(r))

})
