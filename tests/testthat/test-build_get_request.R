base_url <- "https://api.trongrid.io"
path <- c("v1", "accounts", "TQjaZ9FD473QBTdUzMLmSyoGB6Yz1CGpux")
params <- list(only_confirmed = tolower(TRUE))

test_that("build_get_request trhows errors as expected", {

  expect_error(build_get_request(base_url = as.factor(base_url),
                                 path = path,
                                 query_parameters = params))

  expect_error(build_get_request(base_url = base_url,
                                 path = as.factor(path),
                                 query_parameters = params))

  expect_error(build_get_request(base_url = base_url,
                                 path = path,
                                 query_parameters = TRUE))

  wrong_url <- build_get_request(base_url = base_url,
                                 path = c("v1", "accounts"),
                                 query_parameters = params)
  expect_true(httr::http_error(httr::GET(wrong_url)))

})


test_that("build_get_request returns correct URL", {

  correct_url <- build_get_request(base_url = base_url,
                                 path = path,
                                 query_parameters = params)
  expect_false(httr::http_error(httr::GET(correct_url)))

})


