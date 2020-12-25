test_that("run_paginated_queries ruturns correct results", {

  address = "TAUN6FwrnwwmaEqYcckffC7wYmbaS6cBiX"
  only_confirmed = TRUE
  only_from = TRUE
  min_timestamp = "1577836800000"
  max_timestamp1 = "1577838600000"
  max_timestamp2 = "1577836800001"

  base_url = "https://api.trongrid.io"
  path <- c("v1", "accounts", address, "transactions")


  query_params1 <- list(only_confirmed = tolower(only_confirmed),
                        only_from = tolower(only_from),
                        min_timestamp = min_timestamp,
                        max_timestamp = max_timestamp1,
                        search_internal = tolower(FALSE),
                        limit = 10L)

  query_params2 <- query_params1
  query_params2$max_timestamp <- max_timestamp2

  url1 <- tronr::build_get_request(base_url = base_url,
                                   path = path,
                                   query_parameters = query_params1)

  url2 <- tronr::build_get_request(base_url = base_url,
                                   path = path,
                                   query_parameters = query_params2)

  d1 <- run_paginated_query(url1)
  d2 <- suppressMessages(run_paginated_query(url2))

  expect_is(d1, "list")
  expect_equal(length(d1), 9L)
  expect_message(run_paginated_query(url2), "No data found")

  expect_null(d2)

})
