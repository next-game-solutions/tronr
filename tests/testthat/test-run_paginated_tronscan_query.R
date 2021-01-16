test_that("run_paginated_tronscan_query returns correct object", {
  expect_error(run_paginated_tronscan_query(
    base_url = factor("https://apilist.tronscan.org/"),
    path = c("api", "transaction"),
    params = list(
      sort = "-timestamp",
      count = "true",
      limit = 25,
      block = 26808690
    )
  ))

  expect_error(run_paginated_tronscan_query(
    base_url = "https://apilist.tronscan.org/",
    path = factor(c("api", "transaction")),
    params = list(
      sort = "-timestamp",
      count = "true",
      limit = 25,
      block = 26808690
    )
  ))

  expect_error(run_paginated_tronscan_query(
    base_url = "https://apilist.tronscan.org/",
    path = c("api", "transaction"),
    params = list(
      "-timestamp",
      "true",
      25,
      26808690
    )
  ))

  result <- run_paginated_tronscan_query(
    base_url = "https://apilist.tronscan.org/",
    path = c("api", "transaction"),
    params = list(
      sort = "-timestamp",
      count = "true",
      limit = 25,
      block = 26808690
    )
  )

  expect_is(result, "list")
  expect_equal(length(result), 131L)
  expect_equal(result[[1]]$block, 26808690)
})
