#' Run paginated queries
#'
#' Retrieves large amounts of data from the TRON network page by page
#'
#' @param url (character): URL to query.
#' @eval function_params(c("max_attempts"))
#'
#' @return If the requested data exist, this function will return a list
#'     whose length is equal to the number of records retrieved from the
#'     network. Otherwise an empty list is returned.
#'
#' @keywords internal
#'
run_paginated_query <- function(url, max_attempts = 3L) {
  validate_arguments(arg_max_attempts = max_attempts)

  data <- list()
  p <- 1

  pb <- progress::progress_bar$new(
    total = NA,
    clear = TRUE,
    force = TRUE,
    format = ":spin Fetching data... Elapsed time: :elapsedfull"
  )
  pb$tick(0)

  while (TRUE) {
    r <- api_request(url = url, max_attempts = max_attempts)
    data <- c(data, r$data)

    if (is.null(r$meta$fingerprint)) {
      break
    }

    p <- p + 1
    url <- r$meta$links$`next`

    pb$tick()
  }

  return(data)
}
