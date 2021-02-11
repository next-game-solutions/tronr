#' Run paginated queries
#'
#' Retrieves large amounts of data from the TRON network page by page
#'
#' @param url (character): URL to query.
#' @eval function_params(c("max_attempts"))
#'
#' @return If the requested data exist, this function will return a list
#'     whose length is equal to the number of records retrieved from the
#'     network. Otherwise nothing (`NULL`) is returned, with a console
#'     message `"No data found"`.
#'
#' @export
#'
#' @examples
#' address <- "TAUN6FwrnwwmaEqYcckffC7wYmbaS6cBiX"
#' only_confirmed <- TRUE
#' only_from <- TRUE
#' min_timestamp <- "1577836800000"
#' max_timestamp <- "1577838600000"
#' query_params <- list(
#'   only_confirmed = tolower(only_confirmed),
#'   only_from = tolower(only_from),
#'   min_timestamp = min_timestamp,
#'   max_timestamp = max_timestamp,
#'   search_internal = tolower(FALSE)
#' )
#' url <- tronr::build_get_request(
#'   base_url = "https://api.trongrid.io",
#'   path = c(
#'     "v1", "accounts",
#'     address, "transactions"
#'   ),
#'   query_parameters = query_params
#' )
#' d <- run_paginated_query(url)
#' print(d)
run_paginated_query <- function(url, max_attempts = 3L) {
  tronr::validate_arguments(arg_max_attempts = max_attempts)

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

  if (length(data) == 0L) {
    message("No data found")
    return(NULL)
  }

  return(data)
}
