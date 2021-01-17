#' Run paginated Tronscan queries
#'
#' Retrieves data from the TRON network page by page using the Tronscan API
#'
#' @param base_url (character): host URL.
#' @param path (character): a vector of character values that define the
#'     API's endpoint. Order of these values matters - it must be same as in the
#'     endpoint's definition.
#' @param params (list): a named list with query parameters.
#' @eval function_params(c("max_attempts"))
#'
#' @return A list with the retrieved data.
#'
#' @examples
#' tx_data <- tronr:::run_paginated_tronscan_query(
#'   base_url = "https://apilist.tronscan.org/",
#'   path = c("api", "transaction"),
#'   params = list(
#'     sort = "-timestamp",
#'     count = "true",
#'     limit = 25,
#'     block = 26808690
#'   )
#' )
#' print(tx_data)
run_paginated_tronscan_query <- function(base_url,
                                         path,
                                         params,
                                         max_attempts = 3L) {
  if (!is.character(base_url)) {
    rlang::abort("`base_url` must be a character value")
  }

  if (!is.character(path)) {
    rlang::abort("`path` must be a character vector")
  }

  if (!is.list(params) | length(names(params)) == 0L) {
    rlang::abort("`params` must be a named list")
  }

  data <- list()
  start <- 0

  pb <- progress::progress_bar$new(
    total = NA,
    clear = TRUE,
    force = TRUE,
    format = ":spin Elapsed time::elapsed"
  )
  pb$tick(0)

  while (TRUE) {
    url <- build_get_request(
      base_url = base_url,
      path = path,
      query_parameters = c(params, start = start)
    )

    r <- tronr::api_request(url = url, max_attempts = max_attempts)

    previous_data_length <- length(data)
    data <- c(data, r$data)
    new_data_length <- length(data)

    pb$tick()

    if (new_data_length > previous_data_length) {
      start <- start + params$limit
    } else {
      pb$finished <- TRUE
      break
    }
  }

  return(data)
}
