#' Run paginated Tronscan queries
#'
#' Retrieves data from the TRON network page by page using the Tronscan API
#'
#' @param base_url (character): host URL.
#' @param path (character): a vector of character values that define the
#'     API's endpoint. Order of these values matters - it must be same as in the
#'     endpoint's definition.
#' @param params (list): a named list with query parameters.
#' @param show_spinner (boolean): toggle to turn on / off a counter of the
#'     time elapsed since the beginning of query.
#' @eval function_params(c("max_attempts"))
#'
#' @return A list with the retrieved data.
#'
#' @keywords internal
#'
run_paginated_tronscan_query <- function(base_url,
                                         path,
                                         params,
                                         show_spinner = TRUE,
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

  if (show_spinner) {
    pb <- progress::progress_bar$new(
      total = NA,
      clear = TRUE,
      force = TRUE,
      format = ":spin Fetching data... Elapsed time: :elapsedfull"
    )
    pb$tick(0)

    while (TRUE) {
      url <- build_get_request(
        base_url = base_url,
        path = path,
        query_parameters = c(params, start = start)
      )

      r <- api_request(url = url, max_attempts = max_attempts)
      names(r) <- snakecase::to_snake_case(names(r))

      previous_data_length <- length(data)

      if ("data" %in% names(r)) {
        new_batch <- r$data
      }
      if ("token_transfers" %in% names(r)) {
        new_batch <- r$token_transfers
      }

      data <- c(data, new_batch)
      new_data_length <- length(data)

      pb$tick()

      if (new_data_length > previous_data_length) {
        start <- start + params$limit
      } else {
        pb$finished <- TRUE
        break
      }
    }
  }

  while (TRUE) {
    url <- build_get_request(
      base_url = base_url,
      path = path,
      query_parameters = c(params, start = start)
    )

    r <- api_request(url = url, max_attempts = max_attempts)
    names(r) <- snakecase::to_snake_case(names(r))

    previous_data_length <- length(data)

    if ("data" %in% names(r)) {
      new_batch <- r$data
    }
    if ("token_transfers" %in% names(r)) {
      new_batch <- r$token_transfers
    }

    data <- c(data, new_batch)
    new_data_length <- length(data)

    if (new_data_length > previous_data_length) {
      start <- start + params$limit
    } else {
      break
    }
  }

  return(data)
}
