#' Get events of latest block
#'
#' Retrieves events that took place in the most recent block
#'
#' @eval function_params(c(
#'    "only_confirmed",
#'    "max_attempts"
#' ))
#'
#' @return
#' @export
#'
#' @examples r <- get_events_of_latest_block()
#' print(r)
#'
get_events_of_latest_block <- function(only_confirmed = FALSE,
                                       max_attempts = 3L) {

  tronr::validate_arguments(arg_only_confirmed = only_confirmed,
                            arg_max_attempts = max_attempts)

  query_params <- list(only_confirmed = tolower(only_confirmed))

  url <- tronr::build_get_request(base_url = "https://api.trongrid.io",
                                  path = c("v1", "blocks",
                                           "latest", "events"),
                                  query_parameters = query_params)

  data <- tronr::run_paginated_query(url = url, max_attempts = max_attempts)

  if (is.null(data)) {return(data)}

  result <- dplyr::bind_rows(lapply(data, tronr::parse_events_info))
  return(result)

}
