#' Make an API call
#'
#' Performs a GET request with an exponential backoff mechanism built in
#'
#' @param url A character value - the URL to be called.
#' @param max_attempts A non-zero, positive integer - the maximum number of
#'     additional attempts to make if the initial call fails.
#'
#' @return An R list, which contains the parsed response object. The structure
#'     of this list will vary depending on the actual API command called and its
#'     parameters.
#'
#' @details This function performs only `GET` requests. The URL is
#'    expected to be built appropriately before passing onto this function.
#'    Data returned by the API are expected to be in JSON format. This function
#'    will automatically parse that JSON object and return an R list with the
#'    respective elements.
#'
#' @export
#'
#' @examples
#' base_url <- "https://api.trongrid.io"
#' address <- "TQjaZ9FD473QBTdUzMLmSyoGB6Yz1CGpux"
#' url <- httr::modify_url(base_url, path = c("v1", "accounts", address))
#' url <- httr::parse_url(url)
#' url$query <- list(only_confirmed = tolower(TRUE))
#' url <- httr::build_url(url)
#'
#' r <- api_request(url)
#' print(r)
#'
api_request <- function(url, max_attempts = 3L) {

  stopifnot(is.character(url))
  stopifnot(is.integer(max_attempts) & max_attempts > 0)

  for (attempt in seq_len(max_attempts)) {

    r <- try(httr::GET(url), silent = FALSE)

    if (class(r) == "try-error" | httr::http_error(r)) {
      delay <- stats::runif(n = 1, min = 0, max = 2^attempt - 1)
      message("API request failed. Retrying after ",
              round(delay, 2), " seconds...")
      Sys.sleep(delay)
    } else {break}

  }

  stopifnot(tronr::is_application_json(r))

  if (httr::http_error(r)) {

    parsed <- jsonlite::fromJSON(
      httr::content(r, "text"),
      simplifyVector = FALSE)

    stop(
      sprintf("API request failed [status code %s].\n%s",
              httr::status_code(r),
              parsed$error),
      call. = FALSE
    )

  }

  parsed <- jsonlite::fromJSON(
    httr::content(r, "text"),
    simplifyVector = FALSE)

  return(parsed)

}
