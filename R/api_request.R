#' Make an API call
#'
#' Performs `GET` requests, with an exponential backoff mechanism built in
#'
#' @param url (character): URL to call.
#' @eval function_params(c("max_attempts"))
#'
#' @return A named list, the structure of which will vary depending on the
#'     actual `GET` request made.
#'
#' @details This function only performs `GET` requests. The `url` is
#'    expected to be built properly before passing onto this function.
#'    The returned data are expected to be in JSON format. This function
#'    will automatically parse the response object and return a named R list
#'    with the respective elements.
#'
#' @keywords internal
#'
api_request <- function(url, max_attempts = 3L) {
  if (!is.character(url)) {
    rlang::abort("`url` must be a character value")
  }

  validate_arguments(arg_max_attempts = max_attempts)

  ua <- httr::user_agent(
    sprintf(
      "tronr/%s (R toolbox to explore the TRON network; https://github.com/next-game-solutions/tronr)",
      utils::packageVersion("tronr")
    )
  )

  for (attempt in seq_len(max_attempts)) {
    r <- try(httr::GET(url, ua), silent = FALSE)

    if (class(r) == "try-error" || httr::http_error(r)) {
      delay <- stats::runif(n = 1, min = 0, max = 2^attempt - 1)
      message(
        "\nAPI request failed. Retrying after ",
        round(delay, 2), " seconds..."
      )
      Sys.sleep(delay)
    } else {
      break
    }
  }

  if (!is_application_json(r)) {
    rlang::abort("Returned data are not JSON-formatted")
  }

  if (httr::http_error(r)) {
    parsed <- jsonlite::fromJSON(
      httr::content(r, "text"),
      simplifyVector = FALSE
    )

    stop(
      sprintf(
        "API request failed [status code %s]. \n%s",
        httr::status_code(r),
        parsed$error
      ),
      call. = FALSE
    )
  }

  parsed <- jsonlite::fromJSON(
    httr::content(r, "text"),
    simplifyVector = FALSE
  )

  return(parsed)
}
