#' Validate JSON objects
#'
#' Checks if an API call returned JSON-formatted data.
#'
#' @param response A response object as returned by the `httr`'s request verbs
#'     (e.g., `httr::GET` or `httr::POST`).
#'
#' @return A boolean value (`TRUE` if the API call returned JSON data).
#' @export
#'
#' @examples
#' account_address <- "TQjaZ9FD473QBTdUzMLmSyoGB6Yz1CGpux"
#' url <- httr::modify_url(
#'   url = "https://api.trongrid.io",
#'   path = c("v1", "accounts", account_address)
#' )
#' r <- httr::GET(url)
#' is_application_json(r)
is_application_json <- function(response) {
  if (!inherits(response, "response")) {
    rlang::abort("Provided input object is not of class `response`")
  }

  httr::http_type(response) == "application/json"
}
