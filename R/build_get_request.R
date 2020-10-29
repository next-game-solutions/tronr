#' Build URL for a GET request
#'
#' Returns a URL properly formatted for a GET request
#'
#' @param base_url A character value corresponding the API's base URL (host).
#'     The default value is `"https://api.trongrid.io"`.
#' @param path A character vector, whose elements form the path of the
#'     respective API endpoint. The order of these elements is important. For
#'     example, if the path is `v1/accounts/TQjaZ9FD473QBTdUzMLmSyoGB6Yz1CGpux`,
#'     then this vector must be
#'     `path = c("v1", "accounts", "TQjaZ9FD473QBTdUzMLmSyoGB6Yz1CGpux")`
#' @param query_parameters A named list with query parameters expected by
#'     the endpoint.
#'
#' @details No validation of the base URL is performed by this function,
#'     so make sure it is correctly formatted.
#'
#' @return A URL that can be used in a `GET` request.
#' @export
#'
#' @examples
#'
#' path <- c("v1", "accounts", "TQjaZ9FD473QBTdUzMLmSyoGB6Yz1CGpux")
#' params <- list(only_confirmed = tolower(TRUE))
#' url <- build_get_request(base_url = "https://api.trongrid.io",
#'                          path = path,
#'                          query_parameters = params)
#'
build_get_request <- function(base_url = "https://api.trongrid.io",
                              path,
                              query_parameters) {

  stopifnot(is.character(base_url))
  stopifnot(is.character(path))
  stopifnot(is.list(query_parameters))

  url <- httr::modify_url(base_url, path = path)
  url <- httr::parse_url(url)
  url$query <- query_parameters
  url <- httr::build_url(url)

  return(url)

}
