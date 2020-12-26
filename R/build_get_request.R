#' Build URL for a GET request
#'
#' Returns a URL properly formatted for a GET request
#'
#' @param base_url (character) - API's base URL (host). The default value
#'     is `"https://api.trongrid.io"`.
#' @param path (character) - vector, whose elements form the path of the
#'     respective API endpoint. The order of these elements is important. For
#'     example, if the path is `v1/accounts/TQjaZ9FD473QBTdUzMLmSyoGB6Yz1CGpux`,
#'     then this vector must be
#'     `path = c("v1", "accounts", "TQjaZ9FD473QBTdUzMLmSyoGB6Yz1CGpux")`
#' @param query_parameters (named list) - contains query parameters as
#'     expected by the endpoint.
#'
#' @details No validation of the base URL is performed by this function,
#'     so users are advised to ensure it is correctly formatted and encoded.
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

  if (!is.character(base_url)) {
    rlang::abort("`base_url` must be a character value")
  }

  if (!is.character(path)) {
    rlang::abort("`path` must be a character value")
  }

  if (!is.list(query_parameters)) {
    rlang::abort("`query_parameters` must be a list")
  }


  url <- httr::modify_url(base_url, path = path)
  url <- httr::parse_url(url)
  url$query <- query_parameters
  url <- httr::build_url(url)

  return(url)

}
