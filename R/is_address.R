#' Validate TRON addresses
#'
#' Checks if a given address is a valid `base58check`- or `hex`-formatted address
#'
#' @param address (character): an address to be validated.
#'
#' @return `TRUE` or `FALSE`, depending on the result.
#' @export
#'
#' @examples
#' is_address("41357a7401a0f0c2d4a44a1881a0c622f15d986291") # TRUE
#' is_address("TEqyWRKCzREYC2bK2fc3j7pp8XjAa6tJK1") # TRUE
#' is_address("abc") # FALSE
is_address <- function(address) {

  if (!is.character(address)) {
    rlang::abort("`address` must be a character value")
  }

  r <- httr::POST(
    url = "https://api.trongrid.io/wallet/validateaddress",
    body = list(address = address),
    encode = c("json")
  )

  parsed <- jsonlite::fromJSON(
    httr::content(r, "text", encoding = "UTF-8"),
    simplifyVector = FALSE
  )

  return(parsed$result)
}
