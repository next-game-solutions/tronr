#' Convert addresses
#'
#' Converts between `hex` and `base58check` formats of account addresses
#'
#' @param address (character) Account address
#'
#' @return Account address (character). If `address` is in `hex` format, it
#'     will be converted to `base58check` format, and the other way around.
#' @export
#'
#' @examples hex_address <- "41357a7401a0f0c2d4a44a1881a0c622f15d986291"
#' base58_address <- "TEqyWRKCzREYC2bK2fc3j7pp8XjAa6tJK1"
#' convert_address(hex_address)
#' convert_address(base58_address)
#'
convert_address <- function(address) {

  if (!tronr::is_address(address)) {
    rlang::abort("Provided address is not valid")
  }

  if (nchar(address) == 42L) {
    v8_global_context$get(sprintf("tronWeb.address.fromHex('%s')", address))
  } else {
    v8_global_context$get(sprintf("tronWeb.address.toHex('%s')", address))
  }

}
