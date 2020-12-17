#' Convert TRON addresses
#'
#' Converts between `hex` and `base58check` formats of account addresses
#'
#' @param address (character) Account address
#'
#' @return Account address (character). If `address` is in `hex` format (i.e.
#'     starts with `41` or `0x`), it will be converted to `base58check` format
#'     (starts with `T`). If it is in `base58check` format, it will be
#'     converted into the `41`-variant of `hex` format.
#' @export
#'
#' @examples hex_address <- "41357a7401a0f0c2d4a44a1881a0c622f15d986291"
#' hex_0x_address <- "0xb92b8c4f17fb2e9f9265caf48928f73e790a55da"
#' base58_address <- "TEqyWRKCzREYC2bK2fc3j7pp8XjAa6tJK1"
#' convert_address(hex_address)
#' convert_address(hex_0x_address)
#' convert_address(base58_address)
#'
convert_address <- function(address) {

  first_two_chars <- substr(address, 1, 2)

  if ((first_two_chars == "41" & nchar(address) == 42L) |
      first_two_chars == "0x") {
    r <- v8_global_context$get(sprintf("tronWeb.address.fromHex('%s')", address))
  } else {
    r <- v8_global_context$get(sprintf("tronWeb.address.toHex('%s')", address))
  }

  if (!tronr::is_address(r)) {
    rlang::abort("Provided address could not be converted to another valid address")
  }

  return(r)

}
