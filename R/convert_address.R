#' Convert TRON addresses
#'
#' Converts between the `hex` and `base58check` address formats
#'
#' @eval function_params(c("address"))
#'
#' @return Account address (character). If `address` is a `41`-prefixed `hex`
#'     string, it will be converted into a `base58check`-encoded string,
#'     and vice versa.
#'
#' @details Sometimes, especially in the raw smart contract data returned by some of
#'     the `tronr` functions, TRON addresses can be represented as `0x`-prefixed
#'     `hex` strings (as opposed to the proper `41`-prefixed `hex` strings).
#'     This function can automatically convert such `0x`-prefixed addresses
#'     into human-readable `base58check`-encoded strings - see "Examples".
#'
#' @export
#'
#' @examples
#' hex_address <- "41357a7401a0f0c2d4a44a1881a0c622f15d986291"
#' hex_0x_address <- "0x357a7401a0f0c2d4a44a1881a0c622f15d986291"
#' base58_address <- "TEqyWRKCzREYC2bK2fc3j7pp8XjAa6tJK1"
#' convert_address(hex_address)
#' convert_address(hex_0x_address)
#' convert_address(base58_address)
convert_address <- function(address) {

  first_two_chars <- substr(address, 1, 2)

  if (first_two_chars == "0x") {
    r <- v8_global_context$get(sprintf("tronaddr.fromHex('%s')", address))
  }

  if (first_two_chars == "41") {
    r <- v8_global_context$get(sprintf("tronaddr.fromHex('%s')", address))
  }

  if (first_two_chars != "0x" & first_two_chars != "41") {
    r_temp <- v8_global_context$get(sprintf("tronaddr.toHex('%s')", address))
    r <- paste0("41", substr(r_temp, 3, nchar(r_temp)))
  }

  if (!is_address(r)) {
    rlang::abort("Provided `address` is not a valid TRON address")
  } else {
    return(r)
  }
}
