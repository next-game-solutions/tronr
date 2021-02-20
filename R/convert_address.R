#' Convert TRON addresses
#'
#' Converts between the `hex` and `base58check` address formats
#'
#' @eval function_params(c("address"))
#'
#' @return Account address (character). If `address` is in `hex` format, it
#'     will be converted to `base58check` format. If it is in `base58check`
#'     format, it will be converted to `hex` format (the variant with
#'     the `41` prefix).
#'
#' @details In addition to the `41`-prefixed `hex` addresses, this function
#'     can also convert `0x`-prefixed `hex` addresses - see examples.
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

    # if (!is_address(r)) {
    #   rlang::abort("Provided address is not a valid 0x-hex address")
    # } else {
    #   return(r)
    # }
    return(r)
  }

  # if (!is_address(address)) {
  #   rlang::abort("Provided address is not a valid TRON address")
  # }

  r <- ifelse(first_two_chars == "41",
    v8_global_context$get(
      sprintf("tronaddr.fromHex('%s')", address)
    ),
    v8_global_context$get(
      sprintf("tronaddr.toHex('%s')", address)
    )
  )

  return(r)
}
