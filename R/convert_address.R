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
