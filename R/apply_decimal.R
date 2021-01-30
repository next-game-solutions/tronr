#' Change representation of token amounts
#'
#' Represents token amounts using the whole number and the decimal parts
#'
#' @param amount (double): token amount expressed using the "machine-level"
#'     precision (see Details). Can be a vector of values.
#' @param decimal (double): number of digits after the decimal point for the
#'     token of interest. Can be a vector of values.
#'
#' @details All token transfers on the TRON blockchain are performed using the
#'     "machine-level" precision of token amounts. This function helps users to
#'     obtain the whole number and the decimal parts of the corresponding
#'     amounts. Here is an example for Tronix (TRX), whose decimal precision
#'     is 6:
#'
#' - machine-level representation: `30000555`
#' - representation using the whole number and the decimal parts: `30.000555`
#'
#' @return A numeric vector with token amounts.
#' @export
#'
#' @examples
#' amounts <- c(30000555, 110500655)
#' decimals <- c(6, 8)
#' apply_decimal(amount = amounts, decimal = decimals)
apply_decimal <- function(amount, decimal) {
  if (!is.numeric(amount) || !is.numeric(decimal)) {
    rlang::abort("`amount` and `decimal` must be numeric values")
  }

  dec <- as.numeric(paste0("1e", decimal))
  return(amount / dec)
}
