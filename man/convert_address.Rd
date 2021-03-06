% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convert_address.R
\name{convert_address}
\alias{convert_address}
\title{Convert TRON addresses}
\usage{
convert_address(address)
}
\arguments{
\item{address}{(character): address of the account of interest,
either in \code{base58check} or \code{hex} format.}
}
\value{
Account address (character). If \code{address} is a \code{41}-prefixed \code{hex}
string, it will be converted into a \code{base58check}-encoded string,
and vice versa.
}
\description{
Converts between the \code{hex} and \code{base58check} address formats
}
\details{
Sometimes, especially in the raw smart contract data returned by some of
of the \code{tronr} functions, TRON addresses can be represented as \verb{0x}-prefixed
\code{hex} strings (as opposed to the proper \code{41}-prefixed \code{hex} strings).
This function can automatically convert such \verb{0x}-prefixed addresses
into human-readable \code{base58check}-encoded strings - see "Examples".
}
\examples{
hex_address <- "41357a7401a0f0c2d4a44a1881a0c622f15d986291"
hex_0x_address <- "0x357a7401a0f0c2d4a44a1881a0c622f15d986291"
base58_address <- "TEqyWRKCzREYC2bK2fc3j7pp8XjAa6tJK1"
convert_address(hex_address)
convert_address(hex_0x_address)
convert_address(base58_address)
}
