% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/apply_decimal.R
\name{apply_decimal}
\alias{apply_decimal}
\title{Change representation of token amounts}
\usage{
apply_decimal(amount, decimal)
}
\arguments{
\item{amount}{(double): token amount expressed using the "machine-level"
precision (see Details). Can be a vector of values.}

\item{decimal}{(double): number of digits after the decimal point for the
token of interest. Can be a vector of values.}
}
\value{
A numeric vector with token amounts.
}
\description{
Represents token amounts using the whole number and the decimal parts
}
\details{
All token transfers on the TRON blockchain are performed using the
"machine-level" precision of token amounts. This function helps users to
obtain the whole number and the decimal parts of the corresponding
amounts. Here is an example for Tronix (TRX), whose decimal precision
is 6:
\itemize{
\item machine-level representation: \code{30000555}
\item representation using the whole number and the decimal parts: \code{30.000555}
}
}
\examples{
amounts <- c(30000555, 110500655)
decimals <- c(6, 8)
apply_decimal(amount = amounts, decimal = decimals)
}
