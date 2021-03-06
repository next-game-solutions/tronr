% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/build_get_request.R
\name{build_get_request}
\alias{build_get_request}
\title{Build URLs for \code{GET} requests}
\usage{
build_get_request(base_url = "https://api.trongrid.io", path, query_parameters)
}
\arguments{
\item{base_url}{(character): API's base URL (host). Defaults to
\code{"https://api.trongrid.io"}.}

\item{path}{(character): vector, whose elements form the path of the
respective API endpoint. The order of these elements is important. For
example, if the path is \code{v1/accounts/TQjaZ9FD473QBTdUzMLmSyoGB6Yz1CGpux},
then this vector must be
\code{path = c("v1", "accounts", "TQjaZ9FD473QBTdUzMLmSyoGB6Yz1CGpux")}}

\item{query_parameters}{(named list): contains parameters of the request.}
}
\value{
A URL that can be used in a \code{GET} request.
}
\description{
Returns a URL properly formatted for a \code{GET} request
}
\details{
No validation of the base URL is performed by this function,
so users are advised to ensure the URL is correctly formatted and encoded.
}
\keyword{internal}
