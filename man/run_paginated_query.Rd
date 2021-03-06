% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_paginated_query.R
\name{run_paginated_query}
\alias{run_paginated_query}
\title{Run paginated queries}
\usage{
run_paginated_query(url, max_attempts = 3L)
}
\arguments{
\item{url}{(character): URL to query.}

\item{max_attempts}{(integer, positive): specifies the
maximum number of additional attempts to call a URL if the
first attempt fails (i.e. its call status is different from
\code{200}). Additional attempts are implemented with an exponential
backoff. Defaults to \code{3}.}
}
\value{
If the requested data exist, this function will return a list
whose length is equal to the number of records retrieved from the
network. Otherwise an empty list is returned.
}
\description{
Retrieves large amounts of data from the TRON network page by page
}
\keyword{internal}
