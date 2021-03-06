% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parse_events_info.R
\name{parse_events_info}
\alias{parse_events_info}
\title{Parse event attributes}
\usage{
parse_events_info(info)
}
\arguments{
\item{info}{A non-empty, named list returned as a result of calling
events-related methods (see \code{\link[=get_events_by_block_number]{get_events_by_block_number()}},
\code{\link[=get_events_by_contract_address]{get_events_by_contract_address()}}, etc.).}
}
\value{
A nested tibble where each row corresponds to an event associated
with the account of interest. This tibble contains the following
columns:
\itemize{
\item \code{tx_id} (character): transaction ID;
\item \code{block_number} (character);
\item \code{timestamp} (POSIXct, UTC timezone): block timestamp;
\item \code{contract_address} (character): adress of the smart contract that triggered
the event;
\item \code{event_name} (character): possible values of this column are contract-
and event-specific;
\item \code{event_data} (list): each element of this list contains a named list with
additional attributes of the event.
}
}
\description{
Converts a list with event attributes into a nested tibble
}
\keyword{internal}
