#' Get transactions for a given account
#'
#' Returns various bits of information about the transactions of a given account
#'
#' @param address A character value - address of the account of interest, in
#'     `base58` (starts with `T`) or `hex` (starts with `41`) format.
#' @param only_confirmed A boolean value. If `NULL` (default) or `FALSE`,
#'     both confirmed and unconfirmed transactions are returned. If `TRUE`,
#'     only confirmed transactions are returned. Cannot be used simultanously
#'     with the `only_unconfirmed` argument (see next).
#' @param only_unconfirmed A boolean value. If `NULL` (default) or `FALSE`,
#'     both confirmed and unconfirmed transactions are returned. If `TRUE`,
#'     only unconfirmed transactions are returned. Cannot be used simultanously
#'     with the `only_confirmed` argument.
#' @param only_to A boolean value (defautls to `FALSE`). If `TRUE`, only
#'     inbound transactions are returned.
#' @param only_from A boolean value (defautls to `FALSE`). If `TRUE`, only
#'     inbound transactions are returned.
#' @param min_timestamp A Unix timestamp (_including milliseconds_), presented
#'     as either a numeric or a character value. Defines the beginning of the
#'     period to retrieve the transactions from.
#' @param max_timestamp A Unix timestamp (_including milliseconds_), presented
#'     as either a numeric or a character value. Defines the end of the
#'     period to retrieve the transactions from.
#' @param limit An integer - number of transactions per page. Defaults to 20.
#'     Maximum accepted value is 200.
#' @param search_internal A boolean value. If `TRUE` (default), query params are
#'     applied to both normal and internal transactions. If `FALSE`, query
#'     params are only applied to normal transactions.
#' @param max_attempts A non-zero, positive integer specifying the maximum
#'     number of additional attempts to call the API if the first attempt fails
#'     (i.e. its call status is different from `200`). Additional attempts are
#'     implemented with an exponential backoff. Defaults to 3.
#'
#' @details As some accounts may have a very high load of transactions going
#'     through them, this function will only return data for a maximum of 1 day.
#'     This behaviour is enforced even if the difference between the requested
#'     `max_timestamp` and `min_timestamp` is larger than 1 day. The user is
#'     advised to implement their own logic if they need to retrieve data
#'     covering a larger period of time.
#'
#' @return A data frame where each row corresponds to one transaction.
#' @export
#'
#' @examples tx_df <- get_tx_info_by_account_address(
#'                          address = "TAUN6FwrnwwmaEqYcckffC7wYmbaS6cBiX",
#'                          only_confirmed = TRUE,
#'                          only_from = TRUE,
#'                          min_timestamp = "1577836800000",
#'                          max_timestamp = "1577838600000",
#'                          limit = 10L
#'                          )
#' print(tx_df)
#'
get_tx_info_by_account_address <- function(address,
                                           only_confirmed = NULL,
                                           only_unconfirmed = NULL,
                                           only_to = FALSE,
                                           only_from = FALSE,
                                           min_timestamp = 0,
                                           max_timestamp = NULL,
                                           limit = 10L,
                                           search_internal = TRUE,
                                           max_attempts = 3L) {

  # Validate inputs:

  stopifnot(is.character(address))
  stopifnot(is.logical(only_confirmed) | is.null(only_confirmed))
  stopifnot(is.logical(only_unconfirmed) | is.null(only_unconfirmed))
  stopifnot(is.logical(only_to))
  stopifnot(is.logical(only_from))
  stopifnot(is.integer(limit))
  stopifnot(is.logical(search_internal))
  stopifnot(is.integer(max_attempts) & max_attempts > 0)

  if (!(is.character(min_timestamp) | is.numeric(min_timestamp))) {
    stop("`min_timestamp` is neither a numeric nor a character value",
         call. = FALSE)
  }

  min_dt <- suppressWarnings(as.numeric(min_timestamp) / 1000)

  if (is.na(min_dt)) {
    stop("`min_timestamp` cannot be coerced to a POSIXct value", call. = FALSE)
  }

  if (!(is.character(max_timestamp) | is.numeric(max_timestamp))) {
    stop("`max_timestamp` is neither a numeric nor a character value",
         call. = FALSE)
  }

  max_dt <- suppressWarnings(as.numeric(max_timestamp) / 1000)

  if (is.na(max_dt)) {
    stop("`max_timestamp` cannot be coerced to a POSIXct value", call. = FALSE)
  }

  if (is.logical(only_confirmed) & is.logical(only_unconfirmed)) {
    stop("`only_confirmed` and `only_unconfirmed` cannot be used simultaneously",
         call. = FALSE)
  }


  # Build query:
  query_params <- list(only_confirmed = tolower(only_confirmed),
                       only_unconfirmed = tolower(only_unconfirmed),
                       only_to = tolower(only_to),
                       only_from = tolower(only_from),
                       min_timestamp = min_timestamp,
                       max_timestamp = max_timestamp,
                       search_internal = tolower(search_internal),
                       limit = limit)

  url <- tronr::build_get_request(base_url = "https://api.trongrid.io",
                                  path = c("v1", "accounts",
                                           address, "transactions"),
                                  query_parameters = query_params)

  data <- list()
  p <- 1
  while (TRUE) {
    message("Reading page ", p, "...")
    r <- tronr::api_request(url = url, max_attempts = max_attempts)
    data <- c(data, r$data)
    if (is.null(r$meta$fingerprint)) {break}
    p <- p + 1
    url <- r$meta$links$`next`
  }

  return(data)

}
