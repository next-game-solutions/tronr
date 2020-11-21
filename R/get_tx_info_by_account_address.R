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
#' @param limit An integer - number of transactions per page. Defaults to 200.
#'     Maximum accepted value is 200.
#' @param max_attempts A non-zero, positive integer specifying the maximum
#'     number of additional attempts to call the API if the first attempt fails
#'     (i.e. its call status is different from `200`). Additional attempts are
#'     implemented with an exponential backoff. Defaults to 3.
#'
#' @details Some accounts may have a very high load of transactions going
#'     through them. Users are, therefore, advised to choose `min_timestamp` and
#'     `max_timestamp` wisely as the TronGrid API will not return more than
#'     10000 transactions in one request. If the number of transactions
#'     exceeds this limit, the the request will fail with status `404` and an
#'     error message
#'     `"Exceeds the maximum limit, please change your query time range"`). If
#'     data are to be retrieve for a large time range, users are advised
#'     to implement their own logic for querying the network (e.g.,
#'     splitting that range into smaller chunks and then combining the results).
#'
#' @return A tibble where each row corresponds to one transaction.
#'
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
                                           limit = 200L,
                                           max_attempts = 3L) {

  stopifnot(is.character(address))
  stopifnot(is.logical(only_confirmed) | is.null(only_confirmed))
  stopifnot(is.logical(only_unconfirmed) | is.null(only_unconfirmed))
  stopifnot(is.logical(only_to))
  stopifnot(is.logical(only_from))
  stopifnot(is.integer(limit))
  stopifnot(is.integer(max_attempts) & max_attempts > 0)


  if (!(is.character(min_timestamp) |
        is.numeric(min_timestamp) |
        is.null(min_timestamp)) ) {
    stop("`min_timestamp` is neither a numeric nor a character value",
         call. = FALSE)
  }


  if (!is.null(min_timestamp)) {
    min_dt <- suppressWarnings(as.numeric(min_timestamp) / 1000)
    if (is.na(min_dt)) {
      stop("`min_timestamp` cannot be coerced to a POSIXct value", call. = FALSE)
    }
  }


  if (!(is.character(max_timestamp) |
        is.numeric(max_timestamp) |
        is.null(max_timestamp))) {
    stop("`max_timestamp` is neither a numeric nor a character value",
         call. = FALSE)
  }

  if (!is.null(max_timestamp)) {
    max_dt <- suppressWarnings(as.numeric(max_timestamp) / 1000)

    if (is.na(max_dt)) {
      stop("`max_timestamp` cannot be coerced to a POSIXct value", call. = FALSE)
    }
  }


  if (is.logical(only_confirmed) & is.logical(only_unconfirmed)) {
    stop("`only_confirmed` and `only_unconfirmed` cannot be used simultaneously",
         call. = FALSE)
  }


  query_params <- list(only_confirmed = tolower(only_confirmed),
                       only_unconfirmed = tolower(only_unconfirmed),
                       only_to = tolower(only_to),
                       only_from = tolower(only_from),
                       min_timestamp = min_timestamp,
                       max_timestamp = max_timestamp,
                       search_internal = tolower(FALSE),
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

  result <- dplyr::bind_rows(lapply(data, tronr::parse_tx_info))

  return(result)

}
