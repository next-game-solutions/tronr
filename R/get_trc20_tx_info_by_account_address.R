get_trc20_tx_info_by_account_address <- function(address,
                                                 only_confirmed = NULL,
                                                 only_unconfirmed = NULL,
                                                 only_to = FALSE,
                                                 only_from = FALSE,
                                                 min_timestamp = 0,
                                                 max_timestamp = NULL,
                                                 contract_address = NULL,
                                                 limit = 200L,
                                                 max_attempts = 3L) {

  stopifnot(is.character(address))
  stopifnot(is.character(contract_address) | is.null(contract_address))
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
                       contract_address = contract_address,
                       search_internal = tolower(FALSE),
                       limit = limit)

  url <- tronr::build_get_request(base_url = "https://api.trongrid.io",
                                  path = c("v1", "accounts",
                                           address, "transactions", "trc20"),
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

  if (length(data) == 0) {
    message("No TRC20 transactions found for this account within this time range")
    return(NULL)
  }

  result <- dplyr::bind_rows(lapply(data, tronr::parse_trc20_tx_info))

  return(result)

}
