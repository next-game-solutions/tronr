get_events_by_contract_address <- function(address,
                                           event_name = NULL,
                                           block_number = NULL,
                                           only_confirmed = NULL,
                                           only_unconfirmed = NULL,
                                           min_timestamp,
                                           max_timestamp,
                                           direction = "desc",
                                           limit = 100L,
                                           max_attempts = 3L) {

  if (!tronr::is_address(address)) {
    rlang::abort("Provided address is not valid")
  }

  if (!is.character(event_name)) {
    rlang::abort("`event_name` must be a character value")
  }

  if (!is.character(block_number)) {
    rlang::abort("`block_number` must be a character value")
  }

  if (!(is.logical(only_confirmed) | is.null(only_confirmed))) {
    rlang::abort("`only_confirmed` must be either boolean or NULL")
  }

  if (!(is.logical(only_unconfirmed) | is.null(only_unconfirmed))) {
    rlang::abort("`only_unconfirmed` must be either boolean or NULL")
  }

  if (is.logical(only_confirmed) & is.logical(only_unconfirmed)) {
    rlang::abort("`only_confirmed` and `only_unconfirmed` cannot be used simultaneously")
  }

  if (!(is.character(min_timestamp) |
        is.numeric(min_timestamp) |
        is.null(min_timestamp)) ) {
    rlang::abort("`min_timestamp` must be either numeric or character or NULL")
  }

  if (!is.null(min_timestamp)) {
    min_dt <- suppressWarnings(as.numeric(min_timestamp) / 1000)
    if (is.na(min_dt)) {
      rlang::abort("`min_timestamp` cannot be coerced to a POSIXct value")
    }
  }

  if (!(is.character(max_timestamp) |
        is.numeric(max_timestamp) |
        is.null(max_timestamp))) {
    rlang::abort("`max_timestamp` must be either numeric or character or NULL")
  }

  if (!is.null(max_timestamp)) {
    max_dt <- suppressWarnings(as.numeric(max_timestamp) / 1000)
    if (is.na(max_dt)) {
      rlang::abort("`max_timestamp` cannot be coerced to a POSIXct value")
    }
  }

  if (!is.character(direction)) {
    rlang::abort("`direction` must be a character value")
  }

  if (!direction %in% c("asc", "desc")) {
    rlang::abort(c("`order_by` must be one of:", c("asc", "desc")))
  }

  if (!(is.integer(limit) & limit > 0)) {
    rlang::abort("`limit` must be a positive integer")
  }

  if (!(is.integer(max_attempts) & max_attempts > 0)) {
    rlang::abort("`max_attempts` must be a positive integer")
  }


  query_params <- list(event_name = event_name,
                       block_number = block_number,
                       only_confirmed = tolower(only_confirmed),
                       only_unconfirmed = tolower(only_unconfirmed),
                       min_block_timestamp = min_timestamp,
                       max_block_timestamp = max_timestamp,
                       order_by = paste("block_timestamp", direction, sep = ","),
                       limit = limit)

  url <- tronr::build_get_request(base_url = "https://api.trongrid.io",
                                  path = c("v1", "contracts",
                                           address, "events"),
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

  if (length(data) == 0L) {
    message("No transactions found")
    return(NULL)
  }

  result <- dplyr::bind_rows(lapply(data, tronr::parse_events_info))
  result <- dplyr::bind_cols(result)

}
