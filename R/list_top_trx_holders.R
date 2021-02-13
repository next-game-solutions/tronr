#' List top TRX holders
#'
#' Returns a list of accounts with the largest TRX balances
#'
#' @param n (double): number of top accounts to retrieve.
#' @eval function_params("max_attempts")
#'
#' @return A tibble with `n` rows and the following columns:
#' * `request_time` (POSIXct): date and time when the request was made;
#' * `address` (character): account address (in `base58check` format);
#' * `trx_balance` (double): TRX balance;
#' * `total_tx` (integer): total number of transactions associated with
#' the respective `account`;
#' * `tron_power` (double): amount of TRX frozen
#' (see [official documentation](https://tronprotocol.github.io/documentation-en/introduction/overview/#2-srs-and-committee)
#' for details).
#'
#' @export
#'
#' @examples
#' r <- list_top_trx_holders(10)
#' print(r)
list_top_trx_holders <- function(n = 20,
                                 max_attempts = 3L) {
  if (!is.numeric(n) | length(n) > 1L) {
    rlang::abort("`n` must be a numeric vector of length 1")
  }

  validate_arguments(
    arg_max_attempts = max_attempts
  )

  data <- list()
  start <- 0

  pb <- progress::progress_bar$new(
    total = n,
    clear = TRUE,
    force = TRUE,
    format = ":spin Fetching data... Elapsed time: :elapsedfull"
  )
  pb$tick(0)

  while (TRUE) {
    url <- build_get_request(
      base_url = "https://apilist.tronscan.org/",
      path = c("api", "account", "list"),
      query_parameters = list(
        sort = "-balance",
        start = 0L,
        limit = n
      )
    )

    request_time <- Sys.time()
    attr(request_time, "tzone") <- "UTC"

    r <- tronr::api_request(url = url, max_attempts = max_attempts)
    names(r) <- snakecase::to_snake_case(names(r))

    previous_data_length <- length(data)

    if ("data" %in% names(r)) {
      new_batch <- r$data
    }

    data <- c(data, new_batch)
    new_data_length <- length(data)

    pb$tick()

    if (new_data_length < n) {
      start <- start + n
    } else {
      pb$finished <- TRUE
      break
    }
  }

  pb$finished <- TRUE

  if (length(data) == 0L) {
    message("No data found")
    return(NULL)
  }

  pb <- progress::progress_bar$new(
    total = length(data),
    clear = TRUE,
    force = TRUE,
    format = ":spin Processing data... Elapsed time: :elapsedfull"
  )
  pb$tick(0)

  result <- lapply(data, function(x) {
    names(x) <- snakecase::to_snake_case(names(x))
    r <- tibble::tibble(
      request_time = request_time,
      address = x$address,
      trx_balance = apply_decimal(x$balance, 6),
      total_tx = as.integer(x$total_transaction_count),
      tron_power = apply_decimal(x$power, 6)
    )
    pb$tick()
    return(r)
  }) %>%
    dplyr::bind_rows()

  pb$finished <- TRUE

  return(result)
}
