#' Get description of a TRC-10 token
#'
#' Returns various bits of information about a TRC-10 token
#'
#' @eval function_params(c("token_id", "token_name",
#'                         "detailed_info", "max_attempts"))
#'
#' @details TRC-10 are tokens issued by the system contract (as opposed to
#'     TRC-20 tokens, which are issued by smart contracts). See
#'     [official documentation](https://developers.tron.network/docs/trc10)
#'     for details.
#'
#' @return A tibble, whose content depends on the `detailed_info` argument. If
#'      `detailed_info = FALSE` (default), the tibble will have the following 5
#'      columns:
#' * `token_id` (character): same as argument `token_id`;
#' * `token_name` (character): commont name of the token;
#' * `token_abbr` (character): abbreviated name of the token;
#' * `owner_address` (character): address of the token issuer, in `base58check`
#'     format;
#' * `precision` (integer): number of digits in the decimal part of the
#'     token's amount (see [apply_decimal()] for details).
#'
#' If `detailed_info = TRUE`, the returned tibble will have the same 5 columns
#'     as above, and the following additional columns:
#' * `request_time` (POSIXct): date and time when the query was made;
#' * `reputation` (character): reputation of the token on the TRON blockchain
#' (usually, `"ok"`);
#' * `vip` (boolean): indicator of whether this token is treated as a VIP asset
#' on the blockchain;
#' * `description` (character): a free-text field describing the token;
#' * `date_created` (POSIXct): date and time of the token's creation;
#' * `ico_start_time` (POSIXct, UTC timezone): date and time of the asset's ICO
#'     start;
#' * `ico_end_time` (POSIXct, UTC timezone): date and time of the asset's ICO
#'     end;
#' * `url` (character): URL of the token's project;
#' * `github` (character): URL of the project's Github repository;
#' * `total_supply` (double): total suppyly of the token;
#' * `amount_issued` (double): number of issued tokens;
#' * `issued_percentage` (double): percentage of `amount_issued` from
#' `total_supply`;
#' * `number_of_holders` (integer): current number of accounts that hold this
#' token;
#' * `total_tx` (integer): current cumulative number of transactions that
#' involved this token;
#' * `price_in_trx` (double): current price of this token expressed in TRX;
#' * `tx_count_24h` (integer): number of transactions in the last 24h that
#' involved this token;
#' * `vol_in_trx_24h` (double): volume of transactions in the last 24h that
#' involved this token (as of `request_time`).
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' r1 <- get_trc10_token_description(token_id = "1002000",
#'                                   detailed_info = TRUE)
#' r2 <- get_trc10_token_description(token_name = "BitTorrent",
#'                                   detailed_info = TRUE)
#' identical(r1$owner_address, r2$owner_address) # TRUE
#' print(r1)
get_trc10_token_description <- function(token_id = NULL,
                                        token_name = NULL,
                                        detailed_info = FALSE,
                                        max_attempts = 3L) {
  tronr::validate_arguments(
    arg_token_id = token_id,
    arg_token_name = token_name,
    arg_detailed_info = detailed_info,
    arg_max_attempts = max_attempts
  )

  if (is.null(token_id) & is.null(token_name)) {
    rlang::abort("Either `token_id` or `token_name` are missing")
  }

  if (!is.null(token_id) & !is.null(token_name)) {
    rlang::abort("`token_id` and `token_name` cannot be used simultaneously")
  }

  request_time <- Sys.time()
  attr(request_time, "tzone") <- "UTC"

  if (!is.null(token_id)) {
    url <- build_get_request(
      base_url = "https://apilist.tronscan.org/",
      path = c("api", "token"),
      query_parameters = list(id = token_id)
    )

    data <- api_request(url, max_attempts = max_attempts)$data
  }

  if (!is.null(token_name)) {
    url <- build_get_request(
      base_url = "https://apilist.tronscan.org/",
      path = c("api", "token"),
      query_parameters = list(name = token_name)
    )

    data <- api_request(url, max_attempts = max_attempts)$data
  }

  result <- lapply(data, function(x) {
    names(x) <- snakecase::to_snake_case(names(x))

    if ("market_info" %in% names(x) &
      is.list(x$market_info) &
      length(x$market_info) != 0) {
      price_in_trx <- x$market_info$priceInTrx
      tx_count_24h <- x$market_info$txCount24h
      vol_in_trx_24h <- apply_decimal(x$market_info$volume24hInTrx, 6)
    } else {
      price_in_trx <- NA
      tx_count_24h <- NA
      vol_in_trx_24h <- NA
    }

    tibble::tibble(
      request_time = request_time,
      token_id = as.character(x$token_id),
      token_name = x$name,
      token_abbr = x$abbr,
      owner_address = x$owner_address,
      reputation = tolower(x$reputation),
      vip = x$vip,
      description = ifelse(nchar(x$description) <= 1L,
        NA_character_, x$description
      ),
      date_created = from_unix_timestamp(x$date_created),
      ico_start_time = from_unix_timestamp(x$start_time),
      ico_end_time = from_unix_timestamp(x$end_time),
      url = ifelse(nchar(x$url) <= 1L ||
        x$url == "N/A" ||
        x$url == "http://" ||
        x$url == "http://...",
      NA_character_, x$url
      ),
      github = ifelse(nchar(x$github) <= 1L ||
        x$github == "N/A" ||
        x$github == "http://" ||
        x$github == "http://...",
      NA_character_, x$github
      ),
      total_supply = x$total_supply,
      amount_issued = x$issued,
      issued_percentage = x$issued_percentage,
      precision = x$precision,
      number_of_holders = x$nr_of_token_holders,
      total_tx = x$total_transactions,
      price_in_trx = price_in_trx,
      tx_count_24h = tx_count_24h,
      vol_in_trx_24h = vol_in_trx_24h
    )
  }) %>%
    dplyr::bind_rows()


  if (!detailed_info) {
    result <- result %>% dplyr::select(
      token_id,
      token_name,
      token_abbr,
      owner_address,
      precision
    )
  }

  return(result)
}
