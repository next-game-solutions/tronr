#' Get description of a TRC-20 token
#'
#' Returns various bits of information about a TRC-20 token
#'
#' @eval function_params(c("contract_address", "detailed_info", "max_attempts"))
#'
#' @details TRC-20 are tokens issued by smart contracts. See
#'     [official documentation](https://developers.tron.network/docs/trc20)
#'     for details.
#'
#' @return A tibble, whose content depends on the `detailed_info` argument. If
#'      `detailed_info = FALSE` (default), the tibble will have the following 5
#'      columns:
#' * `token_name` (character): commont name of the token;
#' * `token_abbr` (character): abbreviated name of the token ("symbol");
#' * `token_contract_address` (character): address of the token's contract, in
#'     `base58check` format;
#' * `token_owner_address` (character): address of the token's creator, in
#'     `base58check` format;
#' * `precision` (integer): number of digits in the decimal part of the
#'     token's amount (see [apply_decimal()] for details).
#'
#' If `detailed_info = TRUE`, the returned tibble will have the same 5 columns
#'     as above, and the following additional columns:
#' * `request_time` (POSIXct): date and time when the query was made;
#' * `vip` (boolean): indicator of whether this token is treated as a VIP asset
#' on the blockchain;
#' * `description` (character): a free-text field describing the token;
#' * `date_created` (POSIXct): date and time of the token's creation;
#' * `url` (character): URL of the token's project;
#' * `total_supply` (double): total supply of the token;
#' * `number_of_holders` (integer): current number of accounts that hold this
#' token;
#' * `total_tx` (integer): current cumulative number of transactions that
#' involved this token;
#' * `price_in_trx` (double): current price of this token expressed in TRX.
#'
#' If no description for the requested token can be found, the function will
#' return no data (`NULL`), with the respective console message.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' r1 <- get_trc20_token_description(
#'   contract_address = "TLa2f6VPqDgRE67v1736s7bJ8Ray5wYjU7",
#'   detailed_info = TRUE
#' )
#' r2 <- get_trc20_token_description(
#'   contract_address = "TLa2f6VPqDgRE67v1736s7bJ8Ray5wYjU7",
#'   detailed_info = FALSE
#' )
#' print(r1)
#' print(r2)
get_trc20_token_description <- function(contract_address,
                                        detailed_info = FALSE,
                                        max_attempts = 3L) {
  validate_arguments(
    arg_contract_address = contract_address,
    arg_detailed_info = detailed_info,
    arg_max_attempts = max_attempts
  )

  if (substr(contract_address, 1, 2) == "41" |
      substr(contract_address, 1, 2) == "0x") {
    contract_address <- convert_address(contract_address)
  }

  request_time <- Sys.time()
  attr(request_time, "tzone") <- "UTC"

  url_for_n_tx <- build_get_request(
    base_url = "https://apilist.tronscan.org/",
    path = c("api", "token_trc20", "transfers"),
    query_parameters = list(contract_address = contract_address,
                            limit = 0)
  )

  n_tx <- api_request(url_for_n_tx, max_attempts = max_attempts)
  n_tx <- ifelse(is.null(n_tx$rangeTotal),
                 NA_integer_, as.integer(n_tx$rangeTotal)
  )

  url <- build_get_request(
    base_url = "https://apilist.tronscan.org/",
    path = c("api", "token_trc20"),
    query_parameters = list(contract = contract_address)
  )

  data <- api_request(url, max_attempts = max_attempts)$trc20_tokens[[1]]


  if (length(data) == 0) {
    message("\nNo data found for this token")
    return(NULL)
  }

  names(data) <- snakecase::to_snake_case(names(data))

  if ("market_info" %in% names(data) &
      is.list(data$market_info) &
      length(data$market_info) != 0) {
    price_in_trx <- data$market_info$priceInTrx
  } else {
    price_in_trx <- NA
  }

  result <- tibble::tibble(
    request_time = request_time,
    token_name = data$name,
    token_abbr = data$symbol,
    token_contract_address = data$contract_address,
    token_owner_address = data$issue_address,
    vip = data$vip,
    description = ifelse(nchar(data$token_desc) <= 1L,
                         NA_character_, data$token_desc
    ),
    date_created = as.POSIXct(data$issue_time, tz = "UTC"),
    url = ifelse(nchar(data$home_page) <= 1L ||
                   data$home_page == "N/A" ||
                   data$home_page == "http://" ||
                   data$home_page == "http://...",
                 NA_character_, data$home_page
    ),
    total_supply = as.numeric(data$total_supply_with_decimals),
    precision = data$decimals,
    number_of_holders = data$holders_count,
    total_tx = n_tx,
    price_in_trx = price_in_trx
  )

  if (!detailed_info) {
    result <- result %>% dplyr::select(
      .data$token_name,
      .data$token_abbr,
      .data$token_contract_address,
      .data$token_owner_address,
      .data$precision
    )
  }

  return(result)
}
