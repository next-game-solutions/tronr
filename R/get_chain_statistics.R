#' Get historical metrics of the TRON blockchain
#'
#' Retrieves historical values of the blockhain statistics
#'
#' @param days (double): number of days to look back (defaults to 14 days).
#' @param include_current_date (boolean): whether the current date's statistics
#'    (as of the query time) should be returned. Defaults to `FALSE`.
#' @eval function_params("max_attempts")
#'
#' @return A tibble with the following columns:
#' * `date` (Date): date in the `YYYY-MM-DD` format;
#' * `avg_block_time` (double): average time covered by a block (in seconds);
#' * `avg_block_size` (double): average block size (in bytes);
#' * `total_blocks` (integer): cumulative number of blocks on the chain;
#' * `new_blocks` (integer): number of newly generated blocks;
#' * `chain_size` (double): cumulative size of the chain (in bytes);
#' * `total_addresses` (integer): cumuative number of address on the chain;
#' * `active_addresses` (integer): number of addresses that were active on `date`;
#' * `new_addresses` (integer): number of newly created addresses;
#' * `addresses_with_trx` (integer): number of Tronix (TRX)-holding addresses;
#' * `total_trc10_tokens` (integer): cumulative number of TRC-10 assets on the
#' chain;
#' * `total_trc20_tokens` (integer): cumulative number of TRC-20 assets on the
#' chain;
#' * `new_trc10_tokens`(integer): number of newly created TRC-10 assets;
#' * `new_trc20_tokens` (integer): number of newly created TRC-20 assets;
#' * `new_tx` (integer): number of new transaction on the chain;
#' * `trx_transfer_tx` (integer): number of TRX transfer transactions;
#' * `trc10_transfer_tx` (integer): number of TRC-10 transfer transactions;
#' * `freeze_tx` (integer): number of TRX freezing transactions;
#' * `vote_tx` (integer): number of vote transactions;
#' * `shielded_tx` (integer): number of shielded transactions;
#' * `other_tx` (integer): number of other transactions;
#' * `contract_triggers` (integer): cumulative number of smart contract triggers;
#' * `energy_usage` (double): amount of energy consumed;
#' * `net_usage` (double): amount of bandwidth consumed.
#'
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#'
#' @export
#'
#' @examples
#' r <- get_chain_statistics(days = 7)
#' print(r)
get_chain_statistics <- function(days = 14,
                                 include_current_date = FALSE,
                                 max_attempts = 3L) {
  if (length(days) > 1L) {
    rlang::abort("Only one `days` value is allowed")
  }

  if (is.na(days) | !is.numeric(days)) {
    rlang::abort("`days` must be a numeric value")
  }

  if (!is.logical(include_current_date)) {
    rlang::abort("`include_current_date` must be a boolean value")
  }

  validate_arguments(arg_max_attempts = max_attempts)

  if (!include_current_date) {
    days <- days + 1L
  }

  url <- build_get_request(
    base_url = "https://apilist.tronscan.org/",
    path = c("api", "stats", "overview"),
    query_parameters = list(days = days)
  )

  r <- api_request(url = url, max_attempts = max_attempts)

  if (length(r$data) == 0L) {
    message("No data found")
    return(NULL)
  }

  result <- lapply(r$data, function(x) {
    names(x) <- snakecase::to_snake_case(names(x))
    tibble::tibble(
      date = as.Date(from_unix_timestamp(x$date)),
      avg_block_time = as.numeric(x$avg_block_time),
      avg_block_size = as.numeric(x$avg_block_size),
      total_blocks = x$total_block_count,
      new_blocks = x$new_block_seen,
      chain_size = x$blockchain_size,
      total_addresses = x$total_address,
      active_addresses = x$active_account_number,
      new_addresses = x$new_address_seen,
      addresses_with_trx = x$account_with_trx,
      total_trc10_tokens = x$total_trc_10,
      total_trc20_tokens = x$total_trc_20,
      new_trc10_tokens = x$new_trc_10,
      new_trc20_tokens = x$new_trc_20,
      new_tx = x$new_transaction_seen,
      trx_transfer_tx = x$trx_transfer,
      trc10_transfer_tx = x$trc_10_transfer,
      freeze_tx = x$freeze_transaction,
      vote_tx = x$vote_transaction,
      shielded_tx = x$shielded_transaction,
      other_tx = x$other_transaction,
      contract_triggers = x$triggers,
      energy_usage = x$energy_usage,
      net_usage = x$net_usage
    )
  }) %>%
    dplyr::bind_rows()

  if (!include_current_date) {
    result <- dplyr::filter(result, .data$date != max(.data$date))
  }

  return(result)
}
