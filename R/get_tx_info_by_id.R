#' Get transaction attributes
#'
#' Returns attributes of a transaction based on its ID
#'
#' @eval function_params(c("tx_id", "add_contract_data", "max_attempts"))
#'
#' @details All token amounts in the resultant tibble (specifically, in
#'      columns `trx_transfer`, `trc10_transfer`, `trc20_transfer`, and
#'      `internal_tx`) are expressed using the whole number and the decimal
#'      parts. However, if the user requests raw contract data
#'      (`add_contract_data = TRUE`), the returned raw data will show token
#'      amounts "as is", i.e. expressed using the machine-level precision.
#'      See [apply_decimal()] for details.
#'
#' If no data can be retrived for the requested transaction (e.g., when a
#' wrong transaction ID is provided), nothing (`NULL`) is returned, with
#' the corresponding console message.
#'
#' @return A nested tibble with one row and the following columns:
#' - `request_time` (POSIXct): date and time when the request was made;
#' - `tx_id` (character): transation ID;
#' - `block_number` (character);
#' - `timestamp` (POSIXct, UTC timezone): block's time stamp;
#' - `contract_result` (character): result of calling the contract
#' (e.g., `SUCCESS`, `REVERT`, etc.);
#' - `confirmed` (boolean): a flag indicating whether this transaction has been
#' confirmed;
#' - `confirmation_count` (integer): number of accounts that confirmed;
#' - `sr_confirm_list` (list): contains a tibble with the following
#' columns:
#'     - `address` (character): `base58check`-formatted address of the
#'     Super Representative's account that confirmed this transaction;
#'     - `name` (character): name of the Super Representative's account;
#'     - `block_number` (character): block that the confirmation transaction
#'     belongs to.
#' - `contract_type` (character): type of the system contract call (see
#' [official documentation](https://tronprotocol.github.io/documentation-en/mechanism-algorithm/system-contracts/)
#' for details);
#' - `from_address` (character): `base58check`-formatted address of the account
#' that initiated this transaction (also known as the "owner address");
#' - `to_address` (character): `base58check`-formatted address of the receiving
#' account;
#' - `is_contract_from_address_account` (boolean): flag indicating whether the
#' `from_address` is a contract account;
#' - `is_contract_to_address` (boolean): flag indicating whether the
#' `to_address` is a contract account;
#' - `costs` (list): contains a tibble with the following columns:
#'     - `net_fee` (integer);
#'     - `net_fee_cost` (integer);
#'     - `energy_usage` (integer);
#'     - `energy_fee_cost` (integer);
#'     - `energy_fee` (integer);
#'     - `energy_usage_total` (integer);
#'     - `origin_energy_usage` (integer);
#'     - `net_usage` (integer).
#' - `tx_transfer` (double): amount of TRX transferred from `from_address` to
#' `to_address` as part of this transaction;
#' - `trc10_transafer` (list): contains a tibble with information on the
#' TRC-10 tokens (other than TRX) transferred as part of this transaction
#' (or `NA` if no such transfer happened). This tibble contains the following
#' columns:
#'     - `token_id` (character): ID of the TRC-10 token transferred;
#'     - `token_name` (character): common name of the token;
#'     - `token_abbr` (character): abbreviated name of the token;
#'     - `vip` (boolean): flag indicating whether this token is treated as a
#'     VIP asset on the TRON blockchain;
#'     - `amount` (double): amount of the token transferred.
#' - `trc20_transfer` (list): contains a tibble with iformation on the TRC-20
#' tokens transferred as part of this transaction (or `NA` if no such transfer
#' happened). This tibble contains the following columns:
#'     - `token_name` (character): common name of the token;
#'     - `token_abbr` (character): abbreviated name of the token;
#'     - `token_contract` (character): `base58check`-formatted address of the
#'     token's contract;
#'     - `from_address` (character): `base58check`-formatted address of the
#'     sending account;
#'     - `to_address` (character): `base58check`-formatted address of the
#'     receiving account;
#'     - `is_contract_from_address_account` (boolean): flag indicating whether
#'     the `from_address` is a contract account;
#'     - `is_contract_to_address` (boolean): flag indicating whether the
#'     `to_address` is a contract account;
#'     - `vip` (boolean): flag indicating whether this token is treated as a
#'     VIP asset on the TRON blockchain;
#'     - `amount` (double): amount of the token transferred.
#' - `internal_tx` (list): contains a tibble with iformation on the internal
#' transactions triggered by this transaction (or `NA` if no internal
#' transactions occurred). This tibble contains the following columns:
#'     - `internal_tx_id` (character): ID of the internal transaction;
#'     - `from_address` (character): `base58check`-formatted address that
#'     initiated this internal transaction (also known as "owner address");
#'     - `to_address` (character): `base58check`-formatted address of the
#'     receiving account;
#'     - `is_contract_from_address_account` (boolean): flag indicating whether
#'     the `from_address` is a contract account;
#'     - `is_contract_to_address` (boolean): flag indicating whether the
#'     `to_address` is a contract account;
#'     - `confirmed` (boolean): a flag indicating whether this transaction has
#'     been confirmed;
#'     - `rejected` (boolean): a flag indicating whether this transaction has
#'     been rejected for some reason;
#'     - `token_id` (character): ID of the token transferred as part of this
#'     internal transaction;
#'     - `token_name` (character): common name of the token;
#'     - `token_abbr` (character): abbreviated name of the token;
#'     - `vip` (boolean): flag indicating whether this token is treated as a
#'     VIP asset on the TRON blockchain;
#'     - `amount` (double): amount of the token transferred.
#' - `info` (list): contains a list with additional attributes of this
#' transaction (or `NA` if no such information is available);
#' - `contract_data` (list): contains a list with raw data generated by the
#' contract that performed this transaction. This column is only added if the
#' `add_contract_data` argument is `TRUE`. The actual content of the list with
#' raw data will depend on the transaction's nature.
#'
#' @export
#'
#' @examples
#' id <- "dca447279bc2fd3c10325e442746f9a42938e25bac33bc277b3e7720027aaaf2"
#' tx <- get_tx_info_by_id(id)
#' print(tx)
get_tx_info_by_id <- function(tx_id,
                              add_contract_data = TRUE,
                              max_attempts = 3L) {
  validate_arguments(
    arg_tx_id = tx_id,
    arg_add_contract_data = add_contract_data,
    arg_max_attempts = max_attempts
  )

  url <- build_get_request(
    base_url = "https://apilist.tronscan.org/",
    path = c("api", "transaction-info"),
    query_parameters = list(hash = tx_id)
  )

  request_time <- Sys.time()
  attr(request_time, "tzone") <- "UTC"

  r <- api_request(url = url, max_attempts = max_attempts)

  if (length(r) == 0) {
    message("No data found for this transaction")
    return(NULL)
  }

  result <- tibble::tibble(
    request_time,
    parse_tx_info(r)
  )

  if (add_contract_data) {
    result <- dplyr::mutate(
      result,
      contract_data = list(r$contractData)
    )
  }

  return(result)
}
