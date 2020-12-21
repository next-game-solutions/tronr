validate_arguments <- function(arg_address = NULL,
                               arg_contract_address = NULL,
                               arg_asset_id = NULL,
                               arg_asset_name = NULL,
                               arg_tx_id = NULL,
                               arg_event_name = NULL,
                               arg_block_number = NULL,
                               arg_only_confirmed = NULL,
                               arg_only_unconfirmed = NULL,
                               arg_only_to = NULL,
                               arg_only_from = NULL,
                               arg_min_timestamp = NULL,
                               arg_max_timestamp = NULL,
                               arg_arg_detailed_info = NULL,
                               arg_order_by = NULL,
                               arg_direction = NULL,
                               arg_limit = NULL,
                               arg_max_attempts = NULL) {

  if (!is.null(arg_address)) {
    if (!tronr::is_address(arg_address)) {
      rlang::abort("Provided `address` is not valid")
    }
  }

  if (!is.null(arg_contract_address)) {
    if (!tronr::is_address(arg_contract_address)) {
      rlang::abort("Provided `contract_address` is not valid")
    }
  }


}
