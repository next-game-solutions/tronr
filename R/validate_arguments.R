#' Validate arguments
#'
#' Validates arguments that are commonly used in `tronr` functions
#'
#' @param arg_address (character): account or smart contract address;
#' @param arg_contract_address (character): contract address;
#' @param arg_asset_id (character): token ID;
#' @param arg_asset_name (character): token name;
#' @param arg_tx_id (character): transaction ID;
#' @param arg_event_name (character): event name;
#' @param arg_block_number (character): block number;
#' @param arg_only_confirmed (boolean): whether results should be returned
#'     for confirmed transactions only;
#' @param arg_only_unconfirmed (boolean): whether results should be returned
#'     for unconfirmed transactions only;
#' @param arg_only_to (boolean): whether results should be returned
#'     for transactions directed to a given account;
#' @param arg_only_from (boolean): whether results should be returned
#'     for transactions originated from a given account;
#' @param arg_min_timestamp (numeric or character): start of the time range
#'     to retrieve the data from;
#' @param arg_max_timestamp (numeric or character): end of the time range
#'     to retrieve the data from;
#' @param arg_detailed_info (boolean): whether detailed information
#'     should be retrieved for an entity (e.g. TRC-10 tokens);
#' @param arg_add_contract_data (boolean): whether to add raw contract data
#'     to the tibble with transaction attributes.
#' @param arg_order_by (character): variable to order the results by;
#' @param arg_direction (character): how to order the results (`desc` or `asc`);
#' @param arg_max_attempts (integer): number of additional attempts to call
#'     the API if the first attempt fails.
#' @param arg_vs_currencies (character): abbreviated names of the currencies
#'     to benchmark TRX against (using the public CoinGecko API).
#'
#' @return This function is to be used for its side effects. If any of the
#'     checks is not passed, it returns the respective error. If all checks
#'     are passed successfully, nothing is returned.
#' @export
#'
#' @examples
#' validate_arguments(arg_address = "TAUN6FwrnwwmaEqYcckffC7wYmbaS6cBiX")
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
                               arg_detailed_info = NULL,
                               arg_add_contract_data = NULL,
                               arg_order_by = NULL,
                               arg_direction = NULL,
                               arg_max_attempts = NULL,
                               arg_vs_currencies = NULL) {
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


  if (!is.null(arg_asset_id)) {
    if (!is.character(arg_asset_id)) {
      rlang::abort("`id` must be a character value")
    }
  }


  if (!is.null(arg_asset_name)) {
    if (!is.character(arg_asset_name)) {
      rlang::abort("`name` must be a character value")
    }
  }


  if (!is.null(arg_tx_id)) {
    if (!is.character(arg_tx_id)) {
      rlang::abort("`tx_id` must be a character value")
    }
  }


  if (!is.null(arg_event_name)) {
    if (!is.character(arg_event_name)) {
      rlang::abort("`event_name` must be a character value")
    }
  }


  if (!is.null(arg_block_number)) {
    if (!is.character(arg_block_number)) {
      rlang::abort("`block_number` must be a character value")
    }
  }


  if (!is.null(arg_only_confirmed)) {
    if (!is.logical(arg_only_confirmed)) {
      rlang::abort("`only_confirmed` must be either boolean or NULL")
    }
  }


  if (!is.null(arg_only_unconfirmed)) {
    if (!is.logical(arg_only_unconfirmed)) {
      rlang::abort("`only_unconfirmed` must be either boolean or NULL")
    }
  }


  if (!(is.null(arg_only_confirmed) & is.null(arg_only_unconfirmed))) {
    if (is.logical(arg_only_confirmed) & is.logical(arg_only_unconfirmed)) {
      rlang::abort("`only_confirmed` and `only_unconfirmed` cannot be used simultaneously")
    }
  }


  if (!is.null(arg_only_to)) {
    if (!is.logical(arg_only_to)) {
      rlang::abort("`only_to` must be boolean")
    }
  }


  if (!is.null(arg_only_from)) {
    if (!is.logical(arg_only_from)) {
      rlang::abort("`only_from` must be boolean")
    }
  }


  if (!is.null(arg_min_timestamp)) {
    if (!(is.character(arg_min_timestamp) | is.numeric(arg_min_timestamp))) {
      rlang::abort("`min_timestamp` must be either numeric or character or NULL")
    }

    min_dt <- suppressWarnings(as.numeric(arg_min_timestamp) / 1000)
    if (is.na(min_dt)) {
      rlang::abort("`min_timestamp` cannot be coerced to a POSIXct value")
    }
  }


  if (!is.null(arg_max_timestamp)) {
    if (!(is.character(arg_max_timestamp) | is.numeric(arg_max_timestamp))) {
      rlang::abort("`max_timestamp` must be either numeric or character or NULL")
    }

    max_dt <- suppressWarnings(as.numeric(arg_max_timestamp) / 1000)
    if (is.na(max_dt)) {
      rlang::abort("`max_timestamp` cannot be coerced to a POSIXct value")
    }
  }


  if (!is.null(arg_detailed_info)) {
    if (!is.logical(arg_detailed_info)) {
      rlang::abort("`detailed_info` must be either boolean or NULL")
    }
  }


  if (!is.null(arg_add_contract_data)) {
    if (!is.logical(arg_add_contract_data)) {
      rlang::abort("`add_contract_data` must be a boolean value")
    }
  }


  if (!is.null(arg_order_by)) {
    if (!is.character(arg_order_by)) {
      rlang::abort("`order_by` must be a character value")
    }
  }


  if (!is.null(arg_direction)) {
    if (!is.character(arg_direction)) {
      rlang::abort("`direction` must be a character value")
    }

    if (!arg_direction %in% c("asc", "desc")) {
      rlang::abort(c("`direction` must be one of:", c("asc", "desc")))
    }
  }


  if (!is.null(arg_max_attempts)) {
    if (!(is.integer(arg_max_attempts) & arg_max_attempts > 0)) {
      rlang::abort("`max_attempts` must be a positive integer")
    }
  }

  if (!is.null(arg_vs_currencies)) {
    if (!is.character(arg_vs_currencies)) {
      rlang::abort("`vs_currencies` must be a character vector")
    }
  }
}
