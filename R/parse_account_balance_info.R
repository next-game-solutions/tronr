#' Parse account balance attributes
#'
#' Converts a list with account balance attributes into a nested tibble
#'
#' @param info A non-empty, named list returned as a result of calling the
#'     `api/account` endpoint of the Tronscan API. This list is also
#'     supposed to be appended with a `request_time` element that contains
#'     a POSIXct values correspinding to the time when the API call was made.
#'
#' @return A nested tibble with parsed attributes related to the account
#'     balance. See [get_account_balance()] for details.
#'
#' @importFrom tidyselect vars_select_helpers
#'
parse_account_balance_info <- function(info) {
  if (!is.list(info)) {
    rlang::abort("`info` must be a list")
  }

  names(info) <- snakecase::to_snake_case(names(info))

  names(info$bandwidth) <- snakecase::to_snake_case(names(info$bandwidth))
  bandwidth_vars_to_use <- setdiff(names(info$bandwidth), c("assets"))
  bandwidth <- info$bandwidth[bandwidth_vars_to_use] %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
      dplyr::across(tidyselect::vars_select_helpers$everything(), as.numeric)
    )

  if (!is.null(info$tokens)) {
    which_trc20 <- sapply(info$tokens, function(x) x$tokenType == "trc20")
    n_trc20 <- sum(which_trc20)

    if (n_trc20 != 0) {
      trc20 <-  lapply(info$tokens[which_trc20], function(x) {
          names(x) <- snakecase::to_snake_case(names(x))
          return(x)
        }) %>%
        dplyr::bind_rows() %>%
        dplyr::mutate(balance = as.numeric(.data$balance)) %>%
        dplyr::select(-c("token_can_show", "amount", "token_logo")) %>%
        dplyr::rename(contract_address = .data$token_id) %>%
        dplyr::relocate(tidyselect::vars_select_helpers$where(is.numeric),
          .after = tidyselect::vars_select_helpers$where(is.character)
        )
    } else {
      trc20 <- NA
    }

    which_trc10 <- sapply(info$tokens, function(x) x$tokenType == "trc10")
    n_trc10 <- sum(which_trc10)

    if (n_trc10 != 0) {
      trc10 <- lapply(info$tokens[which_trc10], function(x) {
          names(x) <- snakecase::to_snake_case(names(x))
          if ("amount" %in% names(x)) {
            x <- x[-which(names(x) == "amount")]
          }
          return(x)
        }) %>%
        dplyr::bind_rows() %>%
        dplyr::mutate(balance = as.numeric(.data$balance)) %>%
        dplyr::select(-c("token_can_show", "token_logo")) %>%
        dplyr::relocate(tidyselect::vars_select_helpers$where(is.numeric),
          .after = tidyselect::vars_select_helpers$where(is.character)
        ) %>%
        dplyr::relocate(.data$token_price_in_trx,
          .after = tidyselect::vars_select_helpers$everything()
        )

      if ("_" %in% trc10$token_id) {
        n_trc10 <- n_trc10 - 1
        trx_balance <- trc10 %>%
          dplyr::filter(.data$token_id == "_") %>%
          dplyr::pull(.data$balance)
        trc10 <- trc10 %>% dplyr::filter(.data$token_id != "_")
      } else {
        trx_balance <- 0
      }
    } else {
      trc10 <- NA
    }
  }

  result <- tibble::tibble(
    name = ifelse(nchar(info$name) == 0, NA, info$name),
    total_transaction_count = info$total_transaction_count,
    bandwidth = list(bandwidth),
    trx_balance = trx_balance,
    n_trc20 = as.integer(n_trc20),
    trc20 = list(trc20),
    n_trc10 = as.integer(n_trc10),
    trc10 = list(trc10)
  )

  return(result)
}
