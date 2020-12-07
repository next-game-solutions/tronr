#' Get description of a TRC-10 asset by its ID
#'
#' Returns information about a TRC-10 token based on its ID
#'
#' @param id (character) - ID of the TRC-10 token of interest, presented as a
#'     set of numbers (`"1002762"`) or as address of the owner who
#'     issued this token (in `base58` or `hex` format). Using either of this
#'     IDs is possible because there is a 1:1 relationship between them, i.e.
#'     an account is only allowed to issue a single TRC-10 token.
#' @param only_confirmed (boolean) - if `TRUE`, the asset balance will be
#'     returned as of the latest confirmed block, otherwise as of the
#'     latest unconfirmed one. Defaults to `FALSE`.
#' @param detailed_info (boolean) - if `FALSE` (default), only basic
#'     information about the asset will be returned. If `TRUE`, an extended
#'     information will be returned.
#' @param max_attempts (integer, poistive) - a non-zero integer, maximum
#'     number of additional attempts to call the API if the first attempt fails
#'     (i.e. its call status is different from `200`). Additional attempts are
#'     implemented with an exponential backoff. Defaults to 3.
#'
#' @details TRC-10 are tokens issued by the system contract (as opposed to
#'     TRC-20, which are issued by smart contracts). See
#'     [official documentation](https://developers.tron.network/docs/trc10)
#'     for details.
#'
#' @return A tibble, whose content depends on the `detailed_info` argument. If
#'      `detailed_info = FALSE`, the tibble will have the following 5 columns:
#' * `asset_id` (character) - asset `id`, presented as a set of numbers
#'     (e.g. `"1002762"`);
#' * `owner_address` (character) - address of the asset issuer in `base58`
#'     format;
#' * `abbr` (character) - abbreviated name of the asset;
#' * `asset_name` (character) - full name of the asset
#' * `precision` (character) - precision used to present the asset's balance
#'     (e.g., if it's 6, then one needs to divide the returned balance by 1
#'     million to obtain the actual balance for that asset).
#'
#' If `detailed_info = TRUE`, the returned tibble will have the same 5 columns
#'     as above, and the following additional columns:
#' * `description` (character) - a free-text field describing the asset;
#' * `url` (character) - URL of the project;
#' * `total_supply` (character) - total issued amount of the asset's tokens;
#' * `num` (character) - amount of the asset tokens that one can buy
#'     with `trx_num` TRX tokens (see next point);
#' * `trx_num` (character) - amount of TRX tokens that is required to buy `num`
#'     tokens of the asset (thus, `num / num_trx` is the asset's price during
#'     its ICO);
#' * `ico_start_time` (POSIXct, UTC timezone): date and time of the asset's ICO
#'     start;
#' * `ico_end_time` (POSIXct, UTC timezone): date and time of the asset's ICO
#'     end.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' result <- get_asset_by_id(id = "1002762", detailed_info = TRUE)
#' print(result)
#'
get_asset_by_id <- function(id,
                            only_confirmed = FALSE,
                            detailed_info = FALSE,
                            max_attempts = 3L) {

  stopifnot(is.character(id))
  stopifnot(is.logical(only_confirmed))
  stopifnot(is.logical(detailed_info))
  stopifnot(is.integer(max_attempts) & max_attempts > 0)

  query_params <- list(only_confirmed = tolower(only_confirmed),
                       detailed_info = tolower(detailed_info))

  url <- tronr::build_get_request(base_url = "https://api.trongrid.io",
                                  path = c("v1", "assets", id),
                                  query_parameters = query_params)

  r <- tronr::api_request(url = url, max_attempts = max_attempts)
  data <- r$data[[1]]

  all_attributes <- c("id", "owner_address", "abbr",
                      "name", "description",  "url", "precision",
                      "total_supply", "num", "trx_num",
                      "start_time", "end_time")
  basic_attributes <- c("id", "owner_address", "abbr",
                        "name", "precision")

  if (detailed_info) {

    result <- data[all_attributes]
    result$total_supply <- as.character(gmp::as.bigz(result$total_supply))
    result$num <- as.character(gmp::as.bigz(result$num))
    result$trx_num <- as.character(gmp::as.bigz(result$trx_num))

    result <- result %>%
      unlist() %>%
      tibble::enframe(name = "attribute", value = "value") %>%
      tidyr::pivot_wider(names_from = .data$attribute) %>%
      dplyr::mutate(description = trimws(.data$description),
                    start_time = tronr::from_unix_timestamp(.data$start_time),
                    end_time = tronr::from_unix_timestamp(.data$end_time),
                    total_supply = gmp::as.bigz(.data$total_supply) %>%
                      as.character(),
                    num = gmp::as.bigz(.data$num) %>% as.character(),
                    trx_num = gmp::as.bigz(.data$trx_num) %>%
                      as.character()) %>%
      dplyr::rename(asset_id = .data$id,
                    ico_start_time = .data$start_time,
                    ico_end_time = .data$end_time)

    return(result)
  }

  result <- data[basic_attributes] %>%
    unlist() %>%
    tibble::enframe(name = "attribute", value = "value") %>%
    tidyr::pivot_wider(names_from = .data$attribute) %>%
    dplyr::rename(asset_id = .data$id)

  return(result)

}
