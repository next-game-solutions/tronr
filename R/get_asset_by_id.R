#' Get description of a TRC-10 asset
#'
#' Returns information about a TRC-10 token based on its ID
#'
#' @eval function_params(c("asset_id", "only_confirmed",
#'                         "detailed_info", "max_attempts"))
#'
#' @details TRC-10 are tokens issued by the system contract (as opposed to
#'     TRC-20, which are issued by smart contracts). See
#'     [official documentation](https://developers.tron.network/docs/trc10)
#'     for details.
#'
#' @return A tibble, whose content depends on the `detailed_info` argument. If
#'      `detailed_info = FALSE` (default), the tibble will have the following 5
#'      columns:
#' * `asset_id` (character): same as argument `asset_id`;
#' * `owner_address` (character): address of the asset issuer, in `base58`
#'     format;
#' * `abbr` (character): abbreviated name of the asset;
#' * `asset_name` (character): full name of the asset
#' * `precision` (integer): precision used to present the asset's balance
#'     (e.g., if it is 6, then one needs to divide the returned balance by 1
#'     million to obtain the actual balance of that asset).
#'
#' If `detailed_info = TRUE`, the returned tibble will have the same 5 columns
#'     as above and the following additional columns:
#' * `description` (character): a free-text field describing the asset;
#' * `url` (character): URL of the token's project;
#' * `total_supply` (character): total issued amount of tokens;
#' * `num` (character): amount of the asset's tokens that one can buy
#'     with `trx_num` TRX tokens (see next point);
#' * `trx_num` (character): amount of TRX tokens that is required to buy `num`
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
#' r <- get_asset_by_id(asset_id = "1002762", detailed_info = TRUE)
#' print(r)
get_asset_by_id <- function(asset_id,
                            only_confirmed = NULL,
                            detailed_info = FALSE,
                            max_attempts = 3L) {
  tronr::validate_arguments(
    arg_asset_id = asset_id,
    arg_only_confirmed = only_confirmed,
    arg_detailed_info = detailed_info,
    arg_max_attempts = max_attempts
  )

  query_params <- list(
    only_confirmed = tolower(only_confirmed),
    detailed_info = tolower(detailed_info)
  )

  url <- tronr::build_get_request(
    base_url = "https://api.trongrid.io",
    path = c("v1", "assets", asset_id),
    query_parameters = query_params
  )

  r <- tronr::api_request(url = url, max_attempts = max_attempts)
  data <- r$data[[1]]

  all_attributes <- c(
    "id", "owner_address", "abbr",
    "name", "description", "url", "precision",
    "total_supply", "num", "trx_num",
    "start_time", "end_time"
  )
  basic_attributes <- c(
    "id", "owner_address", "abbr",
    "name", "precision"
  )

  if (detailed_info) {
    result <- data[all_attributes]
    result$owner_address <- tronr::convert_address(result$owner_address)

    result <- result %>%
      tibble::as_tibble(.data, .name_repair = "minimal") %>%
      dplyr::mutate(
        id = as.character(.data$id),
        description = trimws(.data$description),
        start_time = tronr::from_unix_timestamp(.data$start_time),
        end_time = tronr::from_unix_timestamp(.data$end_time),
        total_supply = as.numeric(.data$total_supply),
        num = as.numeric(.data$num),
        trx_num = as.numeric(.data$trx_num),
        precision = as.numeric(.data$precision)
      ) %>%
      dplyr::rename(
        asset_id = .data$id,
        ico_start_time = .data$start_time,
        ico_end_time = .data$end_time
      )

    return(result)
  }

  result <- data[basic_attributes] %>%
    unlist() %>%
    tibble::enframe(name = "attribute", value = "value") %>%
    tidyr::pivot_wider(names_from = .data$attribute) %>%
    dplyr::mutate(owner_address = tronr::convert_address(.data$owner_address),
                  precision = as.numeric(.data$precision)) %>%
    dplyr::rename(asset_id = .data$id)


  return(result)
}
