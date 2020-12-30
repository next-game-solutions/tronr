#' Get description of TRC-10 assets
#'
#' Returns a list of TRC-10 assets based on their name
#'
#' @eval function_params(c("asset_name", "order_by", "direction",
#'                         "only_confirmed", "max_attempts"))
#'
#' @details TRC-10 are tokens issued by the system contract (as opposed to
#'     TRC-20, which are issued by smart contracts). See
#'     [official documentation](https://developers.tron.network/docs/trc10)
#'     for details.
#'
#' TRC-10 assets on the TRON chain are _not_ required to have unique names. As
#' a result, the number of rows in the tibble returned by this function
#' can be >1.
#'
#' @return A tibble with the following columns:
#' * `request_time` (POSIXct): date and time when the API request was made;
#' * `asset_id` (character): asset `id`, presented as a set of numbers
#'     (e.g. `1002762`);
#' * `owner_address` (character): address of the asset issuer, in `base58check`
#'     format;
#' * `abbr` (character): abbreviated name of the asset;
#' * `asset_name` (character): full name of the asset
#' * `precision` (double): precision used to present the asset's balance
#'     (e.g., if it is 6, then one needs to divide the returned balance by 1
#'     million to obtain the actual balance for that asset).
#' * `description` (character): a free-text field describing the asset;
#' * `url` (character): URL of the asset's project;
#' * `total_supply` (double): total issued amount of tokens;
#' * `num` (double): amount of the asset tokens that one can buy
#'     with `trx_num` TRX tokens (see next point);
#' * `trx_num` (double): amount of TRX tokens that is required to buy `num`
#'     tokens of the asset (thus, `num / num_trx` is the asset's price during
#'     its ICO);
#' * `ico_start_time` (POSIXct, UTC timezone): date and time of the asset's ICO
#'     start;
#' * `ico_end_time` (POSIXct, UTC timezone): date and time of the asset's ICO
#'     end.
#' * `vote_score` (double): vote score.
#'
#' If no assets are found with the requested `name`, nothing (`NULL`) is
#' returned, with a console message `"No data found"`.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' r <- get_assets_by_name(asset_name = "Tronix")
#' print(r)
get_assets_by_name <- function(asset_name = "Tronix",
                               order_by = "total_supply",
                               direction = "desc",
                               only_confirmed = NULL,
                               max_attempts = 3L) {
  tronr::validate_arguments(
    arg_asset_name = asset_name,
    arg_order_by = order_by,
    arg_direction = direction,
    arg_only_confirmed = only_confirmed,
    arg_max_attempts = max_attempts
  )

  correct_order_by_vals <- c(
    "total_supply",
    "ico_start_time",
    "ico_end_time",
    "id"
  )

  if (!order_by %in% correct_order_by_vals) {
    rlang::abort(c("`order_by` must be one of:", correct_order_by_vals))
  }

  query_params <- list(
    order_by = paste(order_by, direction, sep = ","),
    only_confirmed = only_confirmed,
    limit = 20L
  )

  url <- tronr::build_get_request(
    base_url = "https://api.trongrid.io",
    path = c("v1", "assets", asset_name, "list"),
    query_parameters = query_params
  )

  request_time <- Sys.time()
  attr(request_time, "tzone") <- "UTC"

  data <- tronr::run_paginated_query(url = url, max_attempts = max_attempts)

  if (is.null(data)) {
    return(data)
  }

  result <- lapply(data, tibble::as_tibble, .repair_names = "minimal") %>%
    dplyr::bind_rows() %>%
    dplyr::group_by(.data$id) %>%
    dplyr::mutate(
      num = as.numeric(.data$num),
      trx_num = as.numeric(.data$trx_num),
      precision = as.numeric(.data$precision),
      description = ifelse(nchar(.data$description) <= 1L,
        NA_character_, .data$description
      ),
      id = as.character(.data$id),
      abbr = ifelse(nchar(.data$abbr) <= 1L, NA_character_, .data$abbr),
      url = ifelse(nchar(.data$url) <= 1L ||
        .data$url == "N/A" ||
        .data$url == "http://" ||
        .data$url == "http://...",
      NA_character_, .data$url
      ),
      start_time = tronr::from_unix_timestamp(.data$start_time),
      end_time = tronr::from_unix_timestamp(.data$end_time),
      total_supply = as.numeric(.data$total_supply),
      request_time = request_time,
      vote_score = as.numeric(.data$vote_score)
    ) %>%
    dplyr::mutate(owner_address = tronr::convert_address(.data$owner_address)) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      request_time = .data$request_time,
      asset_id = .data$id,
      owner_address = .data$owner_address,
      abbr = .data$abbr,
      asset_name = .data$name,
      precision = .data$precision,
      description = .data$description,
      url = .data$url,
      total_supply = .data$total_supply,
      num = .data$num,
      trx_num = .data$trx_num,
      ico_start_time = .data$start_time,
      ico_end_time = .data$end_time,
      vote_score = .data$vote_score
    )

  return(result)
}
