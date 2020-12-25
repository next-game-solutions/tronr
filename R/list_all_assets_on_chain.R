#' List all assets on the TRON chain
#'
#' Return a list of all TRC-10 assets currently available on the chain
#'
#' @param order_by (character) - specifies the variable to order by.
#'     One of: `total_supply`, `ico_start_time`, `ico_end_time`, `id`.
#' @param direction (character) - specifies the direction of ordering the
#'     results - descending (`desc`) or ascending (`asc`).
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
#' @return A tibble with the following columns:
#' * `request_time` (POSIXct) - time when the API request was made;
#' * `asset_id` (character) - asset `id`, presented as a set of numbers
#'     (e.g. `"1002762"`);
#' * `owner_address` (character) - address of the asset issuer, in `base58`
#'     format;
#' * `abbr` (character) - abbreviated name of the asset;
#' * `asset_name` (character) - full name of the asset
#' * `precision` (integer) - precision used to present the asset's balance
#'     (e.g., if it's 6, then one needs to divide the returned balance by 1
#'     million to obtain the actual balance for that asset).
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
#' * `vote_score` (integer): vote score.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' r <- list_all_assets_on_chain(order_by = "total_supply",
#'                               direction = "desc")
#' print(r)
#'
list_all_assets_on_chain <- function(order_by = "total_supply",
                                     direction = "desc",
                                     max_attempts = 3L) {

  tronr::validate_arguments(arg_order_by = order_by,
                            arg_direction = direction,
                            arg_max_attempts = max_attempts)

  correct_order_by_vals <- c("total_supply",
                             "ico_start_time",
                             "ico_end_time",
                             "id")

  if (!order_by %in% correct_order_by_vals) {
    rlang::abort(c("`order_by` must be one of:", correct_order_by_vals))
  }

  query_params <- list(order_by = paste(order_by, direction, sep = ","),
                       limit = 200L)

  url <- tronr::build_get_request(base_url = "https://api.trongrid.io",
                                  path = c("v1", "assets"),
                                  query_parameters = query_params)

  request_time <- Sys.time()
  attr(request_time, "tzone") <- "UTC"

  data <- tronr::run_paginated_query(url = url, max_attempts = max_attempts)

  if (is.null(data)) {return(data)}

  result <- lapply(data, tibble::as_tibble) %>%
    dplyr::bind_rows() %>%
    dplyr::group_by(.data$id) %>%
    dplyr::mutate(
      num = gmp::as.bigz(.data$num) %>% as.character(),
      trx_num = gmp::as.bigz(.data$trx_num) %>% as.character(),
      description = ifelse(nchar(.data$description) <= 1L,
                           NA_character_, .data$description),
      id = gmp::as.bigz(.data$id) %>% as.character(),
      abbr = ifelse(nchar(.data$abbr) <= 1L, NA_character_, .data$abbr),
      url = ifelse(nchar(.data$url) <= 1L ||
                     .data$url == "N/A" ||
                     .data$url == "http://" ||
                     .data$url == "http://...",
                   NA_character_, .data$url),
      start_time = tronr::from_unix_timestamp(.data$start_time),
      end_time = tronr::from_unix_timestamp(.data$end_time),
      total_supply = gmp::as.bigz(.data$total_supply) %>% as.character(),
      request_time = request_time
    ) %>%
    dplyr::mutate(owner_address = tronr::convert_address(.data$owner_address)) %>%
    dplyr::ungroup() %>%
    dplyr::select(request_time = .data$request_time,
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
                  vote_score = .data$vote_score)

  return(result)

}
