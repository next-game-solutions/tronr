#' Get assets by their common name
#'
#' Returns a list of all TRC-10 assets on the chain based on their common name
#'
#' @param name (character) - common name of the TRC-10 asset (e.g., `Tronix`).
#' @param order_by order_by (character) - specifies the variable to order by.
#'     One of: `total_supply`, `ico_start_time`, `ico_end_time`, `id`.
#' @param direction direction (character) - specifies the direction of ordering the
#'     results - descending (`desc`) or ascending (`asc`).
#' @param only_confirmed (boolean) - if `TRUE`, returns all assets with a given
#'     `name` as of the latest confirmed block, otherwise as of the
#'     latest unconfirmed one. Defaults to `FALSE`.
#' @param max_attempts max_attempts (integer, poistive) - a non-zero integer, maximum
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
#' As there can be several TRC-10 assets with the same name, the number of
#' rows in the return tibble can be >1.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples r <- get_assets_by_name(name = "Tronix")
#' print(r)
#'
get_assets_by_name <- function(name = "Tronix",
                               order_by = "total_supply",
                               direction = "desc",
                               only_confirmed = FALSE,
                               max_attempts = 3L) {

  if (!is.character(name)) {
    rlang::abort("`name` must be a character value")
  }

  if (!is.character(order_by)) {
    rlang::abort("`order_by` must be a character value")
  }

  correct_args <- c("total_supply",
                    "ico_start_time",
                    "ico_end_time",
                    "id")

  if (!order_by %in% correct_args) {
    rlang::abort(c("`order_by` must be one of:", correct_args))
  }

  if (!is.character(direction)) {
    rlang::abort("`direction` must be a character value")
  }

  if (!direction %in% c("asc", "desc")) {
    rlang::abort(c("`order_by` must be one of:", c("asc", "desc")))
  }

  if (!is.logical(only_confirmed)) {
    rlang::abort(c("`only_confirmed` must be one of:", c(TRUE, FALSE)))
  }

  if (!(is.integer(max_attempts) & max_attempts > 0)) {
    rlang::abort("`max_attempts` must be a positive integer")
  }

  if (!(is.integer(max_attempts) & max_attempts > 0)) {
    rlang::abort("`max_attempts` must be a positive integer")
  }


  query_params <- list(order_by = paste(order_by, direction, sep = ","),
                       only_confirmed = only_confirmed,
                       limit = 20L)

  url <- tronr::build_get_request(base_url = "https://api.trongrid.io",
                                  path = c("v1", "assets", name, "list"),
                                  query_parameters = query_params)

  data <- list()
  p <- 1
  while (TRUE) {
    message("Reading page ", p, "...")
    r <- tronr::api_request(url = url, max_attempts = max_attempts)
    data <- c(data, r$data)
    if (is.null(r$meta$fingerprint)) {break}
    p <- p + 1
    url <- r$meta$links$`next`
  }

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
      request_time = tronr::from_unix_timestamp(r$meta$at, tz = "UTC")
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
