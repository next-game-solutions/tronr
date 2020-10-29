#' Get description of a TRC-10 asset by its ID
#'
#' Returns information about a TRC-10 token based on its ID
#'
#' @param id A character string corresponding to the TRC-10 token of interest
#'     (this ID is composed of numbers, e.g. `"1002762"`).
#' @param only_confirmed A boolean value. If `TRUE`, the asset balance will be
#'     returned as of the latest confirmed block, otherwise as of the
#'     latest unconfirmed one. Defaults to `FALSE`.
#' @param max_attempts A non-zero, positive integer specifying the maximum
#'     number of additional attempts to call the API if the first attempt fails
#'     (i.e. its call status is different from `200`). Additional attempts are
#'     implemented with an exponential backoff. Defaults to 3.
#'
#' @return
#' @export
#'
#' @examples
get_asset_by_id <- function(id,
                            only_confirmed = FALSE,
                            max_attempts = 3L) {

}
