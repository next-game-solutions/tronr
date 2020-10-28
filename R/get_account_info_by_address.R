#' Get account information by address
#'
#' Returns account details based on its address
#'
#' @param address A character value - address of the account of interest, in
#'     `base58` (starts with `T`) or  `hex` (starts with `41`) format.
#' @param only_confirmed A boolean value. If `TRUE`, account details will be
#'     returned as of the latest confirmed block, otherwise as of the
#'     latest unconfirmed one. Defaults to `FALSE`.
#' @param max_attempts An integer specifying the maximum number of attempts
#'     to call the API if the first attempt fails (i.e. its call status is
#'     different from `200`). Additional attempts are implemented with an
#'     exponential backoff. Defaults to 3.
#'
#' @return
#' @export
#'
#' @examples
#' address <- "TQjaZ9FD473QBTdUzMLmSyoGB6Yz1CGpux"
#'
get_account_info_by_address <- function(address,
                                        only_confirmed = FALSE,
                                        max_attempts = 3L) {

  stopifnot(is.character(address))
  stopifnot(is.logical(only_confirmed))
  stopifnot(is.numeric(max_attempts))

  base_url <- "https://api.trongrid.io"
  url <- httr::modify_url(base_url, path = c("v1", "accounts", address))
  url <- httr::parse_url(url)
  url$query <- list(only_confirmed = tolower(only_confirmed))
  url <- httr::build_url(url)

  # Call API with an exponential backoff if there is a failure:
  for (attempt in seq_len(max_attempts)) {

    r <- try(httr::GET(url), silent = FALSE)

    if (class(r) == "try-error" | httr::http_error(r)) {
      delay <- runif(n = 1, min = 0, max = 2^attempt - 1)
      message("API request failed. Retrying after ",
              round(delay, 2), " seconds...")
      Sys.sleep(delay)
    } else {break}

  }

  if (httr::http_error(r)) {

    parsed <- jsonlite::fromJSON(
      httr::content(r, "text"),
      simplifyVector = FALSE)

    stop(
      sprintf("API request failed [status code %s].\n%s",
              httr::status_code(r),
              parsed$error),
      call. = FALSE
    )

  }

  parsed <- jsonlite::fromJSON(
    httr::content(r, "text"),
    simplifyVector = FALSE)

  meta <- tibble::tibble(
    request_at = from_unix_timestamp(parsed$meta$at),
    address = parsed$data[[1]]$address,
    type = parsed$data[[1]]$type,
    balance = parsed$data[[1]]$balance,
    account_resource = list(parsed$data[[1]]$account_resource)
  )

  trc20 <- parsed$data[[1]]$trc20 %>%
    unlist() %>%
    tibble::enframe(name = "trc20", value = "balance")

  result <- tibble::tibble(meta, trc20 = list(trc20))

  return(result)

}
