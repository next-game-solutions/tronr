#' Account data parser
#'
#' Extracts information about an account from an API response object
#'
#' @param response A response object as returned by the `httr`'s request verbs
#'     (e.g., `GET` or `POST`).
#' @param return_extra_attributes
#' @param unfold_extra_attributes
#'
#' @return
#' @export
#'
#' @examples
#'
account_info_parser <- function(response,
                                return_extra_attributes = TRUE,
                                unfold_extra_attributes = FALSE) {

  if (!is_application_json(response)) {
    stop("The API response type is not `application/json`", call. = FALSE)
  }

  parsed <- jsonlite::fromJSON(httr::content(response, "text"),
                               simplifyVector = FALSE)

  data <- parsed$data[[1]]
  data_names <- names(data)

  main_attributes <- c("address", "balance", "trc20")
  extra_attributes <- setdiff(data_names, main_attributes)

  if (length(data$trc20) != 0) {
    trc20 <- data$trc20 %>%
      unlist() %>%
      tibble::enframe(name = "trc20", value = "balance") %>%
      list()
  } else (trc20 <- NA)

  result <- tibble(
    request_time = from_unix_timestamp(parsed$meta$at, tz = "UTC"),
    address = data$address,
    balance = as.character(data$balance),
    trc20 = trc20,
    extra_attributes = list(data[extra_attributes])
  )

}
