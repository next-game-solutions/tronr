get_trc10_transfers <- function(owner_address = NULL,
                                min_timestamp,
                                max_timestamp,
                                max_attempts = 3L) {
  validate_arguments(
    arg_address = owner_address,
    arg_min_timestamp = min_timestamp,
    arg_max_timestamp = max_timestamp,
    arg_max_attempts = max_attempts
  )

  if (!is.null(owner_address) && (
    substr(owner_address, 1, 2) == "41" | substr(owner_address, 1, 2) == "0x")
  ) {
    owner_address <- convert_address(owner_address)
  }

  data <- run_paginated_tronscan_query(
    base_url = "https://apilist.tronscan.org/",
    path = c("api", "asset", "transfer"),
    params = list(
      sort = "timestamp",
      limit = 25,
      start_timestamp = min_timestamp,
      end_timestamp = max_timestamp,
      issueAddress = owner_address
    ),
    show_spinner = TRUE,
    max_attempts = max_attempts
  )
}
