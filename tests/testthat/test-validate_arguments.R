test_that("validate_arguments throws errors as expected", {

  expect_error(validate_arguments(arg_address = 1L))

  expect_error(validate_arguments(arg_asset_id = 104056))

  expect_error(validate_arguments(arg_asset_id = as.factor("Tronix")))

  expect_error(validate_arguments(arg_tx_id = 1L))

  expect_error(validate_arguments(arg_event_name = as.factor("Transfer")))

  expect_error(validate_arguments(arg_block_number = 1020304))

  expect_error(validate_arguments(arg_only_confirmed = "TRUE"))

  expect_error(validate_arguments(arg_only_unconfirmed = "TRUE"))

  expect_error(validate_arguments(arg_only_confirmed = TRUE,
                                  arg_only_unconfirmed = FALSE))

  expect_error(validate_arguments(arg_only_to = "TRUE"))

  expect_error(validate_arguments(arg_only_from = "TRUE"))

  expect_error(validate_arguments(arg_min_timestamp = as.factor("1604188800")))

  expect_error(validate_arguments(arg_max_timestamp = as.factor("1604188800")))

  expect_error(validate_arguments(arg_min_timestamp = "abc"))

  expect_error(validate_arguments(arg_max_timestamp = "abc"))

  expect_error(validate_arguments(arg_detailed_info = "TRUE"))

  expect_error(validate_arguments(arg_order_by = 1L))

  expect_error(validate_arguments(arg_direction = 1L))

  expect_error(validate_arguments(arg_direction = "abc"))

  expect_error(validate_arguments(arg_max_attempts = "1L"))

  expect_error(validate_arguments(arg_max_attempts = -1L))

})


test_that("validate_arguments returns nothing as expected", {

  expect_null(
    validate_arguments(arg_address = "TAUN6FwrnwwmaEqYcckffC7wYmbaS6cBiX",
                       arg_only_confirmed = TRUE,
                       arg_only_unconfirmed = NULL,
                       arg_only_to = FALSE,
                       arg_only_from = FALSE,
                       arg_min_timestamp = "1604188800000",
                       arg_max_timestamp = NULL,
                       arg_contract_address = NULL,
                       arg_max_attempts = 3L)
  )

})
