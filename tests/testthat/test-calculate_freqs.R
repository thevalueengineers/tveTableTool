data(test_list)

test_that("function works as expected with all defaults", {

  new_out <- calculate_freqs(input_data = test_list$loaded_data[, c(1, 7, 9)],
                             respid_var = 'respid',
                             var_labels = test_list$var_labels,
                             val_labels = test_list$val_labels)

  expected_out <- list(
    output_coln = tibble::tibble(
      Variable = c(rep('mcvar_1', 2), rep('scvar_1', 4)),
      Label = c(rep('Multi Code Variable 1', 2),
                rep('Single Code Variable 1', 4)),
      Value = c('No', 'Yes', 'Value label A', 'Value label C',
                'Value label D', 'Value label E'),
      Total = c(3, 7, 3, 1, 3, 3)
    ),
    output_colprops = tibble::tibble(
      Variable = c(rep('mcvar_1', 2), rep('scvar_1', 4)),
      Label = c(rep('Multi Code Variable 1', 2),
                rep('Single Code Variable 1', 4)),
      Value = c('No', 'Yes', 'Value label A', 'Value label C',
                'Value label D', 'Value label E'),
      Total = c(0.3, 0.7, 0.3, 0.1, 0.3, 0.3)
    )
  )

  expect_identical(new_out, expected_out)

})



test_that("function works with cross-tabulation", {

  cross_tab_out <- calculate_freqs(
    input_data = test_list$loaded_data[, c(1, 5, 7:9)],
    respid_var = 'respid',
    var_labels = test_list$var_labels,
    val_labels = test_list$val_labels,
    col_var = 'scvar_1'
  )

  # Verify that cross-tabulation adds additional columns
  expect_equal(dim(cross_tab_out$output_coln), c(15, 8))
  expect_equal(dim(cross_tab_out$output_colprops), c(15, 8))
})

test_that("function works with specific freq_vars", {
  specific_vars_out <- calculate_freqs(
    input_data = test_list$loaded_data[, c(1, 5, 7, 9)],
    respid_var = 'respid',
    var_labels = test_list$var_labels,
    val_labels = test_list$val_labels,
    freq_vars = 'mcvar_1'
  )

  # Verify that only specified variables are included
  expect_true(all(specific_vars_out$output_coln$Variable == 'mcvar_1'))
  expect_true(all(specific_vars_out$output_colprops$Variable == 'mcvar_1'))
})

test_that("function returns data.table when tibble_out is FALSE", {
  dt_out <- calculate_freqs(
    input_data = test_list$loaded_data[, c(1, 5, 7, 9)],
    respid_var = 'respid',
    var_labels = test_list$var_labels,
    val_labels = test_list$val_labels,
    tibble_out = FALSE
  )

  # Verify that output is data.table
  expect_true(data.table::is.data.table(dt_out$output_coln))
  expect_true(data.table::is.data.table(dt_out$output_colprops))
})

test_that("function handles empty value labels", {
  # Create test data with empty value labels
  test_val_labels <- test_list$val_labels
  test_val_labels$val_label[1] <- ""

  empty_label_out <- calculate_freqs(
    input_data = test_list$loaded_data[, c(1, 5, 7, 9)],
    respid_var = 'respid',
    var_labels = test_list$var_labels,
    val_labels = test_val_labels,
    col_var = 'scvar_1'
  )

  # Verify that empty labels are replaced with 'blank'
  expect_true(any(empty_label_out$output_coln$Value == 'blank'))
})

test_that("function handles missing value labels", {
  # Create test data with missing value labels
  test_val_labels <- test_list$val_labels
  test_val_labels$val_label[1] <- NA_character_

  missing_label_out <- calculate_freqs(
    input_data = test_list$loaded_data[, c(1, 5, 7, 9)],
    respid_var = 'respid',
    var_labels = test_list$var_labels,
    val_labels = test_val_labels
  )

  # Verify that missing labels are handled appropriately
  expect_false(any(is.na(missing_label_out$output_coln$Value)))
})


test_that("test that values in val_labels can be any type of numeric ", {

  int_val_labels <- test_list$val_labels
  int_val_labels$val_value <- as.integer(int_val_labels$val_value)

  new_out_numeric <- calculate_freqs(input_data = test_list$loaded_data[, c(1, 4, 7:8)],
                                     respid_var = 'respid',
                                     var_labels = test_list$var_labels,
                                     val_labels = test_list$val_labels)

  new_out_integer <- calculate_freqs(input_data = test_list$loaded_data[, c(1, 4, 7:8)],
                                     respid_var = 'respid',
                                     var_labels = test_list$var_labels,
                                     val_labels = int_val_labels)

  expect_identical(new_out_numeric, new_out_integer)

})
