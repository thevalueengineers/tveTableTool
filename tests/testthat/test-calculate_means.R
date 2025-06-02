data(test_list)

test_that("ensure weighted mean calculations works as expected with all defaults", {

  new_out <- calculate_means(input_data = test_list$loaded_data[, c(1, 4, 7:8), with = F],
                             respid_var = 'respid',
                             var_labels = test_list$var_labels,
                             val_labels = test_list$val_labels)

  expected_out <- tibble::tibble(Variable = c('numvar_2', 'scvar_1', 'scvar_2'),
                                 Label = c('Numeric Variable 2',
                                           'Single Code Variable 1',
                                           'Single Code Variable 2'),
                                 Total = c(58.8, 3.3, 1.6))

  expect_identical(new_out, expected_out)

})

test_that("calculate_means works with column breakdown", {
  new_out <- calculate_means(input_data = test_list$loaded_data[, c(1, 4, 7:8), with = F],
                            respid_var = 'respid',
                            var_labels = test_list$var_labels,
                            val_labels = test_list$val_labels,
                            col_var = 'scvar_1')

  expected_out <- tibble::tibble(Variable = c('numvar_2', 'scvar_2'),
                                 Label = c('Numeric Variable 2',
                                           'Single Code Variable 2'),
                                 Total = c(58.8, 1.6),
                                 `Value label A` = c(61 + 1/3, 2),
                                 `Value label C` = c(44, 1),
                                 `Value label D` = c(65 + 1/3, 1 + 1/3),
                                 `Value label E` = c(54 + 2/3, 1 + 2/3))

  expect_equal(new_out, expected_out)
})

test_that("calculate_means works with custom weights", {

  new_out <- calculate_means(input_data = test_list$loaded_data[, c(1, 4, 7:8, 13), with = F],
                            respid_var = 'respid',
                            var_labels = test_list$var_labels,
                            val_labels = test_list$val_labels,
                            weight_var = 'weight')

  # Check that output has correct structure
  expect_true(all(c('Variable', 'Label', 'Total') %in% names(new_out)))
  expect_equal(nrow(new_out), 3) # Should have 3 variables
})

test_that("calculate_means works with custom mean variables", {
  new_out <- calculate_means(input_data = test_list$loaded_data[, c(1, 4, 7:8), with = F],
                            respid_var = 'respid',
                            var_labels = test_list$var_labels,
                            val_labels = test_list$val_labels,
                            mean_vars = c('numvar_2', 'scvar_1'))

  # Check that only specified variables are included
  expect_equal(nrow(new_out), 2)
  expect_true(all(new_out$Variable %in% c('numvar_2', 'scvar_1')))
})

test_that("calculate_means returns data.table when tibble_out is FALSE", {
  new_out <- calculate_means(input_data = test_list$loaded_data[, c(1, 4, 7:8), with = F],
                            respid_var = 'respid',
                            var_labels = test_list$var_labels,
                            val_labels = test_list$val_labels,
                            tibble_out = FALSE)

  expect_true(data.table::is.data.table(new_out))
  expect_false(tibble::is_tibble(new_out))
})

test_that("calculate_means throws error for non-numeric variables", {
  # Create test data with a character variable
  test_data <- data.table::copy(test_list$loaded_data[, c(1, 4, 7:8), with = F])
  test_data[, char_var := "test"]

  expect_error(
    calculate_means(input_data = test_data,
                   respid_var = 'respid',
                   var_labels = test_list$var_labels,
                   val_labels = test_list$val_labels,
                   mean_vars = c('numvar_2', 'char_var')),
    "Only numeric variables allowed for mean scores"
  )
})

test_that("calculate_means handles NA values correctly", {
  # Create test data with NA values
  test_data <- data.table::copy(test_list$loaded_data[, c(1, 4, 7:8), with = F])
  test_data[1:5, numvar_2 := NA]

  new_out <- calculate_means(input_data = test_data,
                            respid_var = 'respid',
                            var_labels = test_list$var_labels,
                            val_labels = test_list$val_labels)

  # Check that output is still valid
  expect_true(all(is.numeric(new_out$Total)))
  expect_false(any(is.na(new_out$Total)))
})
