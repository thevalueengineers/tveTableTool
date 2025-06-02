data(test_list)

test_that("ensure weighted mean calculations works as expected with all defaults", {

  new_out <- calculate_means(input_data = test_list$loaded_data[, c(1, 4, 7:8)],
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
  new_out <- calculate_means(input_data = test_list$loaded_data[, c(1, 4, 7:8)],
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

  new_out <- calculate_means(input_data = test_list$loaded_data[, c(1, 4, 7:8, 13)],
                            respid_var = 'respid',
                            var_labels = test_list$var_labels,
                            val_labels = test_list$val_labels,
                            weight_var = 'weight')

  # Check that output has correct structure
  expect_true(all(c('Variable', 'Label', 'Total') %in% names(new_out)))
  expect_equal(nrow(new_out), 3) # Should have 3 variables
})


test_that("calculate_means returns data.table when tibble_out is FALSE", {
  new_out <- calculate_means(input_data = test_list$loaded_data[, c(1, 4, 7:8)],
                            respid_var = 'respid',
                            var_labels = test_list$var_labels,
                            val_labels = test_list$val_labels,
                            tibble_out = FALSE)

  expect_true(data.table::is.data.table(new_out))
  expect_false(tibble::is_tibble(new_out))
})

test_that("calculate_means throws error for non-numeric variables", {

  expect_error(
    calculate_means(input_data = test_list$loaded_data[, c(1, 4, 5, 7:8)],
                   respid_var = 'respid',
                   var_labels = test_list$var_labels,
                   val_labels = test_list$val_labels,
                   mean_vars = c('numvar_2', 'charvar_1')),
    "Only numeric variables allowed for mean scores"
  )
})




