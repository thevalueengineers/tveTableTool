data(test_list)

test_that("function with defaults and tibble inputs works as expected", {
  new_types <- classify_variables(loaded_data = test_list$loaded_data,
                                val_labels = test_list$val_labels)

  expect_identical(new_types,
                   tibble::tibble(var_name = c(paste0('mcvar_', 1:4),
                                             paste0('scvar_', 1:2)),
                                type = c(rep('multi', 2),
                                       rep('single', 4)))
  )
})

test_that("function works with data.table inputs", {
  # Convert test_list components to data.tables
  dt_data <- data.table::as.data.table(test_list$loaded_data)
  dt_labels <- data.table::as.data.table(test_list$val_labels)

  new_types <- classify_variables(loaded_data = dt_data,
                                val_labels = dt_labels)

  expect_identical(new_types,
                   tibble::tibble(var_name = c(paste0('mcvar_', 1:4),
                                             paste0('scvar_', 1:2)),
                                type = c(rep('multi', 2),
                                       rep('single', 4)))
  )
})

test_that("function works with custom binary labels", {
  custom_binary <- c('Present' = 1, 'Not Present' = 0)

  new_types <- classify_variables(loaded_data = test_list$loaded_data,
                                val_labels = test_list$val_labels,
                                binary_labels = custom_binary)

  # mcvar_3 should now be classified as 'multi' due to matching custom binary labels
  expect_identical(new_types,
                   tibble::tibble(var_name = c(paste0('mcvar_', 1:4),
                                             paste0('scvar_', 1:2)),
                                type = c(rep('single', 2),
                                         'multi',
                                       rep('single', 3)))
  )
})

test_that("function works with no_val_labels parameter", {
  no_labels <- data.frame(var_name = c('numvar_1', 'charvar_1'))

  new_types <- classify_variables(loaded_data = test_list$loaded_data,
                                val_labels = test_list$val_labels,
                                no_val_labels = no_labels)

  # Should include numeric and character variables
  expect_true(all(c('numvar_1', 'charvar_1') %in% new_types$var_name))
  expect_equal(new_types[new_types$var_name == 'numvar_1', ]$type, 'numeric')
  expect_equal(new_types[new_types$var_name == 'charvar_1', ]$type, 'character')
})


test_that("test that values in val_labels can be any type of numeric ", {

  new_types_numeric <- classify_variables(loaded_data = test_list$loaded_data,
                                          val_labels = test_list$val_labels)

  int_val_labels <- test_list$val_labels
  int_val_labels$val_value <- as.integer(int_val_labels$val_value)

  new_types_integer <- classify_variables(loaded_data = test_list$loaded_data,
                                  val_labels = int_val_labels)

  expect_identical(new_types_numeric, new_types_integer)

})
