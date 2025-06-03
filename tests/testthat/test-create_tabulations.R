data(test_list)

test_that("function works as expected with defaults", {

  result <- create_tabulations(
    loaded_data = test_list$loaded_data,
    var_labels = test_list$var_labels,
    val_labels = test_list$val_labels
  )

  expect_named(result,
               c("var_types", "means_table",
                 "n_table", "colprops_table"))

  expect_equal(result$var_types |> dim(),
               c(6, 2))

  expect_equal(result$means_table |> dim(),
               c(2, 3))

  expect_equal(result$n_table |> dim(),
               c(15, 4))

  expect_equal(result$colprops_table |> dim(),
               c(15, 4))

  expect_true(all(sapply(result, methods::is, 'tbl_df')))

})


test_that("test that values in val_labels can be any type of numeric ", {

  int_val_labels <- test_list$val_labels
  int_val_labels$val_value <- as.integer(int_val_labels$val_value)

  new_out_numeric <- create_tabulations(
    loaded_data = test_list$loaded_data,
    var_labels = test_list$var_labels,
    val_labels = test_list$val_labels
  )

  new_out_integer <- create_tabulations(
    loaded_data = test_list$loaded_data,
    var_labels = test_list$var_labels,
    val_labels = int_val_labels
  )

  expect_identical(new_out_numeric, new_out_integer)

})
