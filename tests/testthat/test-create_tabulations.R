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

})
