data(test_list)

test_that("function works as expected with defaults", {

  result <- create_tabulations(
    loaded_data = test_list$loaded_data,
    var_labels = test_list$var_labels,
    val_labels = test_list$val_labels
  )

  expect_named(result,
               c("var_types", "means_table",
                 "n_table", "colprops_table", "base_n_table", "base_props_table"))

  expect_equal(result$var_types |> dim(),
               c(6, 2))

  expect_equal(result$means_table |> dim(),
               c(2, 3))

  expect_equal(result$n_table |> dim(),
               c(15, 4))

  expect_equal(result$colprops_table |> dim(),
               c(15, 4))

  expect_true(all(sapply(result[1:4], methods::is, 'tbl_df')))

  # Or if testing for NA values:
  expect_true(all(is.na(result[[5]])))
  expect_true(all(is.na(result[[6]])))
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


test_that("variable types and frequency tables are returned if no mean score table is produced",{

  num_var_mask <- c('numvar_1', 'numvar_2', 'metavar_1', 'mcvar_1', 'mcvar_2')
  valid_mask <- !colnames(test_list$loaded_data) %in% num_var_mask

  reduced_data <- test_list$loaded_data[, valid_mask]
  reduced_var_labels <- test_list$var_labels[!test_list$var_labels$var_name %in% num_var_mask, ]
  reduced_val_labels <- test_list$val_labels[!test_list$val_labels$var_name %in% num_var_mask, ]
  reduced_no_val_labels <- test_list$no_val_labels[!test_list$no_val_labels$var_name %in% num_var_mask, ]

  result <- create_tabulations(
    loaded_data = reduced_data,
    var_labels = reduced_var_labels,
    val_labels = reduced_val_labels,
    no_val_labels = reduced_no_val_labels)

  expect_null(result$means_table)
  expect_equal(dim(result$var_types),
               c(8, 2))
  expect_equal(dim(result$n_table),
               c(31, 4))
  expect_equal(dim(result$colprops_table),
               c(31, 4))
})


test_that("variable types and mean tables are returned if no frequency tables are produced",{

  freq_var_mask <- c('charvar_1', 'charvar_2', 'mcvar_1', 'mcvar_2',
                     'mcvar_3', 'mcvar_4',
                     'scvar_1', 'scvar_2')
  valid_mask <- !colnames(test_list$loaded_data) %in% freq_var_mask

  reduced_data <- test_list$loaded_data[, valid_mask]
  reduced_var_labels <- test_list$var_labels[!test_list$var_labels$var_name %in% freq_var_mask, ]
  reduced_val_labels <- test_list$val_labels[!test_list$val_labels$var_name %in% freq_var_mask, ]
  reduced_no_val_labels <- test_list$no_val_labels[!test_list$no_val_labels$var_name %in% freq_var_mask, ]

  result <- create_tabulations(
    loaded_data = reduced_data,
    var_labels = reduced_var_labels,
    val_labels = reduced_val_labels,
    no_val_labels = reduced_no_val_labels)

  expect_equal(dim(result$var_types),
               c(5, 2))
  expect_equal(dim(result$means_table),
               c(3, 3))
  expect_null(result$n_table)
  expect_null(result$colprops_table)

})


test_that("variable types table returned if no other table is possible",{

  meta_var_mask <- c('respid', 'weight')
  valid_mask <- colnames(test_list$loaded_data) %in% meta_var_mask

  reduced_data <- test_list$loaded_data[, valid_mask]
  reduced_var_labels <- test_list$var_labels[test_list$var_labels$var_name %in% meta_var_mask, ]
  reduced_val_labels <- test_list$val_labels[test_list$val_labels$var_name %in% meta_var_mask, ]
  reduced_no_val_labels <- test_list$no_val_labels[test_list$no_val_labels$var_name %in% meta_var_mask, ]

  result <- create_tabulations(
    loaded_data = reduced_data,
    var_labels = reduced_var_labels,
    val_labels = reduced_val_labels,
    no_val_labels = reduced_no_val_labels)

  expect_equal(dim(result$var_types),
               c(2, 2))
  expect_null(dim(result$means_table))
  expect_null(result$n_table)
  expect_null(result$colprops_table)

})

test_that("base table outputs meet expectations ",{

  result <- create_tabulations(
    loaded_data = test_list$loaded_data,
    var_labels = test_list$var_labels,
    val_labels = test_list$val_labels,
    col_var = "scvar_1")

  expect_s3_class(result$base_n_table, "data.frame")
  expect_s3_class(result$base_props_table, "data.frame")

})


