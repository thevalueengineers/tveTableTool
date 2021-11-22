temp_table_columnPercents <- generate_table(
  test,
  row_vars = "agegroup",
  col_var = "s2",
  weight_var = "no_weight",
  variable_labels,
  percents = "columns"
) %>%
  add_index()

test_that("index columns are added", {
  expect_equal(
    names(temp_table_columnPercents),
    c(
      "Variable",
      "Label",
      "Value",
      "Total",
      "AnotherGender/Non-binary",
      "Female",
      "Male",
      "AnotherGender/Non-binary_index",
      "Female_index",
      "Male_index"
    )
  )
})

test_that("index is calculated correctly", {
  expect_equal(
    as.numeric(100 * temp_table_columnPercents[1, 6] / temp_table_columnPercents[1, 4]),
    temp_table_columnPercents[1, ][[9]]
  )
})
