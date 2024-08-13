#combined with single, multi and numeric
temp_combined_table <- generate_table(
  testTableData,
  row_vars = c(paste0("s1_", sprintf("%02d", c(01:08, 99))),"s2", "s3"),
  col_var = "s2",
  weight_var = "gender_weight1",
  variable_labels = tveDataLoader::get_varLabels(testTableData),
  value_labels = tveDataLoader::get_valLabels(testTableData)
) %>%
  add_index()

test_that("index columns are added", {
  expect_equal(
    names(temp_combined_table),
    c(
      "Variable",
      "label",
      "value",
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
    as.numeric(100 * temp_combined_table[1, 6] / temp_combined_table[1, 4]),
    temp_combined_table[1, ][[9]]
  )
})
