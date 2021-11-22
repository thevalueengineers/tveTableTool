temp_table_columnPercents <- generate_table(
  test,
  row_vars = "agegroup",
  col_var = "s2",
  weight_var = "no_weight",
  variable_labels,
  percents = "columns"
)

temp_table_rowPercents <- generate_table(
  test,
  row_vars = "agegroup",
  col_var = "s2",
  weight_var = "no_weight",
  variable_labels,
  percents = "rows"
)

temp_table_weight1 <- generate_table(
  test,
  row_vars = "agegroup",
  col_var = "s2",
  weight_var = "gender_weight1",
  variable_labels,
  percents = "rows"
)

test_that("generate tables gives the right column names", {
  expect_equal(
    names(temp_table_columnPercents),
    c("Variable",
      "Label",
      "Value",
      "Total",
      "AnotherGender/Non-binary",
      "Female",
      "Male")
    )
})

test_that("sum of Total col is 1 when running column percents", {
  expect_equal(sum(temp_table_columnPercents$Total), 1)
})

test_that("Total column is the sum of rows when running row percents", {
  expect_equal(
    rowSums(temp_table_rowPercents[, 5:7]),
    temp_table_rowPercents$Total
  )
})
