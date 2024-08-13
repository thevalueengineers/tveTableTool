

#test the sum of all columns equals 1 for single code questions
test_that("sum of values in columns 4 to n is equal to the number of those columns", {
  # Calculate the sum of each column's values
  column_sums <- purrr::map_dbl(
    temp_single_table[, 4:ncol(temp_single_table)],
    ~sum(.x, na.rm = TRUE)
  )

  # Calculate the total sum of these columns
  total_sum <- sum(column_sums)

  # Pull the number of columns we have in the data set minus labels (column 4 onwards)
  num_columns <- ncol(temp_single_table) - 3

  # the sum on the columns should be equal to the number of columns
  expect_equal(total_sum, num_columns)
})

#test that column names match
test_that("generate tables gives the right column names", {
  expect_equal(
    names(temp_combined_table),
    c("Variable",
      "label",
      "value",
      "Total",
      "AnotherGender/Non-binary",
      "Female",
      "Male")
  )
})

#test that column names match - unweighted data
test_that("generate tables gives the right column names", {
  expect_equal(
    names(temp_combined_table_unweighted),
    c("Variable",
      "label",
      "value",
      "Total",
      "AnotherGender/Non-binary",
      "Female",
      "Male")
  )
})

#check that output is a labelled data frame
test_that("temp_combined_table_unweighted is a tibble", {
  # Check if the output object is of class "tbl_df"
  expect_s3_class(temp_combined_table, "tbl_df")

})

#check that output is a labelled data frame - unweighted data
test_that("temp_combined_table_unweighted is a tibble", {
  # Check if the output object is of class "tbl_df"
  expect_s3_class(temp_combined_table_unweighted, "tbl_df")

})
