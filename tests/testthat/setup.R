# generate_table ----
#temporary tables for testing

#single code only
temp_single_table <- generate_table(
  testTableData,
  row_vars = "agegroup",
  col_var = "s2",
  weight_var = "gender_weight1",
  variable_labels = tveDataLoader::get_varLabels(testTableData),
  value_labels = tveDataLoader::get_valLabels(testTableData)
)

#multi code only
temp_multi_table <- generate_table(
  testTableData,
  row_vars = paste0("s1_", sprintf("%02d", c(01:08, 99))),
  col_var = "s2",
  weight_var = "gender_weight1",
  variable_labels = tveDataLoader::get_varLabels(testTableData),
  value_labels = tveDataLoader::get_valLabels(testTableData)
)

#numeric only
temp_numeric_table <- generate_table(
  testTableData,
  row_vars = "s3",
  col_var = "s2",
  weight_var = "gender_weight1",
  variable_labels = tveDataLoader::get_varLabels(testTableData),
  value_labels = tveDataLoader::get_valLabels(testTableData)
)

#combined with single, multi and numeric
temp_combined_table <- generate_table(
  testTableData,
  row_vars = c(paste0("s1_", sprintf("%02d", c(01:08, 99))),"s2", "s3"),
  col_var = "s2",
  weight_var = "gender_weight1",
  variable_labels = tveDataLoader::get_varLabels(testTableData),
  value_labels = tveDataLoader::get_valLabels(testTableData)
)

#unweighted combined table
temp_combined_table_unweighted <- generate_table(
  testTableData,
  row_vars = c(paste0("s1_", sprintf("%02d", c(01:08, 99))),"s2", "s3"),
  col_var = "s2",
  weight_var = NULL,
  variable_labels = tveDataLoader::get_varLabels(testTableData),
  value_labels = tveDataLoader::get_valLabels(testTableData)
)
