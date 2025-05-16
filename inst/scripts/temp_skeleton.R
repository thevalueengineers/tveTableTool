data("test_tve_data")
loaded_data <- test_tve_data$loaded_data |> data.table::copy()
var_labels <- test_tve_data$var_labels |> data.table::copy()
no_var_labels <- test_tve_data$no_var_labels |> data.table::copy()
val_labels <- test_tve_data$val_labels |> data.table::copy()
no_val_labels <- test_tve_data$no_val_labels |> data.table::copy()
row_vars <- c('char_var1', 'lab_var2', 'lab_var3')
respid_var <- 'respid'


create_tabulations(
  loaded_data = loaded_data,
  var_labels = var_labels,
  val_labels = val_labels,
  row_vars = row_vars,
  col_var = "lab_var2",
  weight_var = NULL
)



