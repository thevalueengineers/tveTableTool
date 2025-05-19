data("test_tve_data")
loaded_data <- test_tve_data$loaded_data |> data.table::copy()
var_labels <- test_tve_data$var_labels |> data.table::copy()
no_var_labels <- test_tve_data$no_var_labels |> data.table::copy()
val_labels <- test_tve_data$val_labels |> data.table::copy()
no_val_labels <- test_tve_data$no_val_labels |> data.table::copy()
respid_var <- 'respid'


create_tabulations(
  loaded_data = loaded_data,
  var_labels = var_labels,
  val_labels = val_labels,
  col_var = "lab_var2",
  weight_var = NULL
)



