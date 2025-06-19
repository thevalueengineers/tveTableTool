devtools::load_all()

result <- create_tabulations(
  loaded_data = test_list$loaded_data,
  var_labels = test_list$var_labels,
  val_labels = test_list$val_labels,
  col_var = "scvar_1"
)

library(dplyr)

subgroup_mask <- c("Value label A", "Value label C", "Value label D", "Value label E")

out <- result$colprops_table |>
  group_by(Variable) |>
  mutate(
    grand_total_mean = mean(c_across(all_of(subgroup_mask)), na.rm = TRUE)
  ) |>
  ungroup() |>
  # Calculate row means (mean across the value columns for each row)
  rowwise() |>
  mutate(
    rowMean = mean(c_across(all_of(subgroup_mask)), na.rm = TRUE)
  ) |>
  ungroup() |>
  # Calculate column means (mean down each value column)
  group_by(Variable) |>
  mutate(
    across(all_of(subgroup_mask),
           ~ mean(.x, na.rm = TRUE),
           .names = "colMean_{.col}")
  ) |>
  ungroup() |>
  # Apply adjustment calculation for each value column using across
  group_by(Variable) |>
  mutate(
    across(all_of(subgroup_mask),
           ~ (.x - get(paste0("colMean_", cur_column())) - rowMean + grand_total_mean) * 100,
           .names = "{.col}_di")
  )

library(data.table)

double_index_function <- function()
subgroup_mask <- c("Value label A", "Value label C", "Value label D", "Value label E")

# Convert to data.table
dt <- as.data.table(result$colprops_table)

# Calculate grand total mean by Variable
dt[, grand_total_mean := mean(unlist(.SD), na.rm = TRUE),
   by = Variable, .SDcols = subgroup_mask]

# Calculate row means
dt[, rowMean := mean(unlist(.SD), na.rm = TRUE),
   by = 1:nrow(dt), .SDcols = subgroup_mask]

# Calculate column means by Variable and create column mean columns
for(col in subgroup_mask) {
  col_mean_name <- paste0("colMean_", col)
  dt[, (col_mean_name) := mean(get(col), na.rm = TRUE), by = Variable]
}

# Apply adjustment calculation for each value column
for(col in subgroup_mask) {
  col_mean_name <- paste0("colMean_", col)
  adjusted_name <- paste0(col, "_di")
  dt[, (adjusted_name) := (get(col) - get(col_mean_name) - rowMean + grand_total_mean) * 100]
}

# Unselect the temporary columns used for calculations

dt[, c("grand_total_mean", "rowMean", paste0("colMean_", subgroup_mask)) := NULL]

