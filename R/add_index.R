#' Add index columns to a table
#'
#' Index is calculated by dividing each column by the Total column and
#' multiplying by 100.
#'
#' @param tab Table generated from \code{\link{generate_table}} .
#'
#' @return Tibble as per \code{\link{generate_table}}  with
#' additional columns for indexes
#'
#' @export
add_index <- function(tab) {
  tab %>%
    dplyr::mutate(
      dplyr::across(
        -c(Variable:Total),
        list("index" = ~ .x * 100 / Total)
      )
    )
}

#' Add double index columns to tabulation results
#'
#' @description
#' This function calculates double index values for cross-tabulated survey data by applying
#' the formula: (value - column_average - row_average + grand_average) * 100. The double
#' index helps identify cells that are significantly higher or lower than expected based
#' on marginal distributions. Positive values indicate over-representation while negative
#' values indicate under-representation.
#'
#' @param tab A tibble or data frame containing tabulation results, typically generated
#'   from \code{\link{create_tabulations}}. Must contain columns named 'Variable', 'Label',
#'   'Value', 'Total', and one or more subgroup columns with numeric values.
#'
#' @return A tibble with the same structure as the input table, plus additional columns
#'   with "_di" suffix containing the calculated double index values for each subgroup.
#'   The double index columns are added alongside the original value columns.
#'
#' @details
#' The double index calculation follows these steps:
#' \enumerate{
#'   \item Calculate the grand mean across all cells for each variable
#'   \item Calculate row means (average across subgroups for each row)
#'   \item Calculate column means (average down each subgroup for each variable)
#'   \item Apply the adjustment formula: (cell_value - column_mean - row_mean + grand_mean) * 100
#' }
#'
#' Values are calculated separately for each variable to ensure proper statistical meaning.
#' Missing values (NA) are excluded from all mean calculations.
#'
#' @examples
#' # Using tabulation results from create_tabulations
#' data(test_list)
#'
#' # Create cross-tabulations
#' tabs <- create_tabulations(
#'   loaded_data = test_list$loaded_data,
#'   var_labels = test_list$var_labels,
#'   val_labels = test_list$val_labels,
#'   col_var = 'scvar_1'
#' )
#'
#' # Add double index calculations to proportions table
#' indexed_props <- add_double_index(tabs$colprops_table)
#'
#' # View original and indexed results
#' print(tabs$colprops_table)
#' print(indexed_props)
#'
#' # Double index values interpretation:
#' # Positive values: over-representation in that subgroup
#' # Negative values: under-representation in that subgroup
#' # Values near zero: close to expected distribution
#'
#' @seealso
#' \code{\link{create_tabulations}} for generating the input tabulation tables
#'
#' @export
#'
add_double_index <- function(tab){
  # Get variable columns
  subgroup_mask <- tab |>
    dplyr::select(-Variable, -Label, -Value, -Total) |>
    names()

  # Convert to data.table
  dt <- data.table::as.data.table(tab)

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

  # Un-select the temporary columns used for calculations
  dt[, c("grand_total_mean", "rowMean", paste0("colMean_", subgroup_mask)) := NULL]

  out <- tibble::as_tibble(dt)
}
