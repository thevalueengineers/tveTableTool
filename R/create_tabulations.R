#' Create all tabulations
#'
#' @description
#' This function creates comprehensive tabulations for survey data, including both frequency
#' tables and mean calculations. It automatically classifies variables and applies appropriate
#' statistical methods based on variable types. The function supports weighted calculations,
#' cross-tabulation, and value labeling.
#'
#' @inheritParams calculate_means
#' @inheritParams calculate_freqs
#' @inheritParams classify_variables
#'
#' @return A list containing:
#'   \itemize{
#'     \item var_types: Classification of variables by type
#'     \item means_table: Table of weighted means
#'     \item n_table: Table of frequency counts
#'     \item colprops_table: Table of proportions
#'   }
#'
#' @examples
#' # Using the test_list dataset
#' data(test_list)
#'
#' # Create tabulations with default settings
#' result <- create_tabulations(
#'   loaded_data = test_list$loaded_data,
#'   var_labels = test_list$var_labels,
#'   val_labels = test_list$val_labels
#' )
#'
#' # Create tabulations with cross-tabulation
#' result_with_col <- create_tabulations(
#'   loaded_data = test_list$loaded_data,
#'   var_labels = test_list$var_labels,
#'   val_labels = test_list$val_labels,
#'   col_var = 'scvar_1'
#' )
#'
#' # Create tabulations with custom weights
#' result_with_weights <- create_tabulations(
#'   loaded_data = test_list$loaded_data,
#'   var_labels = test_list$var_labels,
#'   val_labels = test_list$val_labels,
#'   weight_var = 'weight'
#' )
#'
#' # View the results
#' print(result$var_types)    # Variable classifications
#' print(result$means_table)  # Mean calculations
#' print(result$n_table)      # Frequency counts
#' print(result$colprops_table) # Proportions
#'
#' @export
create_tabulations <- function(loaded_data,
                               var_labels,
                               val_labels,
                               no_val_labels = NULL,
                               binary_labels = c('No' = 0, 'Yes' = 1),
                               respid_var = 'respid',
                               col_var = NULL,
                               weight_var = NULL,
                               meta_vars = c('respid', 'weight'),
                               mean_vars = NULL,
                               freq_vars = NULL,
                               tibble_out = TRUE) {

  # transform input_data into data.table for efficient processing
  if(isTRUE(methods::is(loaded_data, 'data.table'))) {
    loaded_data <- data.table::copy(loaded_data)
  } else {
    loaded_data <- data.table::as.data.table(loaded_data)
  }

  # transform val_labels into data.table for efficient processing
  if(isTRUE(methods::is(val_labels, 'data.table'))) {
    val_labels <- data.table::copy(val_labels)
  } else {
    val_labels <- data.table::as.data.table(val_labels)
  }

  # transform var_labels into data.table for efficient processing
  if(isTRUE(methods::is(var_labels, 'data.table'))) {
    var_labels <- data.table::copy(var_labels)
  } else {
    var_labels <- data.table::as.data.table(var_labels)
  }

  # if no weight provided (i.e. weight_var == NULL) then add a dummy weight of 1
  # and set weight_var to "weight"
  if(isTRUE(is.null(weight_var))){
    weight_var <- "weight"
    loaded_data[, (weight_var) := 1]
  }

  meta_vars <- c(respid_var, weight_var, meta_vars) |>
    unique()

  # Identify variable types
  var_types <- classify_variables(loaded_data = loaded_data,
                                  val_labels = val_labels,
                                  no_val_labels = no_val_labels,
                                  meta_vars = meta_vars,
                                  binary_labels = binary_labels,
                                  tibble_out = FALSE)

  # subset variables by type for aggregation
  mean_dt <- var_types[type %in% c('multi', 'numeric')]
  freqs_dt <- var_types[type %in% c('single', 'multi', 'character')]

  # align type of var_types to overall tibble_out value
  if(isTRUE(tibble_out)) {
    data.table::setDF(var_types)
    var_types <- tibble::tibble(var_types)
  }

  # Conditional calculations for numeric and multi-code variables
  if(isTRUE(nrow(mean_dt) > 0)) {
    colmean_mask <- c(meta_vars,
                   col_var,
                   mean_dt[['var_name']]) |>
      unique()

    # Numeric and multi-code calculations
    mean_calcs_result <- calculate_means(
      input_data = loaded_data[, ..colmean_mask],
      col_var = col_var,
      respid_var = respid_var,
      weight_var = weight_var,
      val_labels = val_labels,
      var_labels = var_labels,
      mean_vars = mean_vars,
      tibble_out = tibble_out)
  } else {
    mean_calcs_result <- NULL
  }

  if(isTRUE(nrow(freqs_dt) > 0)) {
    colfreq_mask <- c(meta_vars,
                      col_var,
                      freqs_dt[['var_name']]) |>
      unique()

    # Numeric and multi-code calculations
    freq_calcs_result <- calculate_freqs(
      input_data = loaded_data[, ..colfreq_mask],
      col_var = col_var,
      respid_var = respid_var,
      weight_var = weight_var,
      val_labels = val_labels,
      var_labels = var_labels,
      freq_vars = freq_vars,
      tibble_out = tibble_out)
  } else {
    freq_calcs_result <- NULL
  }

  if(isFALSE(is.null(col_var))) {

    colfreq_mask <- c(meta_vars,
                      col_var) |>
      unique()

  # Count of column var
  freq_base_result <- calculate_freqs(
    input_data = loaded_data[, ..colfreq_mask],
    col_var = NULL,
    respid_var = respid_var,
    weight_var = weight_var,
    val_labels = val_labels,
    var_labels = var_labels,
    freq_vars = NULL,
    tibble_out = tibble_out)
} else {
  freq_base_result <- NULL
}


  list(
    var_types = var_types,
    means_table = mean_calcs_result,
    n_table = freq_calcs_result$output_coln,
    colprops_table = freq_calcs_result$output_colprops,
    base_n_table = freq_base_result$output_coln,
    base_props_table = freq_base_result$output_colprops
  )

}
