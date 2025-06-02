
#' Create all tabulations
#'
#' @export
create_tabulations <- function(loaded_data,
                               var_labels,
                               val_labels,
                               no_val_labels = NULL,
                               binary_labels = c('No' = 0, 'Yes' = 1),
                               respid_var = 'respid',
                               col_var = NULL,
                               weight_var = NULL) {

  # if no weight provided (i.e. weight_var == NULL) then add a dummy weight of 1
  # and set weight_var to "weight"
  if(isTRUE(is.null(weight_var))){
    weight_var <- "weight"
    loaded_data[, (weight_var) := 1]
  }

  meta_vars <- c(respid_var, weight_var)

  # Identify variable types
  var_types <- classify_variables(loaded_data = loaded_data,
                                  val_labels = val_labels,
                                  no_val_labels = no_val_labels,
                                  meta_vars = meta_vars,
                                  binary_labels = binary_labels)

  # subset variables by type for aggregation
  mean_dt <- var_types[type %in% c('multi', 'numeric')]
  freqs_dt <- var_types[type %in% c('single', 'multi', 'character')]

  # Conditional calculations for numeric and multi-code variables
  if(isTRUE(nrow(mean_dt) > 0)) {
    mean_mask <- c(meta_vars,
                   col_var,
                   mean_dt[['var_name']]) |>
      unique()

    # Numeric and multi-code calculations
    mean_calcs_result <- calculate_means(
      input_data = loaded_data[, ..mean_mask],
      col_var = col_var,
      respid_var = respid_var,
      weight_var = weight_var,
      val_labels = val_labels,
      var_labels = var_labels)
  } else {
    mean_calcs_result <- NULL
  }

  if(isTRUE(nrow(mean_dt) > 0)) {
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
      var_labels = var_labels)
  } else {
    freq_calcs_result <- NULL
  }

  list(
    var_types = var_types,
    means_table = mean_calcs_result,
    n_table = freq_calcs_result$output_coln,
    colprops_table = freq_calcs_result$output_colprops
  )

}






