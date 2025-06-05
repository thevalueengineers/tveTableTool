#' Classify variables by type
#'
#' @description
#' This function analyzes a dataset and its value labels to classify variables into different types:
#' - 'multi': Binary variables with Yes/No or similar labels
#' - 'single': Variables with multiple value labels
#' - 'meta': Metadata variables like respondent IDs or weights
#' - 'numeric': Variables with numeric values (when specified in no_val_labels)
#' - 'character': Variables with character values (when specified in no_val_labels)
#'
#' When variables are specified in no_val_labels, their type is determined by their R class:
#' numeric variables will be classified as 'numeric', and character variables as 'character'.
#'
#' @param loaded_data A data frame or data.table containing the survey data
#' @param val_labels A data frame or data.table containing variable value labels with columns:
#'   'var_name', 'val_label', and 'val_value'
#' @param no_val_labels Optional data frame or data.table containing variables that don't have value labels.
#'   For these variables, their R class will determine their type classification.
#' @param binary_labels Named vector defining binary variable labels (default: c('No' = 0, 'Yes' = 1))
#' @param meta_vars Character vector of variable names to be classified as metadata (default: c('respid', 'weight'))
#' @param tibble_out Logical indicating whether to return a tibble (TRUE) or data.table (FALSE)
#'
#' @return A tibble or data.table with two columns:
#'   - var_name: Name of the variable
#'   - type: Classification type ('multi', 'single', 'meta', 'numeric', or 'character')
#'
#' @examples
#' # Using the test_list dataset
#' data(test_list)
#'
#' # Classify variables with default settings
#' var_types <- classify_variables(
#'   loaded_data = test_list$loaded_data,
#'   val_labels = test_list$val_labels
#' )
#'
#' # Classify variables with custom binary labels
#' custom_types <- classify_variables(
#'   loaded_data = test_list$loaded_data,
#'   val_labels = test_list$val_labels,
#'   binary_labels = c('Present' = 1, 'Not Present' = 0)
#' )
#'
#' # Classify variables including numeric and character variables
#' all_types <- classify_variables(
#'   loaded_data = test_list$loaded_data,
#'   val_labels = test_list$val_labels,
#'   no_val_labels = data.frame(var_name = c('numvar_1', 'charvar_1'))
#' )
#'
#' @export
classify_variables <- function(loaded_data,
                               val_labels,
                               no_val_labels = NULL,
                               binary_labels = c('No' = 0, 'Yes' = 1),
                               meta_vars = c('respid', 'weight'),
                               tibble_out = TRUE) {

  # transform loaded_data into data.tables for efficient processing
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

  var_type <- val_labels |>
    # calculate frequencies to group and identify all possible values
    # across each variable included
    _[, .N, by = c('var_name',
                   'val_label',
                   'val_value')] |>
    # if values and value labels found per variable match those in binary_labels,
    # then classify as multi-code, otherwise single-code
    _[, list(valid_labels = identical(sort(val_label), sort(names(binary_labels))),
             valid_values = isTRUE(all.equal(sort(val_value), unname(sort(binary_labels))))),
      by = 'var_name'] |>
    _[, 'type' := data.table::fifelse(valid_labels & valid_values, 'multi','single')] |>
    _[, c('valid_labels', 'valid_values') := NULL]

  if(isTRUE(!is.null(no_val_labels) && nrow(no_val_labels) > 0)) {

    # if no_val_labels is provided, transform it to data.table for efficient processing
    if(isTRUE(methods::is(no_val_labels, 'data.table'))) {
      no_val_labels <- data.table::copy(no_val_labels)
    } else {
      no_val_labels <- data.table::as.data.table(no_val_labels)
    }

    # append any variables that have no value labels but for which types
    # should be identified
    var_type <- list(var_type,
                     loaded_data[, (no_val_labels[['var_name']]), with = F] |>
                       lapply(class) |>
                       lapply(data.table::as.data.table) |>
                       data.table::rbindlist(idcol = 'var_name') |>
                       data.table::setnames('V1', 'type')
    ) |>
      data.table::rbindlist()

  }

  var_type[var_name %in% meta_vars, 'type' := 'meta']

  # if tibble_out is FALSE, return data.table
  if(isFALSE(tibble_out)) return(var_type)

  data.table::setDF(var_type)
  tibble::as_tibble(var_type)

}
