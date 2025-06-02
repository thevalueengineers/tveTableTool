#' Calculate weighted frequencies and proportions
#'
#' @description
#' This function calculates weighted frequencies and proportions for categorical variables,
#' with support for cross-tabulation and value labeling. It can handle both weighted and
#' unweighted calculations, and returns both counts and proportions in a structured format.
#'
#' @param input_data A data frame or data.table containing the survey data
#' @param respid_var Character string specifying the respondent ID variable
#' @param var_labels A data frame or data.table containing variable labels with columns:
#'   \itemize{
#'     \item var_name: Variable names
#'     \item var_label: Corresponding variable labels
#'   }
#' @param val_labels A data frame or data.table containing value labels with columns:
#'   \itemize{
#'     \item var_name: Variable names
#'     \item val_value: Value codes
#'     \item val_label: Corresponding value labels
#'   }
#' @param freq_vars Optional character vector of variables to calculate frequencies for.
#'   If NULL, all variables except respid_var and col_var will be used.
#' @param col_var Optional character string specifying a variable for cross-tabulation
#' @param weight_var Optional character string specifying the weight variable
#' @param tibble_out Logical indicating whether to return tibbles (TRUE) or data.tables (FALSE)
#'
#' @return A list containing two elements:
#'   \itemize{
#'     \item output_coln: A data frame/tibble with frequency counts
#'     \item output_colprops: A data frame/tibble with proportions
#'   }
#'   Both tables include columns for Variable, Label, Value, and Total (plus any cross-tabulation columns)
#'
#' @examples
#' # Example using test_list
#' result <- calculate_freqs(
#'   input_data = test_list$loaded_data,
#'   respid_var = "respid",
#'   var_labels = test_list$var_labels,
#'   val_labels = test_list$val_labels,
#'   col_var = "scvar_1",
#'   weight_var = "weight"
#' )
#'
#' # View frequency counts
#' print(result$output_coln)
#'
#' # View proportions
#' print(result$output_colprops)
#'
#' @export
calculate_freqs <- function(input_data,
                            respid_var,
                            var_labels,
                            val_labels,
                            freq_vars = NULL,
                            col_var = NULL,
                            weight_var = NULL,
                            tibble_out = TRUE) {

  # transform input_data into data.table for efficient processing
  if(isTRUE(methods::is(input_data, 'data.table'))) {
    input_data <- data.table::copy(input_data)
  } else {
    input_data <- data.table::as.data.table(input_data)
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

  # add dummy weight if not available
  if(isTRUE(is.null(weight_var))){
    input_data[, 'aux_internal_weight' := 1]
  } else {
    input_data[, 'aux_internal_weight' := get(weight_var)] |>
      _[, (weight_var) := NULL]
  }

  # keep only numeric/valid variables for mean score calculations
  if(isTRUE(is.null(freq_vars))){
    freq_vars <- setdiff(colnames(input_data),
                         c(respid_var, 'aux_internal_weight', col_var))
  }

  # ensure all value labels are interpreted as characters for consistent processing
  data.table::set(val_labels,
                  j = 'val_value',
                  value = as.character(val_labels[['val_value']]))


  # ensure all variables for which frequencies will be calculated are interpreted
  # as character variables for easy processing
  for(j in freq_vars){

    data.table::set(input_data,
                    j = j,
                    value = as.character(input_data[[j]]))

  }

  # calculate frequencies by variable at total (i.e. full data) level
  total_freqs <- input_data |>
    data.table::melt(id.vars = c(respid_var, 'aux_internal_weight', col_var),
                     variable.name = "row_variable",
                     value.name = "row_level",
                     variable.factor = FALSE) |>
    _[, list(n = sum(aux_internal_weight)),
      by = c('row_variable', 'row_level')] |>
    _[, 'prop' := n/sum(n),
      by = 'row_variable'] |>
    _[, 'col_variable' := 'total'] |>
    _[, 'val_value' := NA_character_]


  if(isFALSE(is.null(col_var))) {

    # calculate frequencies per variables by col_var
    col_freqs <- input_data |>
      data.table::melt(id.vars = c(respid_var, 'aux_internal_weight', col_var),
                       variable.name = "row_variable",
                       value.name = "row_level",
                       variable.factor = FALSE) |>
      _[, list(n = sum(aux_internal_weight)),
        by = c('row_variable', 'row_level', col_var)] |>
      _[, 'prop' := n/sum(n),
        by = c('row_variable', col_var)] |>
      data.table::melt(id.vars = c('row_variable', 'row_level', 'n', 'prop'),
                       measure.vars =  col_var,
                       variable.name = 'col_variable',
                       value.name = 'val_value',
                       variable.factor = FALSE)


    # stack total and col_var level tables of frequencies
    total_freqs <- total_freqs |>
      list(col_freqs) |>
      data.table::rbindlist(use.names = TRUE)


  }

  # add value labels
  output_freqs <- data.table::merge.data.table(total_freqs,
                                               val_labels,
                                               by.x = c('col_variable', 'val_value'),
                                               by.y = c('var_name', 'val_value'),
                                               all.x = TRUE)
  # removing all column attributes inherited from input data
  output_freqs <- output_freqs[, lapply(.SD, as.vector)]

  # add dummy value label "total" for total frequqncies
  output_freqs[is.na(val_label),
               'val_label' := data.table::fifelse(col_variable == 'total',
                                                  col_variable,
                                                  val_value)]
  # add "blank" dummy value for any "empty" value label
  output_freqs[val_label == '', 'val_label' := 'blank']

  # create proportion (props) and counts (n) tables
  output_colprops <- data.table::copy(output_freqs)
  output_colprops[, 'n' := NULL]

  output_coln <- data.table::copy(output_freqs)
  output_coln[, 'prop' := NULL]

  # pivot wider, add variable labels, and value labels to each table
  output_colprops <- output_colprops |>
    data.table::dcast(row_variable + row_level ~ val_label,
                      value.var = c('prop')) |>
    data.table::merge.data.table(var_labels,
                                 by.x = 'row_variable',
                                 by.y = 'var_name') |>
    data.table::merge.data.table(val_labels,
                                 by.x = c('row_variable', 'row_level'),
                                 by.y = c('var_name', 'val_value')) |>
    _[, 'row_level' := NULL]

  # label output columns cleanly
  data.table::setnames(output_colprops,
                       c('row_variable', 'var_label', 'val_label', 'total'),
                       c('Variable', 'Label', 'Value', 'Total'))

  # ensure consistent column order
  data.table::setcolorder(output_colprops,
                          neworder = c('Variable', 'Label', 'Value', 'Total'))

  output_coln <- output_coln |>
    data.table::dcast(row_variable + row_level ~ val_label,
                      value.var = c('n')) |>
    data.table::merge.data.table(var_labels,
                                 by.x = 'row_variable',
                                 by.y = 'var_name')|>
    data.table::merge.data.table(val_labels,
                                 by.x = c('row_variable', 'row_level'),
                                 by.y = c('var_name', 'val_value')) |>
    _[, 'row_level' := NULL]

  # label output columns cleanly
  data.table::setnames(output_coln,
                       c('row_variable', 'var_label', 'val_label', 'total'),
                       c('Variable', 'Label', 'Value', 'Total'))

  # ensure consistent column order
  data.table::setcolorder(output_coln,
                          neworder = c('Variable', 'Label', 'Value', 'Total'))

  # if tibble_out is FALSE, return data.table
  if(isFALSE(tibble_out)) return(list(output_coln = output_coln, output_colprops = output_colprops))

  list(output_coln = output_coln,
       output_colprops = output_colprops) |>
    lapply(data.table::setDF) |>
    lapply(tibble::as_tibble)

}
