#' Calculate weighted means for survey variables
#'
#' @description
#' This function calculates weighted means for numeric variables in survey data. It supports
#' both total-level calculations and breakdowns by a column variable. The function handles
#' value labels and variable labels, and can work with or without explicit weights.
#'
#' @param input_data A data.table or data.frame containing the survey data
#' @param respid_var Character string specifying the respondent ID variable name
#' @param var_labels A data.table or data.frame containing variable labels with columns 'var_name' and 'var_label'
#' @param val_labels A data.table or data.frame containing value labels with columns 'var_name', 'val_value', and 'val_label'
#' @param mean_vars Optional character vector of variables to calculate means for. If NULL, uses all numeric variables
#' @param col_var Optional character string specifying a variable to break down means by
#' @param weight_var Optional character string specifying the weight variable name
#' @param tibble_out Logical indicating whether to return a tibble (TRUE) or data.table (FALSE)
#'
#' @return A tibble or data.table containing the calculated means with variable labels
#'
#' @examples
#' data(test_list)
#'
#' # Calculate means for specific variables with all defaults
#' calculate_means(
#'   input_data = test_list$loaded_data[, c(1, 4, 7:8)],
#'   respid_var = 'respid',
#'   var_labels = test_list$var_labels,
#'   val_labels = test_list$val_labels
#' )
#'
#' # Calculate means with a column breakdown
#' calculate_means(
#'   input_data = test_list$loaded_data[, c(1, 4, 7:8)],
#'   respid_var = 'respid',
#'   var_labels = test_list$var_labels,
#'   val_labels = test_list$val_labels,
#'   col_var = 'scvar_1'
#' )
#'
#' # Calculate means with custom weights
#' calculate_means(
#'   input_data = test_list$loaded_data[, c(1, 4, 7:8, 13)],
#'   respid_var = 'respid',
#'   var_labels = test_list$var_labels,
#'   val_labels = test_list$val_labels,
#'   weight_var = 'weight'
#' )
#'
#' @export
calculate_means <- function(input_data,
                            respid_var,
                            var_labels,
                            val_labels,
                            mean_vars = NULL,
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
  if(isTRUE(is.null(mean_vars))){
    mean_vars <- setdiff(colnames(input_data),
                         c(respid_var, 'aux_internal_weight', col_var))
  }

  # if any variables in mean_vars are not numeric, return error
  assertthat::assert_that(
    all(sapply(input_data[, ..mean_vars], is.numeric)),
    msg = paste0("Only numeric variables allowed for mean scores, check variables: ",
                 paste(mean_vars[!sapply(input_data[, ..mean_vars], is.numeric)],
                       collapse = ", "))
  )

  # ensure to keep only relevant variables for calculations
  vars_mask <- c(respid_var, 'aux_internal_weight', col_var, mean_vars)
  input_data <- input_data[, ..vars_mask]

  # ensure all value labels are interpreted as characters for consistent processing
  data.table::set(val_labels,
                  j = 'val_value',
                  value = as.character(val_labels[['val_value']]))

  # calculate means by variable at total (i.e. full data) level
  total_means <- input_data |>
    data.table::melt(id.vars = c(respid_var, 'aux_internal_weight', col_var),
                     variable.name = "row_variable",
                     value.name = "value",
                     variable.factor = FALSE,
                     measure.vars = mean_vars) |>
    _[, list(total = weighted.mean(value,
                                   w = aux_internal_weight,
                                   na.rm = TRUE)),
      by = c('row_variable')] |>
    data.table::melt(id.vars = c('row_variable'),
                     measure.vars =  'total',
                     variable.name = 'col_variable',
                     value.name = 'score',
                     variable.factor = FALSE) |>
    _[, 'val_value' := NA_character_]


  if(isFALSE(is.null(col_var))) {

    # calculate means per variables by col_var
    col_means <- input_data |>
      data.table::melt(id.vars = c(respid_var, 'aux_internal_weight', col_var),
                       variable.name = "row_variable",
                       value.name = "score",
                       variable.factor = FALSE,
                       measure.vars = mean_vars) |>
      _[, list(score = weighted.mean(score,
                                     w = aux_internal_weight,
                                     na.rm = TRUE)),
        by = c('row_variable', col_var)] |>
      data.table::melt(id.vars = c('row_variable', 'score'),
                       measure.vars =  col_var,
                       variable.name = 'col_variable',
                       value.name = 'val_value',
                       variable.factor = FALSE)

    # stack total and col_var level tables of means
    total_means <- total_means |>
      list(col_means) |>
      data.table::rbindlist(use.names = TRUE)

    # ensure val_value is character for consistent processing
    data.table::set(total_means,
                    j = 'val_value',
                    value = as.character(total_means[['val_value']]))


  }

  # add value labels
  output_means <- data.table::merge.data.table(total_means,
                                               val_labels,
                                               by.x = c('col_variable', 'val_value'),
                                               by.y = c('var_name', 'val_value'),
                                               all.x = TRUE)
  # removing all column attributes inherited from input data
  output_means <- output_means[, lapply(.SD, as.vector)]

  # add dummy value label "total" for total means
  output_means[is.na(val_label),
               'val_label' := data.table::fifelse(col_variable == 'total',
                                                  col_variable,
                                                  val_value)]

  # add "blank" dummy value for any "empty" value label
  output_means[val_label == '', 'val_label' := 'blank']

  # pivot wider so output includes one column per value in col_var (if available)
  # in addition to total means across all input variables
  output_means <- output_means |>
    data.table::dcast(row_variable ~ val_label,
                      value.var = 'score') |>
    data.table::merge.data.table(var_labels,
                                 by.x = 'row_variable',
                                 by.y = 'var_name')

  # label output columns cleanly
  data.table::setnames(output_means,
                       c('row_variable', 'var_label', 'total'),
                       c('Variable', 'Label', 'Total'))

  # ensure consistent column order
  data.table::setcolorder(output_means,
                          neworder = c('Variable', 'Label', 'Total'))

  # if tibble_out is FALSE, return data.table
  if(isFALSE(tibble_out)) return(output_means)

  data.table::setDF(output_means)
  tibble::as_tibble(output_means)

}
