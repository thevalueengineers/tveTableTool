data("test_tve_data")
loaded_data <- test_tve_data$loaded_data |> data.table::copy()
var_labels <- test_tve_data$var_labels |> data.table::copy()
no_var_labels <- test_tve_data$no_var_labels |> data.table::copy()
val_labels <- test_tve_data$val_labels |> data.table::copy()
no_val_labels <- test_tve_data$no_val_labels |> data.table::copy()
row_vars <- c('char_var1', 'lab_var2', 'lab_var3')
respid_var <- 'respid'


identify_var_type <- function(loaded_data,
                              val_labels,
                              no_val_labels = NULL,
                              meta_vars) {

  var_type <- val_labels |>
    _[, .N, by = c('var_name',
                   'val_label',
                   'val_value')] |>
    _[, list(valid_labels = identical(sort(val_label), c('no', 'yes')),
             valid_values = identical(sort(val_value), c(0, 1))),
      by = 'var_name'] |>
    _[, 'type' := data.table::fifelse(valid_labels & valid_values, 'multi','single')] |>
    _[, c('valid_labels', 'valid_values') := NULL]

  if(isTRUE(!is.null(no_val_labels) && nrow(no_val_labels) > 0)) {

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

  return(var_type)

}

calculate_means <- function(input_data,
                            col_var,
                            respid_var,
                            weight_var,
                            var_labels,
                            val_labels) {

  temp_data <- data.table::copy(input_data)

  total_means <- temp_data |>
    data.table::melt(id.vars = c(respid_var, weight_var, col_var),
                     variable.name = "row_variable",
                     value.name = "value",
                     variable.factor = FALSE) |>
    _[, list(total = weighted.mean(value,
                                   wt = get(weight_var),
                                   na.rm = TRUE)),
      by = c('row_variable')] |>
    data.table::melt(id.vars = c('row_variable'),
                     measure.vars =  'total',
                     variable.name = 'col_variable',
                     value.name = 'score',
                     variable.factor = FALSE) |>
    _[, 'val_value' := NA]

  if(isFALSE(is.null(col_var))) {

    col_means <- temp_data |>
      data.table::melt(id.vars = c(respid_var, weight_var, col_var),
                       variable.name = "row_variable",
                       value.name = "score",
                       variable.factor = FALSE) |>
      _[, list(score = weighted.mean(score,
                                     wt = get(weight_var),
                                     na.rm = TRUE)),
        by = c('row_variable', col_var)] |>
      data.table::melt(id.vars = c('row_variable', 'score'),
                       measure.vars =  col_var,
                       variable.name = 'col_variable',
                       value.name = 'val_value',
                       variable.factor = FALSE)

    total_means <- total_means |>
      list(col_means) |>
      data.table::rbindlist(use.names = TRUE)


  }

  output_means <- data.table::merge.data.table(total_means,
                                               val_labels,
                                               by.x = c('col_variable', 'val_value'),
                                               by.y = c('var_name', 'val_value'),
                                               all.x = TRUE)

  output_means[is.na(val_label), 'val_label' := col_variable]
  output_means <- output_means |>
    data.table::dcast(row_variable ~ val_label,
                      value.var = 'score') |>
    data.table::merge.data.table(var_labels,
                                 by.x = 'row_variable',
                                 by.y = 'var_name')

  data.table::setnames(output_means,
                       c('row_variable', 'var_label', 'total'),
                       c('Variable', 'Label', 'Total'))

  data.table::setcolorder(output_means,
                          neworder = c('Variable', 'Label', 'Total'))

  return(output_means)

}

calculate_freqs <- function(input_data,
                            col_var,
                            respid_var,
                            weight_var,
                            var_labels,
                            val_labels) {

  temp_data <- data.table::copy(input_data)
  temp_val_labels <- data.table::copy(val_labels)
  data.table::set(temp_val_labels,
                  j = 'val_value',
                  value = as.character(temp_val_labels[['val_value']]))
  var_mask <- setdiff(colnames(temp_data),
                      c(respid_var, weight_var))

  for(j in var_mask){

    data.table::set(temp_data,
                    j = j,
                    value = as.character(temp_data[[j]]))

  }

  total_freqs <- temp_data |>
    data.table::melt(id.vars = c(respid_var, weight_var, col_var),
                     variable.name = "row_variable",
                     value.name = "row_level",
                     variable.factor = FALSE) |>
    _[, list(n = sum(get(weight_var))),
      by = c('row_variable', 'row_level')] |>
    _[, 'prop' := n/sum(n),
      by = 'row_variable'] |>
    _[, 'col_variable' := 'total'] |>
    _[, 'val_value' := NA]


  if(isFALSE(is.null(col_var))) {

    col_freqs <- temp_data |>
      data.table::melt(id.vars = c(respid_var, weight_var, col_var),
                       variable.name = "row_variable",
                       value.name = "row_level",
                       variable.factor = FALSE) |>
      _[, list(n = sum(get(weight_var))),
        by = c('row_variable', 'row_level', col_var)] |>
      _[, 'prop' := n/sum(n),
        by = c('row_variable', col_var)] |>
      data.table::melt(id.vars = c('row_variable', 'row_level', 'n', 'prop'),
                       measure.vars =  col_var,
                       variable.name = 'col_variable',
                       value.name = 'val_value',
                       variable.factor = FALSE)

    total_freqs <- total_freqs |>
      list(col_freqs) |>
      data.table::rbindlist(use.names = TRUE)


  }

  output_freqs <- data.table::merge.data.table(total_freqs,
                                               temp_val_labels,
                                               by.x = c('col_variable', 'val_value'),
                                               by.y = c('var_name', 'val_value'),
                                               all.x = TRUE)

  output_freqs[is.na(val_label), 'val_label' := col_variable]

  output_colprops <- data.table::copy(output_freqs)
  output_colprops[, 'n' := NULL]

  output_coln <- data.table::copy(output_freqs)
  output_coln[, 'prop' := NULL]

  output_colprops <- output_colprops |>
    data.table::dcast(row_variable + row_level ~ val_label,
                      value.var = c('prop')) |>
    data.table::merge.data.table(var_labels,
                                 by.x = 'row_variable',
                                 by.y = 'var_name')

  data.table::setnames(output_colprops,
                       c('row_variable', 'var_label', 'row_level', 'total'),
                       c('Variable', 'Label', 'Value', 'Total'))

  data.table::setcolorder(output_colprops,
                          neworder = c('Variable', 'Label', 'Value', 'Total'))

  output_coln <- output_coln |>
    data.table::dcast(row_variable + row_level ~ val_label,
                      value.var = c('n')) |>
    data.table::merge.data.table(var_labels,
                                 by.x = 'row_variable',
                                 by.y = 'var_name')

  data.table::setnames(output_coln,
                       c('row_variable', 'var_label', 'row_level', 'total'),
                       c('Variable', 'Label', 'Value', 'Total'))

  data.table::setcolorder(output_coln,
                          neworder = c('Variable', 'Label', 'Value', 'Total'))

  return(list(output_coln = output_coln,
              output_colprops = output_colprops))

}


create_tabulations <- function(loaded_data,
                               var_labels,
                               val_labels,
                               row_vars,
                               no_val_labels = NULL,
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
  var_types <- identify_var_type(loaded_data = loaded_data,
                                 val_labels = val_labels,
                                 no_val_labels = no_val_labels,
                                 meta_vars = meta_vars)

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



