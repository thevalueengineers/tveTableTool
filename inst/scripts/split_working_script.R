identify_var_type <- function(dat,
                               row_vars,
                               variable_labels,
                               value_labels) {

  #create data frame that detects whether multi or single code variable. Anything missing is either numeric or string.

  var_type <- value_labels %>%
    split(.$variable) |>
    purrr::map(
      ~dplyr::distinct(.x) %>%
        {
          if(all(.$value %in% c(0, 1)) & all(.$`value label` %in% c("Yes", "No"))) {
            "MC"
          } else {
            "SC"
          }
        }
    ) |>
    dplyr::bind_rows(.id = "variable") |>
    tidyr::pivot_longer(tidyselect::everything(), names_to = "variable", values_to = "type")

  #flag character vars in data frame - TRUE = character
  character_vars <- purrr::map_lgl(dat, ~ is.character(.x))

  #extract them from the data set so we can filter using it
  character_names <- names(dat)[character_vars]

  final_type <- variable_labels |>
    #drop respid
    dplyr::filter(variable != "respid") |>
    dplyr::left_join(var_type,dplyr::join_by("variable")) |>
    #flag character vars
    dplyr::mutate(type = ifelse(
      variable %in% character_names,"STRING",type)
    ) |>
    #anything else left is numeric
    dplyr::mutate(type = dplyr::case_when(
      is.na(type) ~ "NUM",
      TRUE ~ type
    ))

  #filter by variable type and row vars
  mc_flag <- final_type |> dplyr::filter(type == "MC") |> dplyr::filter(variable %in% row_vars) |> dplyr::pull(variable)
  single_flag <- final_type |> dplyr::filter(type == "SC") |> dplyr::filter(variable %in% row_vars) |> dplyr::pull(variable)
  numeric_flag <- final_type |> dplyr::filter(type == "NUM")  |> dplyr::filter(variable %in% row_vars) |> dplyr::pull(variable)

  return(list(
    mc_flag = mc_flag,
    single_flag = single_flag,
    numeric_flag = numeric_flag
  ))
}

flag_list <- identify_var_type(dat,row_vars,variable_labels,value_labels)

mean_calcs <- function(dat,row_vars,col_var,weight_var,variable_labels,flag_list) {

  # Numeric and multi-code calculations - weighted means
  number_out <- dat %>%
    # Add a specific column variable in case col_var is also selected as a row variable
    dplyr::mutate(column = .data[[col_var]]) %>%
    dplyr::mutate(column = haven::as_factor(column)) %>%
    dplyr::select(tidyselect::all_of(c(flag_list$numeric_flag, flag_list$mc_flag, weight_var, "column"))) %>%
    # Now reselect leaving out col_var unless it is in row_vars
    # Add explicit NA level
    dplyr::mutate(
      dplyr::across(
        -tidyselect::all_of(weight_var),
        ~ dplyr::if_else(is.na(.), "0", as.character(.))  # Use a formula function with ~
      )
    )|>
    tidyr::pivot_longer(-c(tidyselect::all_of(weight_var), column)) |>
    #convert value to numeric so we can calculate means
    dplyr::mutate(value = as.numeric(value))


  if (is.null(weight_var) || weight_var == "") {
    # Unweighted mean calculations
    group_number <- number_out %>%
      dplyr::group_by(name, column) %>%
      dplyr::summarise(value = mean(value, na.rm = TRUE), .groups = 'drop') %>%
      tidyr::pivot_wider(names_from = "column", values_from = "value")

    total_number <- number_out %>%
      dplyr::group_by(name) %>%
      dplyr::summarise(value = mean(value, na.rm = TRUE), .groups = 'drop') %>%
      dplyr::rename(Total = value)
  } else {
    # Weighted mean calculations
    group_number <- number_out %>%
      dplyr::group_by(name, column) %>%
      dplyr::summarise(value = weighted.mean(value, w = .data[[weight_var]], na.rm = TRUE), .groups = 'drop') %>%
      tidyr::pivot_wider(names_from = "column", values_from = "value")

    total_number <- number_out %>%
      dplyr::group_by(name) %>%
      dplyr::summarise(value = weighted.mean(value, w = .data[[weight_var]], na.rm = TRUE), .groups = 'drop') %>%
      dplyr::rename(Total = value)
  }

  # Join them up and add label to show mean
  combined_number <- total_number %>%
    dplyr::left_join(group_number, by = "name") %>%
    dplyr::left_join(variable_labels, by = c("name" = "variable")) %>%
    dplyr::mutate(value = "mean") %>%
    dplyr::select(name, label, value, tidyselect::everything())

  return(combined_number)

}

flag_list <- identify_var_type(dat, row_vars, variable_labels, value_labels)

# Call the custom mean_calcs function
mean_calcs_result <- mean_calcs(
dat,
row_vars,
 col_var,
weight_var = NULL,
variable_labels,
flag_list
)

# Print or view the result
print(mean_calcs_result)

##single code calculations

single_calcs <- function(dat, row_vars, col_var, weight_var, variable_labels, flag_list) {

  # Prepare the data
  prep <- dat %>%
    # Add a specific column variable in case col_var is also selected as a row variable
    dplyr::mutate(column = .data[[col_var]]) %>%
    dplyr::select(dplyr::all_of(c(flag_list$single_flag, weight_var, "column"))) %>%
    # If labelled, convert to ordered factor
    dplyr::mutate(
      dplyr::across(
        where(labelled::is.labelled) & -tidyselect::all_of(weight_var),
        ~haven::as_factor(.x, ordered = TRUE)
      )
    ) %>%
    # Add explicit NA level
    dplyr::mutate(
      dplyr::across(
        -tidyselect::all_of(weight_var),
        forcats::fct_na_value_to_level
      )
    ) %>%
    # Convert everything to character so that we can pivot longer
    dplyr::mutate(
      dplyr::across(
        -tidyselect::all_of(weight_var),
        as.character
      )
    ) %>%
    tidyr::pivot_longer(-c(tidyselect::all_of(weight_var), column)) %>%
    # Add total column
    dplyr::bind_rows(dplyr::mutate(., column = "Total"))

  # Check if weight_var is NULL or empty
  use_weights <- !is.null(weight_var) && weight_var != ""

  # Count with or without weights
  counted <- if (use_weights) {
    prep %>%
      dplyr::group_by(dplyr::across(-tidyselect::all_of(weight_var))) %>%
      dplyr::count(wt = .data[[weight_var]])
  } else {
    prep %>%
      dplyr::group_by(dplyr::across(-tidyselect::all_of(weight_var))) %>%
      dplyr::count()
  }

  # Final transformations
  single_out <- counted %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(
      names_from = column,
      values_from = n
    ) %>%
    dplyr::left_join(variable_labels, by = c("name" = "variable")) %>%
    dplyr::relocate(label, .after = name) %>%
    dplyr::relocate(Total, .after = value) %>%
    # Replace NA with 0
    dplyr::mutate(
      dplyr::across(-c(name, label, value), ~tidyr::replace_na(.x, 0))
    ) %>%
    dplyr::group_by(name) %>%
    dplyr::mutate(dplyr::across(-c(1:2), ~.x / sum(.x, na.rm = TRUE))) %>%
    dplyr::ungroup()

  return(single_out)
}

# Call the custom mean_calcs function
single_calcs_result <- single_calcs(
  dat,
  row_vars,
  col_var,
  weight_var = NULL,
  variable_labels,
  flag_list
)


generate_table_new <- function(dat,
                               row_vars,
                               col_var,
                               weight_var,
                               variable_labels,
                               value_labels) {

  #checks
  assertthat::assert_that(is.data.frame(dat))
  assertthat::assert_that(is.character(row_vars))
  assertthat::assert_that(is.character(col_var))
  # assertthat::assert_that(is.character(weight_var))
  assertthat::assert_that(is.data.frame(variable_labels))

  #flag variable types
  flag_list <- identify_var_type(dat, row_vars, variable_labels, value_labels)

  #numeric and multi code calculations
  mean_calcs_result <- mean_calcs(
    dat,
    row_vars,
    col_var,
    weight_var,
    variable_labels,
    flag_list
  )

  #single code calculations
  single_calcs_result <- single_calcs(
    dat,
    row_vars,
    col_var,
    weight_var,
    variable_labels,
    flag_list
  )

  #store a log of what order we want the data frame in based on the order of inputs
  rows_order <- data.frame(row_vars)
  order_df <- rows_order |>
    dplyr::mutate(order = rownames(rows_order)) |>
    dplyr::rename(name = row_vars) |>
    dplyr::mutate(order = as.numeric(order))

  #join tables together and reorder
  output <- dplyr::bind_rows(mean_calcs_result,single_calcs_result) |>
    dplyr::left_join(order_df,dplyr::join_by("name")) |>
    dplyr::arrange(order) |>
    dplyr::select(-order)

  #check number of unique rows in table matches number of row variables
  assertthat::assert_that(length(unique(output$name))==length(row_vars))

  return(output)

}

generate_table_new(
  dat,
  row_vars,
  col_var,
  weight_var = NULL,
  variable_labels,
  value_labels)

