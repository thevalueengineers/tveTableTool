library(tveDataLoader)
library(tidyr)
library(purrr)
library(dplyr)
library(stringr)
library(haven)

#test ideas
#number of unique variable rows in final data is equal to number of row_vars
dat <- testTableData

setup_summary_vars <- function() {

  c(
    paste0("s1_", sprintf("%02d", c(01:08, 99))),
    "s2",
    "s3",
    "s4",
    paste0("c2_", sprintf("%02d", c(01:05))),
    "s5"
  )

}

row_vars <- setup_summary_vars()
col_var <- "s5"
weight_var <- "no_weight"
value_labels <- dat |> get_valLabels()
variable_labels <- dat |> get_varLabels()

generate_table_new <- function(dat,
                           row_vars,
                           col_var,
                           weight_var,
                           variable_labels,
                           value_labels) {

  assertthat::assert_that(is.data.frame(dat))
  assertthat::assert_that(is.character(row_vars))
  assertthat::assert_that(is.character(col_var))
  assertthat::assert_that(is.character(weight_var))
  assertthat::assert_that(is.data.frame(variable_labels))
  # rlang::arg_match(percents, c("columns", "rows", "none"))

#store a log of what order we want the data frame in
rows_order <- data.frame(row_vars)
order_df <- rows_order |>
  mutate(order = rownames(order_df)) |>
  rename(name = row_vars) |>
  mutate(order = as.numeric(order))

#create data frame that detects whether multi or single code variable

var_type <- value_labels %>%
  split(.$variable) |>
  map(
    ~distinct(.x) %>%
      {
        if(all(.$value %in% c(0, 1)) & all(.$`value label` %in% c("Yes", "No"))) {
          "MC"
        } else {
          "SC"
        }
      }
  ) |>
  bind_rows(.id = "variable") |>
  pivot_longer(everything(), names_to = "variable", values_to = "type")

#flag character vars in data frame
character_vars <- purrr::map_lgl(dat, ~ is.character(.x))

#extract them from the data set so we can filter using it
character_names <- names(dat)[character_vars]

final_type <- variable_labels |>
  #drop respid
  filter(variable != "respid") |>
  left_join(var_type,join_by("variable")) |>
  #flag character vars
  mutate(type = ifelse(
    variable %in% character_names,"STRING",type)
  ) |>
  #anything else left is numeric
  mutate(type = case_when(
    is.na(type) ~ "NUM",
    TRUE ~ type
  ))

# final_type_split <- final_type %>% split(.$variable)

#filter by variable type and row vars
mc_flag <- final_type |> filter(type == "MC") |> filter(variable %in% row_vars) |> pull(variable)
single_flag <- final_type |> filter(type == "SC") |> filter(variable %in% row_vars) |> pull(variable)
numeric_flag <- final_type |> filter(type == "NUM")  |> filter(variable %in% row_vars) |> pull(variable)

#numeric and multi code
number_out <- dat |>
  # add a specific column variable in case col_var is also selected as a row
  # variable
  dplyr::mutate(column = .data[[col_var]]) %>%
  mutate(column = as_factor(column)) |>
  dplyr::select(dplyr::all_of(c(numeric_flag,mc_flag, weight_var,"column"))) %>%
  # now reselect leaving out col_var unless it is in row_vars
  # add explicit NA level
  dplyr::mutate(
    dplyr::across(
      -tidyselect::all_of(weight_var),
      ~ ifelse(is.na(.), "0", as.character(.))
    )
  )|>
  tidyr::pivot_longer(-c(tidyselect::all_of(weight_var), column)) |>
  #convert value to numeric so we can calculate means
  mutate(value = as.numeric(value))

#weighted mean table using two group vars
group_number <- number_out |>
  group_by(name,column) |>
  summarise(value = weighted.mean(value,wt = weight)) |>
  ungroup() |>
  pivot_wider(names_from = "column",values_from = "value")

#table using one group vars (just analysis variable)
total_number <- number_out |>
  group_by(name) |>
  summarise(value = weighted.mean(value,wt = weight)) |>
  ungroup() |>
  rename(Total = value)

#join them up and add label to show mean
combined_number <- total_number |>
  dplyr::left_join(group_number,join_by("name")) |>
  left_join(variable_labels,join_by("name"=="variable")) |>
  mutate(value = "mean") |>
  select(name,label,value,everything())

##single
single_out <- dat |>
  # add a specific column variable in case col_var is also selected as a row
  # variable
  dplyr::mutate(column = .data[[col_var]]) %>%
  dplyr::select(dplyr::all_of(c(single_flag, weight_var,"column"))) %>%
  # if labelled convert to ordered factor
  dplyr::mutate(
    dplyr::across(
      where(labelled::is.labelled) & -tidyselect::all_of(weight_var),
      ~haven::as_factor(.x, ordered = TRUE)
    )
  ) %>%
  # add explicit NA level
  dplyr::mutate(
    dplyr::across(
      -tidyselect::all_of(weight_var),
      forcats::fct_na_value_to_level
    )
  ) %>%
  # convert everything to character so that we can pivot longer
  dplyr::mutate(
    dplyr::across(
      -tidyselect::all_of(weight_var),
      as.character
    )
  ) %>%
  tidyr::pivot_longer(-c(tidyselect::all_of(weight_var), column)) %>%
  # add total column
  dplyr::bind_rows(dplyr::mutate(., column = "Total")) %>%
  dplyr::group_by(dplyr::across(-tidyselect::all_of(weight_var))) %>%
  dplyr::count(wt = .data[[weight_var]]) %>%
  dplyr::ungroup() %>%
  tidyr::pivot_wider(
    names_from = column,
    values_from = n
  ) %>%
  dplyr::left_join(variable_labels, by = c("name" = "variable")) %>%
  dplyr::relocate(label, .after = name) %>%
  dplyr::relocate(Total, .after = value) %>%
  # replace NA with 0
  dplyr::mutate(
    dplyr::across(-c(name, label, value), ~tidyr::replace_na(.x, 0))
  ) |>
  dplyr::group_by(name) %>%
  dplyr::mutate(dplyr::across(-c(1:2), ~.x / sum(.x, na.rm = TRUE))) %>%
  dplyr::ungroup()

output <- bind_rows(combined_number,single_out) |>
  left_join(order_df,join_by("name")) |>
  arrange(order) |>
  select(-order)

#check number of unique rows in table matches  number of row variables
assert_that(length(unique(output$name))==length(row_vars))


return(output)

}
