#' Test data for generating tables.
#'
#' Has 3 example weight variables:
#'
#' * `no_weight`: all records have a value of 1 so no weighting is applied
#' * `gender_weight1`: Male weighted to 0.5, female weighted to 1.5, other
#' weighted to 1
#' * `gender_weight2`: Male weighted to 1.5, female weighted to 0.5, other
#' weighted to 1
"testTableData"

#' Test data for variable classification.
#'
#' Output from tveDataLoader::load_sav containing a list of tibbles with the following components:
#'
#' * `loaded_data`: A tibble containing the raw data with zapped variables and tidy variable names
#'
#' * `var_labels`: A tibble containing variable labels with columns:
#'   - `var_name`: Name of the variable
#'   - `var_label`: Label for the variable
#'
#' * `val_labels`: A tibble containing value labels with columns:
#'   - `var_name`: Name of the variable
#'   - `val_label`: Label for the value
#'   - `val_value`: Numeric value corresponding to the label
#'
#' * `no_var_labels`: A tibble containing variables with no variable labels:
#'   - `var_name`: Name of the variable
#'
#' * `no_val_labels`: A tibble containing variables with no value labels:
#'   - `var_name`: Name of the variable
"test_list"
