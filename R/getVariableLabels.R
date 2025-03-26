#' Shiny module to extract variable labels from data file
#'
#' Takes a data file loaded in a shiny app and reads the variable labels. If a
#' variable has no labels, then it assigns the variable name as label. Any
#' variables supplied to `exclude` are excluded.
#'
#' @param id Module ID
#' @param dat Data frame from which to read variable labels
#' @param exclude Character string of variables to exclude
#'
#' @export
getVariableLabels <- function(id, dat, exclude = "no_weighting") {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      shiny::reactive({
        shiny::req(dat())
        vars_with_labels <- tveDataLoader::get_varLabels(dat())
        vars_without_labels <- names(dat())[!names(dat()) %in% vars_with_labels$variable]

        # add vars without labels to variable_labels
        bind_rows(
          vars_with_labels,
          tibble(
            variable = vars_without_labels,
            label = vars_without_labels
          )
        ) %>%
          # make sure no_weighting var isn't included
          dplyr::filter(!variable %in% exclude)
      })
    }
  )
}
