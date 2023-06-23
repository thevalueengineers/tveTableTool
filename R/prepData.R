#' Prepare data loaded by \link[datamods]{import-file}
#'
#' Shiny module to process .sav files loaded via \link[datamods]{import-file}.
#' Removes everything but numeric variables (i.e. open ends) and adds
#' no_weighting variable.
#'
#' Intended for use with \link[datamods]{import-file}, but might work with other
#' loading methods / source file types.
#'
#' @param id Module ID
#' @param data Dataframe loaded by \link[datamods]{import-file}
#'
#' @return Dataframe with only numeric variables and no_weighting variable.
#'
#' @export
prepData <- function(id, data) {

  moduleServer(
    id,
    function(input, output, session) {
      reactive({
        req(data())
        select(data(), where(is.numeric)) %>%
          # add no_weighting variable
          mutate(no_weighting = 1)
      })
    }
  )
}
