#' Add index columns to a table
#'
#' Index is calculated by dividing each column by the Total column and
#' multiplying by 100.
#'
#' @param tab Table generated from \code{\link{generate_table}} .
#'
#' @return Tibble as per \code{\link{generate_table}}  with
#' additional columns for indexes
#'
#' @export
add_index <- function(tab) {
  tab %>%
    dplyr::mutate(
      dplyr::across(
        -c(Variable:Total),
        list("index" = ~ .x * 100 / Total)
      )
    )
}
