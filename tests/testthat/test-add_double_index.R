test_that("Double index columns are added", {

  list <- create_tabulations(
    loaded_data = test_list$loaded_data,
    var_labels = test_list$var_labels,
    val_labels = test_list$val_labels,
    col_var = "scvar_1"
  )

  out <- list$colprops_table |> add_double_index()

  testthat::expect_equal(ncol(out), 12)
  testthat::expect_equal(nrow(out), 11)

  expect_equal(
    names(out),
    c(
      "Variable", "Label", "Value", "Total",
      "Value label A", "Value label C", "Value label D", "Value label E",
      "Value label A_di", "Value label C_di", "Value label D_di", "Value label E_di"
    )
  )

}
)
