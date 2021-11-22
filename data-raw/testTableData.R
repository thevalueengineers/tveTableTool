testTableData <- tveDataLoader::read_sav("./inst/extdata/test data.sav") %>%
  dplyr::mutate(no_weight = 1)

usethis::use_data(testTableData, overwrite = TRUE)
