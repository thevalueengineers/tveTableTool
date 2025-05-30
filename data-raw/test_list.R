## code to prepare `test_list` dataset goes here

set.seed(1)
data.table::data.table(
  respid = 1:10,
  metavar_1 = sample(1:100, 10) |>
    haven::labelled(label = 'Meta Variable 1'),
  numvar_1 = rnorm(10, mean = 50, sd = 10) |>
    haven::labelled(label = 'Numeric Variable 1'),
  numvar_2 = sample.int(100, 10, replace = TRUE) |>
    haven::labelled(label = 'Numeric Variable 2'),
  charvar_1 = sample(letters, 10) |>
    haven::labelled(label = 'Character Variable 1'),
  charvar_2 = sample(LETTERS, 10) |>
    haven::labelled(label = 'Character Variable 2'),
  scvar_1 = sample(1:5, 10, replace = TRUE) |>
    haven::labelled(label = 'Single Code Variable 1',
                    labels = c('Value label A' = 1,
                               'Value label B' = 2,
                               'Value label C' = 3,
                               'Value label D' = 4,
                               'Value label E' = 5)),
  scvar_2 = sample(1:3, 10, replace = TRUE) |>
    haven::labelled(label = 'Single Code Variable 2',
                    labels = c('Value label X' = 1,
                               'Value label Y' = 2,
                               'Value label Z' = 3)),
  mcvar_1 = sample(0:1, 10, replace = TRUE) |>
    haven::labelled(label = 'Multi Code Variable 1',
                    labels = c('Yes' = 1, 'No' = 0)),
  mcvar_2 = sample(0:1, 10, replace = TRUE) |>
    haven::labelled(label = 'Multi Code Variable 2',
                    labels = c('Yes' = 1, 'No' = 0)),
  mcvar_3 = sample(0:1, 10, replace = TRUE) |>
    haven::labelled(label = 'Multi Code Variable 3',
                    labels = c('Present' = 1, 'Not Present' = 0)),
  mcvar_4 = sample(1:2, 10, replace = TRUE) |>
    haven::labelled(label = 'Multi Code Variable 4',
                    labels = c('Yes' = 2, 'No' = 1)),
  weight = runif(10, 0.5, 2)
) |>
  haven::write_sav(path = here::here('inst', 'extdata', 'test_sav.sav'))

test_list <- tveDataLoader::load_sav(here::here('inst', 'extdata', 'test_sav.sav'))

usethis::use_data(test_list, overwrite = TRUE)
