## code to prepare `test_tve_data` dataset goes here
set.seed(1)
test_sav <- tibble::tibble(
  respid = labelled::labelled(1:100,
                              label = 'respid'),
  lab_var1 = labelled::labelled(
    sample(1:4, 100, replace = TRUE),
    labels = c(
      "A" = 1,
      "B" = 2,
      "C" = 3,
      "D" = 4
    ),
    label = "v1 label"
  ),
  lab_var2 = labelled::labelled(
    sample(1:4, 100, replace = TRUE),
    labels = c(
      "a" = 1,
      "b" = 2
    ),
    label = "v2 label"
  ),
  lab_var3 = labelled::labelled(
    sample(0:1, 100, replace = TRUE),
    labels = c(
      "yes" = 1,
      "no" = 0
    ),
    label = "v3 label"
  ),
  num_var1 = labelled::labelled(
    rnorm(100),
    label = 'numeric var1'
  ),
  char_var1 = sample(letters, 100, replace = TRUE)
)

# Save the dataset as a .sav file
haven::write_sav(test_sav,
                 path = here::here('inst', 'extdata', 'test_sav.sav'))

test_tve_data <- tveDataLoader::load_sav(here::here('inst', 'extdata', 'test_sav.sav'),
                                         tibble_out = FALSE)

usethis::use_data(test_tve_data, overwrite = TRUE)
