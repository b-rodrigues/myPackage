test_that("selecting the grand duchy works", {

  returned_value <- clean_unemp(
    unemp,
    year_of_interest = 2013,
    level_of_interest = "Country",
    col_of_interest = active_population)

  expected_value <- tibble::as_tibble(
                              list("year" = 2013,
                                   "place_name" = "Luxembourg",
                                   "level" = "Country",
                                   "active_population" = 242694))

  expect_equal(returned_value, expected_value)

})

test_that("selecting cantons work", {

  returned_value <- clean_unemp(
    unemp,
    year_of_interest = 2013,
    level_of_interest = "Canton",
    col_of_interest = active_population)

  expected_value <- readr::read_csv("test_data_cantons.csv", show_col_types = FALSE)

  expect_equal(returned_value, expected_value)

})

test_that("selecting communes works", {

  returned_value <- clean_unemp(
    unemp,
    year_of_interest = 2013,
    level_of_interest = "Commune",
    col_of_interest = active_population)

  expected_value <- readr::read_csv("test_data_communes.csv", show_col_types = FALSE)

  expect_equal(returned_value, expected_value)

})

test_that("selecting one commune works", {

  returned_value <- clean_unemp(
    unemp,
    year_of_interest = 2013,
    place_name_of_interest = "Kayl",
    col_of_interest = active_population)

  expected_value <- tibble::as_tibble(
                              list("year" = 2013,
                                   "place_name" = "Kayl",
                                   "level" = "Commune",
                                   "active_population" = 3863))

  expect_equal(returned_value, expected_value)

})

test_that("wrong commune name", {

  returned_value <- clean_unemp(
    unemp,
    year_of_interest = 2013,
    place_name_of_interest = "Paris",
    col_of_interest = active_population)

  expected_value <- tibble::as_tibble(
                              list("year" = numeric(0),
                                   "place_name" = character(0),
                                   "level" = character(0),
                                   "active_population" = numeric(0)))


  expect_equal(returned_value, expected_value)

})

test_that("wrong commune name: warning is thrown", {

  expect_warning({
    clean_unemp(
      unemp,
      year_of_interest = 2013,
      place_name_of_interest = "Paris",
      col_of_interest = active_population)
  }, "This is likely")

})