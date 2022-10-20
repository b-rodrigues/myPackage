unemp_2013 <- readr::read_csv("https://raw.githubusercontent.com/b-rodrigues/modern_R/master/datasets/unemployment/unemp_2013.csv", show_col_types = FALSE)

test_that("selecting the grand duchy works", {

  returned_value <- clean_unemp(
    unemp_2013,
    grepl("Grand-D.*", commune),
    active_population)

  expected_value <- tibble::as_tibble(
                              list("year" = 2013, 
                                   "commune" = "Grand-Duche de Luxembourg", 
                                   "active_population" = 242694))

  expect_equal(returned_value, expected_value)

})

test_that("selecting cantons work", {

  returned_value <- clean_unemp(
    unemp_2013,
    grepl("Canton", commune),
    active_population)

  expected_value <- readr::read_csv("test_data_cantons.csv", show_col_types = FALSE)

  expect_equal(returned_value, expected_value)

})

test_that("selecting communes works", {

  returned_value <- clean_unemp(
    unemp_2013,
    !grepl("(Canton|Grand-D.*)", commune),
    active_population)

  expected_value <- readr::read_csv("test_data_communes.csv", show_col_types = FALSE)

  expect_equal(returned_value, expected_value)

})

test_that("selecting one commune works", {

  returned_value <- clean_unemp(
    unemp_2013,
    grepl("Kayl", commune),
    active_population)

  expected_value <- tibble::as_tibble(
                              list("year" = 2013,
                                   "commune" = "Kayl",
                                   "active_population" = 3863))

  expect_equal(returned_value, expected_value)

})

test_that("wrong commune name", {

  returned_value <- clean_unemp(
    unemp_2013,
    grepl("Paris", commune),
    active_population)

  expected_value <- tibble::as_tibble(
                              list("year" = numeric(0),
                                   "commune" = character(0),
                                   "active_population" = numeric(0)))


  expect_equal(returned_value, expected_value)

})

test_that("wrong commune name: warning is thrown", {

  expect_warning({
    clean_unemp(
      unemp_2013,
      grepl("Paris", commune),
      active_population)
  }, "This is likely")

})
