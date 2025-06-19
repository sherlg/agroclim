
library(testthat)
library(zoo)
source("R/ndays_thre_1var.R")

#' Test for the 'ndays_thre_1var' function
#'

#' @export
test_that("Test for 'ndays_thre_1var' function using default parameters", {
  # Test data
  set.seed(123) 
  dates <- seq(as.Date("2018-10-01"), as.Date("2022-03-31"), by = "day")
  data <- round(runif(length(dates), min = -2, max = 10), 1)

  # Test
  result <- ndays_thre_1var(var = data, dates = dates) 
  expected_result <- c(301, 292, 226)
  expect_equal(result, expected_result, info = "Ndays calculation failed")
})

test_that("Test for 'ndays_thre_1var' function especifying period", {
  # Test data
  set.seed(123) 
  dates <- seq(as.Date("2018-10-01"), as.Date("2022-03-31"), by = "day")
  data <- round(runif(length(dates), min = -2, max = 10), 1)
  
  # Test
  result <- ndays_thre_1var(var = data, dates = dates, start_day = "01-01", end_day = "04-30") 
  expected_result <- c(109, 101, 95,  69)
  expect_equal(result, expected_result, info = "Ndays calculation failed")
})

test_that("Test for 'ndays_thre_1var' function especifying value ranges", {
  # Test data
  set.seed(123) 
  dates <- seq(as.Date("2018-10-01"), as.Date("2022-03-31"), by = "day")
  data <- round(runif(length(dates), min = -2, max = 10), 1)
  
  # Test
  result <- ndays_thre_1var(var = data, dates = dates, start_day = "01-01", end_day = "04-30", 
                                 min_threshold = 1, min_direction = "geq",
                                 max_threshold = 5, max_direction = "leq") 
  expected_result <- c(51, 44, 36, 29)
  expect_equal(result, expected_result, info = "Ndays calculation failed")
})


test_that("Test for 'ndays_thre_1var' function especifying min duration period", {
  # Test data
  set.seed(123) 
  dates <- seq(as.Date("2018-10-01"), as.Date("2022-03-31"), by = "day")
  data <- round(runif(length(dates), min = -2, max = 10), 1)
  
  # Test
  result <- ndays_thre_1var(var = data, dates = dates, start_day = "01-01", end_day = "04-30", 
                            min_threshold = 1, min_direction = "geq",
                            max_threshold = 5, max_direction = "leq",
                            min_duration = 3) 
  expected_result <- c(12, 6,  6,  0)
  expect_equal(result, expected_result, info = "Ndays calculation failed")
})