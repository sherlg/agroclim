
library(testthat)
library(zoo)
source("R/stats_var.R")

#' Test for the 'stats_var' function
#'

#' @export
test_that("Test for 'stats_var' function using default season dates", {
  # Test data
  set.seed(123) 
  dates <- seq(as.Date("2018-10-01"), as.Date("2022-03-31"), by = "day")
  data <- round(runif(length(dates), min = -2, max = 10), 1)

  # Default parameters start_day = "07-01", end_day = "06-30"
  # Test: mean
  result_mean <- stats_var(var = data, dates = dates, operator = "mean") 
  expected_result <- c(3.99, 3.88, 3.84)
  expect_equal(result_mean, expected_result, tolerance = 0.1, info = "Mean calculation failed")
  
  # Test: sum
  result_sum <- stats_var(var = data, dates = dates, operator = "sum")
  expected_result <- c(1452.4, 1416.6, 1051.0)
  expect_equal(result_sum, expected_result, tolerance = 0.1, info = "Sum calculation failed")
  
  # Test: min
  result_min <- stats_var(var = data, dates = dates, operator = "min")
  expected_result <- c(-2, -2, -2)
  expect_equal(result_min, expected_result, tolerance = 0.1, info = "Min calculation failed")
  
  # Test: max
  result_max <- stats_var(var = data, dates = dates, operator = "max")
  expected_result <- c(10.0, 9.9, 10.0)
  expect_equal(result_max, expected_result, tolerance = 0.1, info = "Max calculation failed")
})


test_that("Test for 'stats_var' function especifying period", {
  # Test data
  set.seed(123) 
  dates <- seq(as.Date("2018-10-01"), as.Date("2022-03-31"), by = "day")
  data <- round(runif(length(dates), min = -2, max = 10), 1)
  
  # Default parameters start_day = "01-01", end_day = "04-30"
  # Test: mean
  result_mean <- stats_var(var = data, dates = dates, operator = "mean", start_day = "01-01", end_day = "04-30") 
  expected_result <- c(4.01, 4.03, 4.21, 3.80)
  expect_equal(result_mean, expected_result, tolerance = 0.1, info = "Mean calculation failed")
  
  # Test: sum
  result_sum <- stats_var(var = data, dates = dates, operator = "sum", start_day = "01-01", end_day = "04-30")
  expected_result <- c(481.7, 488.1, 504.9, 342.2)
  expect_equal(result_sum, expected_result, tolerance = 0.1, info = "Sum calculation failed")
  
  # Test: min
  result_min <- stats_var(var = data, dates = dates, operator = "min", start_day = "01-01", end_day = "04-30")
  expected_result <- c(-1.9, -2.0, -2.0, -2.0)
  expect_equal(result_min, expected_result, tolerance = 0.1, info = "Min calculation failed")
  
  # Test: max
  result_max <- stats_var(var = data, dates = dates, operator = "max", start_day = "01-01", end_day = "04-30")
  expected_result <- c(9.8,  9.9,  9.9, 10.0)
  expect_equal(result_max, expected_result, tolerance = 0.1, info = "Max calculation failed")
})
