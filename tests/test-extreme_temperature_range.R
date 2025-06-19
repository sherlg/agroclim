
library(testthat)
library(zoo)
source("R/extreme_temperature_range.R")

#' Test for the 'extreme_temperature_range' function
#'

#' @export
test_that("Test for 'extreme_temperature_range' function using default season dates", {
  # Test data
  set.seed(123) 
  dates <- seq(as.Date("2018-10-01"), as.Date("2022-03-31"), by = "day")
  data1 <- round(runif(length(dates), min = -2, max = 10), 1)
  data2 <- round(runif(length(dates), min = 4, max = 37), 1)
  
  # Default parameters start_day = "01-01", end_day = "12-31"
  # Test: day
  data1_subset<-data1[1:10]
  data2_subset<-data2[1:10]
  dates_subset<-dates[1:10]
  result <- extreme_temperature_range(data2_subset, data1_subset, dates = dates_subset) 
  expected_result <- c(12.8, 9.4, 7.8, 23.5, 14.7, 30.5, 18.3, 4.4, 23.7, 18.6)
  expect_equal(result, expected_result, tolerance = 0.1, info = "Daily ETR failed")
  
  # Test: month
  result <- extreme_temperature_range(data2, data1, dates = dates, time_scale = "month")
  expected_result <- c(33.6, 37.9, 37.5, 38.1, 38.4, 33.4, 37.2, 38.3, 38.1, 38.6, 35.2, 37.2, 36.9, 38.6, 38.1, 38.7, 37.3, 38.5, 37.0, 38.4, 38.4, 37.9, 38.5, 35.5, 38.9, 37.2, 38.1, 37.8, 37.7, 38.9, 36.4, 38.5, 38.3, 36.1, 37.0, 35.2, 38.1, 33.8, 38.6, 36.2, 35.7, 37.8)
  expect_equal(round(result, 2), expected_result, tolerance = 0.1, info = "Monthly ETR failed")
  
  # Test: season
  result <- extreme_temperature_range(data2, data1, dates = dates, time_scale = "season")
  expected_result <- c(38.9, 38.9, 39.0, 37.8)
  expect_equal(round(result, 2), expected_result, tolerance = 0.1, info = "Seasonal ETR failed")
})


test_that("Test for 'extreme_temperature_range' function especifying period", {
  # Test data
  set.seed(123) 
  dates <- seq(as.Date("2018-10-01"), as.Date("2022-03-31"), by = "day")
  data <- round(runif(length(dates), min = -2, max = 10), 1)
  
  result <- extreme_temperature_range(data2, data1, dates = dates, start_day = "07-01", end_day = "06-30", time_scale = "season")
  expected_result <- c(38.9, 39.0, 38.8)
  expect_equal(round(result, 2), expected_result, tolerance = 0.1, info = "Seasonal ETR failed")
})
