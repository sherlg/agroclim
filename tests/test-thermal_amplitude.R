
library(testthat)
library(zoo)
source("R/thermal_amplitude.R")

#' Test for the 'thermal_amplitude' function
#'

#' @export
test_that("Test for 'thermal_amplitude' function using default season dates", {
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
  result <- thermal_amplitude(data2_subset, data1_subset, dates = dates_subset) 
  expected_result <- c(12.8, 9.4, 7.8, 23.5, 14.7, 30.5, 18.3, 4.4, 23.7, 18.6)
  expect_equal(result, expected_result, tolerance = 0.1, info = "Daily thermal amplitude failed")
  
  # Test: month
  result <- thermal_amplitude(data2, data1, dates = dates, time_scale = "month")
  expected_result <- c(16.17, 16.85, 12.56, 15.36, 18.18, 17.51, 19.59, 17.16, 15.51, 14.40, 16.55, 17.06, 16.63, 17.87, 17.67, 19.90, 16.54, 18.05, 16.84, 16.64, 16.06, 14.79, 15.97, 12.57, 18.03, 17.32, 16.49, 17.27, 15.18, 17.06, 14.52, 19.14, 15.59, 16.82, 17.04, 15.23, 17.73, 15.36, 15.56, 14.77, 17.70, 20.12)
  expect_equal(round(result, 2), expected_result, tolerance = 0.1, info = "Monthly thermal amplitude failed")
  
  # Test: season
  result <- thermal_amplitude(data2, data1, dates = dates, time_scale = "season")
  expected_result <- c(16.94, 16.61, 16.40, 17.52)
  expect_equal(round(result, 2), expected_result, tolerance = 0.1, info = "Seasonal thermal amplitude failed")
})


test_that("Test for 'thermal_amplitude' function especifying period", {
  # Test data
  set.seed(123) 
  dates <- seq(as.Date("2018-10-01"), as.Date("2022-03-31"), by = "day")
  data <- round(runif(length(dates), min = -2, max = 10), 1)
  
  result <- thermal_amplitude(data2, data1, dates = dates, start_day = "07-01", end_day = "06-30", time_scale = "season")
  expected_result <- c(17.02, 16.18, 16.70)
  expect_equal(round(result, 2), expected_result, tolerance = 0.1, info = "Seasonal thermal amplitude failed")
})
