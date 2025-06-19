library(testthat)
library(zoo)
source("R/event_prob_emp.R")

#' Test for the 'event_prob_emp' function
#'

#' @export
test_that("Test 1 for 'event_prob_emp' function", {
  # Test data
  set.seed(123) 
  dates <- seq(as.Date("2018-10-01"), as.Date("2022-03-31"), by = "day")
  data <- round(runif(length(dates), min = -2, max = 10), 1)
  
  # Conditions: specific day
  result <- event_prob_emp(var = data, dates = dates, threshold = 0, direction = "leq", time_scale = "day", target_date = "01-05") 
  expected_result <- 0.25
  expect_equal(round(result, 2), expected_result, info = "Prob calculation failed")
})

test_that("Test 2 for 'event_prob_emp' function", {
  # Test data
  set.seed(123) 
  dates <- seq(as.Date("2018-10-01"), as.Date("2022-03-31"), by = "day")
  data <- round(runif(length(dates), min = -2, max = 10), 1)
  
  # Conditions: specific month
  result <- event_prob_emp(var = data, dates = dates, threshold = 0, direction = "leq", time_scale = "month", target_date = "02") 
  expected_result <- 0.22
  expect_equal(round(result, 2), expected_result, info = "Prob calculation failed")
})

test_that("Test 3 for 'event_prob_emp' function", {
  # Test data
  set.seed(123) 
  dates <- seq(as.Date("2018-10-01"), as.Date("2022-03-31"), by = "day")
  data <- round(runif(length(dates), min = -2, max = 10), 1)
  
  # Conditions: specific season
  result <- event_prob_emp(var = data, dates = dates, start_day = "07-01", end_day = "06-30", threshold = 0, direction = "leq", time_scale = "season") 
  expected_result <- 0.20
  expect_equal(round(result, 2), expected_result, info = "Prob calculation failed")
})
