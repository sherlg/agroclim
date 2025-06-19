
library(testthat)
library(zoo)
source("R/event_prob_day.R")

#' Test for the 'event_prob_day' function
#'

#' @export
test_that("Test 1 for 'event_prob_day' function", {
  # Test data
  set.seed(123) 
  dates <- seq(as.Date("2018-10-01"), as.Date("2022-03-31"), by = "day")
  data <- round(runif(length(dates), min = -2, max = 10), 1)
  
  # Conditions: "first" "threshold <= 0" "p=0.10"
  result <- event_prob_day(var = data, dates = dates, threshold = 0, direction = "leq", event = "first", prob = 0.10) 
  expected_result <- 183
  expect_equal(result, expected_result, info = "Day failed")
})

test_that("Test 2 for 'event_prob_day' function", {
  # Test data
  set.seed(123) 
  dates <- seq(as.Date("2018-10-01"), as.Date("2022-03-31"), by = "day")
  data <- round(runif(length(dates), min = -2, max = 10), 1)
  
  # Conditions: "first" "threshold <= 0" "p=0.90"
  result <- event_prob_day(var = data, dates = dates, threshold = 0, direction = "leq", event = "first", prob = 0.90) 
  expected_result <- 188
  expect_equal(result, expected_result, info = "Day failed")
})

test_that("Test 3 for 'event_prob_day' function", {
  # Test data
  set.seed(123) 
  dates <- seq(as.Date("2018-10-01"), as.Date("2022-03-31"), by = "day")
  data <- round(runif(length(dates), min = -2, max = 10), 1)
  
  # Conditions: "last" "threshold <= 0" "p=0.10"
  result <- event_prob_day(var = data, dates = dates, threshold = 0, direction = "leq", event = "last", prob = 0.10) 
  expected_result <- 180
  expect_equal(result, expected_result, info = "Day failed")
})

test_that("Test 4 for 'event_prob_day' function", {
  # Test data
  set.seed(123) 
  dates <- seq(as.Date("2018-10-01"), as.Date("2022-03-31"), by = "day")
  data <- round(runif(length(dates), min = -2, max = 10), 1)
  
  # Conditions: "last" "threshold <= 0" "p=0.90"
  result <- event_prob_day(var = data, dates = dates, threshold = 0, direction = "leq", event = "last", prob = 0.90) 
  expected_result <- 102
  expect_equal(result, expected_result, info = "Day failed")
})

test_that("Test 5 for 'event_prob_day' function", {
  # Test data
  set.seed(123) 
  dates <- seq(as.Date("2018-10-01"), as.Date("2022-03-31"), by = "day")
  data <- round(runif(length(dates), min = -2, max = 10), 1)
  
  # Conditions: "first" "threshold <= 0" "p=0.10" "season: 01-01:04-30"
  result <- event_prob_day(var = data, dates = dates, start_day = "01-01", end_day = "04-30", threshold = 0, direction = "leq", event = "last", prob = 0.10) 
  expected_result <- 120
  expect_equal(result, expected_result, info = "Day failed")
})
