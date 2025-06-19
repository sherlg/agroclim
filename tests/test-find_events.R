library(testthat)
library(zoo)
source("R/find_events.R")

#' Test for the 'event_prob_teo' function
#'

#' @export
test_that("Test 1 for 'find_events' function", {
  # Test data
  set.seed(123) 
  dates <- seq(as.Date("2018-10-01"), as.Date("2022-03-31"), by = "day")
  data <- round(runif(length(dates), min = -2, max = 10), 1)
  
  dates_subset<-dates[50:240]
  data_subset<-data[50:240]
  
  # Conditions: data subset, default parameters
  result <- find_events(var = data_subset, dates = dates_subset) 
  expected_result <- 22
  expect_equal(nrow(result), expected_result, info = "Find events failed")
})

test_that("Test 2 for 'find_events' function", {
  # Test data
  set.seed(123) 
  dates <- seq(as.Date("2018-10-01"), as.Date("2022-03-31"), by = "day")
  data <- round(runif(length(dates), min = -2, max = 10), 1)
  
  # Conditions: season 01-01:04-30, gap_length = 3
  result <- find_events(var = data, dates = dates, start_day = "01-01", end_day = "04-30", gap_length=3) 
  expected_result <- 6
  expect_equal(nrow(result), expected_result, info = "Find events failed")
})

test_that("Test 3 for 'find_events' function", {
  # Test data
  set.seed(123) 
  dates <- seq(as.Date("2018-10-01"), as.Date("2022-03-31"), by = "day")
  data <- round(runif(length(dates), min = -2, max = 10), 1)
  
  dates_subset<-dates[50:240]
  data_subset<-data[50:240]
  
  # Conditions: data subset, var <= 2
  result <- find_events(var = data_subset, dates = dates_subset, direction="leq", threshold=2) 
  expected_result <- 41
  expect_equal(nrow(result), expected_result, info = "Find events failed")
})
