library(testthat)
source("R/ph.R")

#' Test for the 'ph' function (Frost Probability for a Specific Day)
#'
# Test for the calculation of probability of frost occurrence on a specific calendar
test_that("Test for frost probability calculation", {
  # Test data: temperatures, dates, and specific day
  set.seed(123) 
  dates <- seq(as.Date("2018-10-01"), as.Date("2022-03-31"), by = "day")
  mn <- round(runif(length(dates), min = -6, max = 8), 1)
  frost_day <- "01-05"

  # Run the 'ph' function
  result_ph <- ph(mn, dates, thres = 0, day = frost_day)
  
  # Expected result
  expected_result_ph <- 0.25 
  
  # Check if the result matches the expected result for the frost probability
  expect_equal(result_ph, expected_result_ph, tolerance = 0.01)
})

# Test for the calculation of probability of frost occurrence on a specific calendar
test_that("Test for frost probability calculation with NAs", {
  # Test data: temperatures, dates, and specific day
  set.seed(123) 
  dates <- seq(as.Date("2018-10-01"), as.Date("2022-03-31"), by = "day")
  mn <- round(runif(length(dates), min = -6, max = 8), 1)
  frost_day <- "01-05"
  
  # Introduce some NAs into the temperature data
  mn[sample(1:length(mn), 100)] <- NA
  
  # Run the 'ph' function
  result_ph <- ph(mn, dates, thres = 0, day = frost_day)

  # Calculate the proportion of frost days
  expected_result_ph <- 0
  
  # Check if the result matches the expected result for the frost probability
  expect_equal(result_ph, expected_result_ph, tolerance = 0.01)
})
