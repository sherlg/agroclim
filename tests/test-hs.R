library(testthat)
source("R/hs.R")

#' Test for the 'hs' function (Length of the Frost Period)
#'
# Test for the calculation of the length of guaranteed frost days over years
test_that("Test for the number of guaranteed frost days calculation", {
  # Test data: temperatures mx and mn, and dates
  set.seed(123) 
  dates <- seq(as.Date("2018-10-01"), as.Date("2022-03-31"), by = "day")
  mn <- round(runif(length(dates), min = -6, max = 2), 1)
  mx <- round(runif(length(dates), min = -3, max = 6), 1)

  # Run the 'hs' function
  result_hs <- hs(mn, mx, dates, thres = 0)
  
  # Expected result
  expected_result_hs <- c(197,207,154)
  
  # Check if the result matches the expected result
  expect_equal(result_hs, expected_result_hs)
})