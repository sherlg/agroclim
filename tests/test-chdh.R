library(testthat)
source("R/chdh.R")

#' Test for the 'chdh' function (Freeze-Thaw Cycles)
#'
# Test for the calculation of the number of days in each seasonal range where the minimum temperature is less than or equal to a 
#' threshold and the maximum temperature is greater than or equal to another threshold.
test_that("Test for the number of frost-free days calculation", {
  # Test data: temperatures mn, mx, and dates
  set.seed(123) 
  dates <- seq(as.Date("2018-10-01"), as.Date("2022-03-31"), by = "day")
  mn <- round(runif(length(dates), min = -6, max = 2), 1)
  mx <- round(runif(length(dates), min = -3, max = 6), 1)
  
  # Run the 'chdh' function
  result_hs <- chdh(mn, dates, thres_mn = -2, thres_mx = 2)
  
  # Expected result
  expected_result_hs <- c(91,81,60)
  
  # Check if the result matches the expected result
  expect_equal(result_hs, expected_result_hs)
})