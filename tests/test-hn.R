library(testthat)
source("R/hn.R")

#' Test for the 'hn' function (Length of the Non-Frost Period)
#'
# Test for the calculation of the number of free-frost days over years
test_that("Test for the number of frost-free days calculation", {
  # Test data: temperatures mn, and dates
  set.seed(123) 
  dates <- seq(as.Date("2018-10-01"), as.Date("2022-03-31"), by = "day")
  mn <- round(runif(length(dates), min = -2, max = 10), 1)

  # Run the 'hn' function
  result_hs <- hn(mn, dates, thres = 0)
  
  # Expected result
  expected_result_hs <- c(297,289,226)
  
  # Check if the result matches the expected result
  expect_equal(result_hs, expected_result_hs)
})