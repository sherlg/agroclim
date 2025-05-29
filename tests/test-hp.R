library(testthat)
source("R/hp.R")

#' Test for the 'hp' function (Length of the Probable Frost Period)
#'
# Test for the calculation of the length of probable frost days over years
test_that("Test for the number of probable frost days calculation", {
  # Test data: temperature mn and dates
  set.seed(123) 
  dates <- seq(as.Date("2018-10-01"), as.Date("2022-03-31"), by = "day")
  mn <- round(runif(length(dates), min = -2, max = 10), 1)
  
  # Run the 'hp' function
  result_hs <- hp(mn, dates, thres = 0, thres_abs = 0)
  
  # Expected result
  expected_result_hs <- c(107,100,93)
  
  # Check if the result matches the expected result
  expect_equal(result_hs, expected_result_hs)
})