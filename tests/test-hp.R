library(testthat)
source("R/hp.R")

#' Test for the 'hp' function (Length of the Probable Frost Period)
#'
# Test for the calculation of the length of the all periods with probable frost days over years
test_that("Test for all probable frost periods calculation", {
  # Test data: temperature mn and dates
  set.seed(123) 
  dates <- seq(as.Date("2018-10-01"), as.Date("2022-03-31"), by = "day")
  mn <- round(runif(length(dates), min = -6, max = 2), 1)

  # Run the 'hp' function
  result_hs <- hp(mn, dates, thres_mn = 0, thres_abs_mn = 0, min_duration = 2, period = "longest", type = "number")
  
  # Expected result
  expected_result_hs <- list(6)
  
  # Check if the result matches the expected result for the length of the frost periods
  expect_equal(result_hs, expected_result_hs)
})