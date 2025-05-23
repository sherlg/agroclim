library(testthat)
source("R/hn.R")

#' Test for the 'hn' function (Length of the Non-Frost Period)
#'
# Test for the calculation of the length of the all periods free of frost days over years
test_that("Test for all frost periods calculation", {
  # Test data: temperatures mn, and dates
  set.seed(123) 
  dates <- seq(as.Date("2018-10-01"), as.Date("2022-03-31"), by = "day")
  mn <- round(runif(length(dates), min = -2, max = 10), 1)

  # Run the 'hn' function
  result_hs <- hn(mn, dates, thres = 0, min_duration = 2, period = "all", type = "number")
  
  # Expected result
  expected_result_hs <-#TODO: Terminar
  
  # Check if the result matches the expected result for the length of the frost periods
  expect_equal(result_hs, expected_result_hs)
})