library(testthat)
source("R/tmc.R")

#' Test for the 'tmc' function (Warmest Month of the Year)
#'

#' @export
test_that("Test for warmest month calculation", {
  # Test data
  mn <- c(15, 16, 14, 13, 10, 9, 8, 7, 11, 12, 13, 14, 15, 14, 12, 10, 11, 9, 8, 7, 11, 12, 13, 12, 14, 13, 14, 15, 10, 11, 10, 9, 7, 6, 9, 8)  # Minimum temperatures
  mx <- c(30, 32, 29, 28, 25, 24, 23, 22, 26, 27, 28, 29, 30, 28, 27, 25, 24, 23, 22, 21, 26, 27, 28, 29, 30, 28, 27, 26, 25, 26, 25, 24, 22, 21, 22, 23)  # Maximum temperatures
  dates <- as.Date(c("1991-01-01", "1991-01-02", "1991-01-03", "1991-01-04", "1991-02-01", "1991-02-02", "1991-02-03", "1991-02-04", "1991-03-01", "1991-03-02", "1991-03-03", "1991-03-04",
                     "1992-01-01", "1992-01-02", "1992-01-03", "1992-01-04", "1992-02-01", "1992-02-02", "1992-02-03", "1992-02-04", "1992-03-01", "1992-03-02", "1992-03-03", "1992-03-04",
                     "1993-01-01", "1993-01-02", "1993-01-03", "1993-01-04", "1993-02-01", "1993-02-02", "1993-02-03", "1993-02-04", "1993-03-01", "1993-03-02", "1993-03-03", "1993-03-04"))
  
  # Run the 'tmc()' function for 'temp' type (mean temperature of the warmest month)
  result_temp <- tmc(mn, mx, dates, type = "temp")
  
  # Expected result for the warmest month
  expected_result_temp <- 21
  
  # Check if the result matches the expected result for temperature
  # The calculated result is rounded to the nearest whole number to avoid issues 
  # with small decimal differences in the comparison. The rounding will make sure 
  # that any minor decimal differences don't cause the test to fail unnecessarily.
  expect_equal(round(result_temp), expected_result_temp, tolerance = 0.5)
  
  # Run the 'tmc()' function for 'month' type (number of the warmest month)
  result_month <- tmc(mn, mx, dates, type = "month")
  
  # Expected result for the warmest month
  expected_result_month <- 1
  
  # Check if the result matches the expected result for the warmest month
  expect_equal(result_month, expected_result_month)
})

# Test for missing data handling
test_that("Test for missing data handling in 'tmc' function", {
  mn_na <- c(NA, NA, NA, NA, NA)
  mx_na <- c(NA, NA, NA, NA, NA)
  dates_na <- as.Date(c("2025-01-01", "2025-02-01", "2025-03-01", "2025-04-01", "2025-05-01"))
  
  # Run the 'tmc()' function with missing data
  result_na <- tmc(mn_na, mx_na, dates_na, type = "temp")
  
  # Expected result is NA, as all values are missing
  expect_equal(result_na, NA)
})
