
library(testthat)
source("R/ta.R")

#' Test for the 'ta' function (Thermal Amplitude Calculation)
#'

# Test data
mx <- c(30, 32, 29, 28)  # Maximum temperatures
mn <- c(15, 16, 14, 13)  # Minimum temperatures
dates <- as.Date(c("2025-01-01", "2025-01-02", "2025-01-03", "2025-01-04"))

#' @export
test_that("Test for daily thermal amplitude calculation", {
  # Test data
  mx <- c(30, 32, 29, 28)  # Maximum temperatures
  mn <- c(15, 16, 14, 13)  # Minimum temperatures
  dates <- as.Date(c("2025-01-01", "2025-01-02", "2025-01-03", "2025-01-04"))
  
  # Run the 'ta()' function
  result <- ta(mx, mn, dates)
  
  # Expected result (thermal amplitudes)
  expected_result <- c(15, 16, 15, 15)
  
  # Check if the result matches the expected result
  expect_equal(result, expected_result)
})
