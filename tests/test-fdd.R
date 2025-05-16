library(testthat)
source("R/fdd.R")

#' Test for the 'fdd' function (Chilling Hours Calculation)
#'
# Test for specific season dates
test_that("Test for chilling hours calculation", {
  # Test data: temperatures and dates
  mn <- c(5, 6, 7, 8, 3, 4, 5, 6, 2, 1, 0)
  mx <- c(10, 12, 14, 15, 10, 11, 12, 13, 9, 8, 7)
  dates <- as.Date(c("2024-11-01", "2024-11-02", "2024-11-03", "2024-11-04", "2024-11-05", "2024-11-06", "2024-11-07", "2024-11-08", 
                     "2024-11-09", "2024-11-10", "2024-11-11"))
  
  # Run the 'fdd' function
  result_chilling_hours <- fdd(mx, mn, dates, iniday = "11-01", endday = "11-05") 
  
  # Expected result
  expected_result_chilling_hours <- 30
  
  # Check if the result matches the expected result for the chilling quantification
  expect_true(abs(result_chilling_hours - expected_result_chilling_hours) <= 1)
})

# Test for the northern hemisphere (November to February)
test_that("Test for chilling hours calculation", {
  # Test data: temperatures and dates
  set.seed(123) 
  dates <- seq(as.Date("2021-10-01"), as.Date("2022-03-31"), by = "day")
  mn <- round(runif(length(dates), min = -2, max = 8), 1)
  mx <- round(runif(length(dates), min = 5, max = 18), 1)

  # Run the 'fdd' function. Specific latitude for the northern hemisphere
  result_chilling_hours <- fdd(mx, mn, dates, lati = 40)
  
  # Expected result
  expected_result_chilling_hours <- 1606  
  
  # Check if the result matches the expected result for the chilling quantification
  expect_true(abs(result_chilling_hours - expected_result_chilling_hours) <= 1)
})


# Test for the southern hemisphere (May to August)
test_that("Test for chilling hours calculation in southern hemisphere", {
  # Test data: temperatures and dates
  set.seed(123) 
  dates <- seq(as.Date("2021-05-01"), as.Date("2021-08-31"), by = "day")
  mn <- round(runif(length(dates), min = -2, max = 8), 1)
  mx <- round(runif(length(dates), min = 5, max = 18), 1)
  
  # Run the 'fdd' function. Specific latitude for the southern hemisphere
  result_chilling_hours <- fdd(mx, mn, dates, lati = -40)  # Latitud del hemisferio sur

  # Expected result
  expected_result_chilling_hours <- 1519

  # Check if the result matches the expected result for the chilling quantification
  expect_true(abs(result_chilling_hours - expected_result_chilling_hours) <= 1)
})

# Test for missing data handling
test_that("Test for missing data handling in 'fdd' function", {
  # Test data: temperatures and dates
  mn <- c(5, 6, NA, 8, 3, 4, 5, 6, 2, 1, 0)
  mx <- c(10, 12, 14, 15, 10, 11, NA, 13, 9, 8, 7)
  dates <- as.Date(c("2024-11-01", "2024-11-02", "2024-11-03", "2024-11-04", "2024-11-05", "2024-11-06", "2024-11-07", "2024-11-08", 
                     "2024-11-09", "2024-11-10", "2024-11-11"))
  
  # Check if fdd throws an error
  expect_error(
    fdd(mx, mn, dates, lati = 40),
    regexp = "`mx` and `mn` must not contain NA values."
  )
})

