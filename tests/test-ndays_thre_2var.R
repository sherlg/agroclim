
library(testthat)
library(zoo)
source("R/ndays_thre_2var.R")

#' Test for the 'ndays_thre_2var' function
#'

#' @export
test_that("Test for 'ndays_thre_2var' function using default parameters", {
  # Test data
  set.seed(123) 
  dates <- seq(as.Date("2018-10-01"), as.Date("2022-03-31"), by = "day")
  data1 <- round(runif(length(dates), min = -2, max = 10), 1)
  data2 <- round(runif(length(dates), min = 4, max = 37), 1)

  # Test
  result <- ndays_thre_2var(var1 = data1, var2 = data2, dates = dates) 
  expected_result <- c(301, 292, 226)
  expect_equal(result, expected_result, info = "Ndays calculation failed")
})

test_that("Test for 'ndays_thre_2var' function especifying period", {
  # Test data
  set.seed(123) 
  dates <- seq(as.Date("2018-10-01"), as.Date("2022-03-31"), by = "day")
  data1 <- round(runif(length(dates), min = -2, max = 10), 1)
  data2 <- round(runif(length(dates), min = 4, max = 37), 1)
  
  # Test
  result <- ndays_thre_2var(var1 = data1, var2 = data2, dates, start_day = "01-01", end_day = "04-30") 
  expected_result <- c(109, 101, 95,  69)
  expect_equal(result, expected_result, info = "Ndays calculation failed")
})

test_that("Test for 'ndays_thre_2var' function especifying thresholds", {
  # Test data
  set.seed(123) 
  dates <- seq(as.Date("2018-10-01"), as.Date("2022-03-31"), by = "day")
  data1 <- round(runif(length(dates), min = -2, max = 10), 1)
  data2 <- round(runif(length(dates), min = 4, max = 37), 1)
  
  # Test
  result <- ndays_thre_2var(var1 = data1, var2 = data2, dates = dates, start_day = "01-01", end_day = "04-30", 
                            threshold1 = 2, direction1 = "geq",
                            threshold2 = 14, direction2 = "leq") 
  expected_result <- c(20, 20, 26, 17)
  expect_equal(result, expected_result, info = "Ndays calculation failed")
})

test_that("Test for 'ndays_thre_2var' function especifying min duration period", {
  # Test data
  set.seed(123) 
  dates <- seq(as.Date("2018-10-01"), as.Date("2022-03-31"), by = "day")
  data1 <- round(runif(length(dates), min = -2, max = 10), 1)
  data2 <- round(runif(length(dates), min = 4, max = 37), 1)
  
  # Test
  result <- ndays_thre_2var(var1 = data1, var2 = data2, dates = dates, start_day = "01-01", end_day = "04-30", 
                            threshold1 = 2, direction1 = "geq",
                            threshold2 = 14, direction2 = "leq",
                            min_duration = 3) 
  expected_result <- c(0, 0, 0, 3)
  expect_equal(result, expected_result, info = "Ndays calculation failed")
})
