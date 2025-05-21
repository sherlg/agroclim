#' Frost Probability for a Specific Day
#' 
#' @description Calculates the probability of frost occurrence on a specific calendar day across 
#' all available years, based on daily minimum temperature records and a specified temperature threshold.
#' @param mn A numeric vector of daily minimum temperature series.
#' @param dates A vector of dates corresponding with daily temperature series.
#' @param thres A numeric value indicating the temperature threshold considered to trigger frost occurrence (0 by default).
#' @param day A character string representing the calendar day in "mm-dd" format.
#' 
#' @return
#' A numeric value between 0 and 1 indicating the frost probability is returned.
#' 
#' @details A frost is defined as a day when the minimum temperature is below the specified threshold.
#' The probability of frost on a given day is computed as:
#' 
#' ph = (Number of years with frost (<0ºC) on that day) / (Total number of years with data for that day)
#'
#' @examples
#' 

ph <- function(mn, dates, thres = 0, day = NULL){
  # Check if the lengths of the temperature and dates vectors are the same
  if (length(mn) != length(dates)) {
    stop("Temperatures and dates must have the same length.")
  }
  
  # Check if a specific day has been provided
  if (is.null(day)){
    stop("A specific day to calculate frost probability must be selected.")
  }
  
  # Extract "mm-dd" format from the dates
  day_month <- format(dates, "%m-%d")
  
  # Filter for the target day
  selected_day <- which(day_month == day)
  
  # Check if there is data available for the specified day
  if (length(selected_day) == 0) {
    warning("No data available for the specified day.")
    return(NA)
  }
  
  # Subset the temperatures for the selected day
  temp_day <- mn[selected_day]
  
  # Remove NA values from the temperature data for that day
  temp_day <- temp_day[!is.na(temp_day)]
  
  # Calculate the proportion of years with frost (temperature < threshold)
  frost_days <- sum(temp_day < thres)
  total_days <- length(temp_day)
  
  # Calculate frost probability
  prob <- frost_days / total_days
  
  return(prob)
}