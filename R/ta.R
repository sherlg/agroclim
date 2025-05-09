#' Thermal Amplitude
#' 
#' @description 
#' Calculates the daily thermal amplitude based on the series of daily minimum 
#' and maximum temperatures.
#' 
#' @param mn A numeric vector representing the daily minimum temperatures.
#' @param mx A numeric vector representing the daily maximum temperatures.
#' @param dates A vector of dates corresponding to the daily temperature series.
#'
#' @returns 
#' A numeric vector of daily thermal amplitudes, calculated as the difference between 
#' the daily maximum and minimum temperatures for each day.
#'

ta <- function(mx, mn, dates) {
  # Check if the dimensions of mx and mn match
  if (length(mx) != length(mn)) {
    stop("Dimensions of mx and mn do not match")
  }
  
  # Calculate the thermal amplitude
  tmp = mx - mn
  
  # Check if the length of the temperature arrays matches the length of dates
  if(length(tmp) != length(dates)) {
    stop("Dimensions of temperature arrays do not match the length of dates")
  }
  
  return(tmp)
}