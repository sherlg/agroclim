#' Minimum Temperature of the Coldest Month
#' 
#' @description Calculates the average of the daily minimum temperatures of the coldest month of the year, based on the lowest mean temperature.
#' @param mn vector of daily minimum temperature series.
#' @param mx vector of daily maximum temperature series.
#' @param dates vector of dates corresponding with daily temperature series
#' @param type type of output. It can be "temp" for temperature or "month" for the number of the coldest month.
#' @param ... any other argument. It is expected to be "na.rm" in case NA values are required to be removed.
#'
#' @returns
#' If `type` is "temp", it returns the average of the daily minimum temperatures of the coldest month.
#' If `type` is "month", it returns the number of the coldest month (in "MM" format).
#' @export
#'
#' @examples
#' 

tnmf <- function(mn, mx, dates, type = 'temp', ...){
  # Return NA if all values in mn or mx are missing
  if (all(is.na(mn)) || all(is.na(mx))) {
    return(NA)
  } else{
    # Calculate the daily average temperature
    daily_tmean <- (mn + mx) / 2
    # Aggregate by month
    ag <- aggregate(daily_tmean, by = list(substr(dates, 1, 7)), FUN = 'mean', ...)
    # Aggregate the monthly averages by month
    ag_month <- aggregate(ag[, 2], by = list(substr(ag[, 1], 6, 7)), FUN = 'mean', ...)
    
    # Find the month with the lowest average temperature
    coldest_month_index <- which.min(ag_month[, 2])
    
    # Extract the month corresponding to the coldest month
    coldest_month <- ag_month[coldest_month_index, 1]
    
    # Extract the dates of the coldest month
    coldest_month_dates <- substr(dates, 6, 7) == coldest_month
    
    # Calculate the average of the daily minimum temperatures for the coldest month
    coldest_month_mn <- mn[coldest_month_dates]
    
    # Return either the coldest month or the mean temperature of the coldest month
    if(type == 'temp') {
      z <- mean(coldest_month_mn, ...)
    } else if(type == 'month') {
      z <- coldest_month
    } else {
      stop("Invalid 'type' argument. Use 'temp' or 'month'.")
    }
  }
  return(z)
}