#' Maximum Temperature of the Warmest Month
#' 
#' @description Calculates the average of the daily maximum temperatures of the warmest month of the year, based on the highest mean temperature.
#' @param mn vector of daily minimum temperature series.
#' @param mx vector of daily maximum temperature series.
#' @param dates vector of dates corresponding to the daily temperature series.
#' @param type type of output. It can be "temp" for temperature or "month" for the number of the warmest month.
#' @param ... any other argument. It is expected to be "na.rm" in case NA values are required to be removed.
#'
#' @returns
#' If `type` is "temp", it returns the average of the daily maximum temperatures of the warmest month.
#' If `type` is "month", it returns the number of the warmest month (in "MM" format).
#' @export
#'
#' @examples
#' 

txmc <- function(mn, mx, dates, type = 'temp', ...){
  # Return NA if all values in mn or mx are missing
  if (all(is.na(mn)) || all(is.na(mx))) {
    return(NA)
  } else {
    # Calculate the daily average temperature
    daily_tmean <- (mn + mx) / 2
    # Aggregate by month
    ag <- aggregate(daily_tmean, by = list(substr(dates, 1, 7)), FUN = 'mean', ...)
    # Aggregate the monthly averages by month
    ag_month <- aggregate(ag[, 2], by = list(substr(ag[, 1], 6, 7)), FUN = 'mean', ...)
    
    # Find the month with the highest average temperature
    warmest_month_index <- which.max(ag_month[, 2])
    
    # Extract the month corresponding to the warmest month
    warmest_month <- ag_month[warmest_month_index, 1]
    
    # Extract the dates of the warmest month
    warmest_month_dates <- substr(dates, 6, 7) == warmest_month
    
    # Calculate the daily maximum temperatures for the warmest month
    warmest_month_mx <- mx[warmest_month_dates]
    
    # Return either the warmest month or the mean temperature of the warmest month
    if(type == 'temp') {
      z <- mean(warmest_month_mx, ...)
    } else if(type == 'month') {
      z <- warmest_month
    } else {
      stop("Invalid 'type' argument. Use 'temp' or 'month'.")
    }
  }
  return(z)
}
