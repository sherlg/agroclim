#' Warmest month of the year
#' 
#' @description Calculates the mean temperature of the warmest month of the year
#' @param mn vector of daily minimum temperature series.
#' @param mx vector of daily maximum temperature series.
#' @param dates vector of dates corresponding with daily temperature series
#' @param type type of output. It can be "temp" for temperature or "month" for the number of the warmest month.
#' @param ... any other argument. It is expected to be "na.rm" in case NA values are required to be removed.
#'
#' @returns
#' If `type` is "temp", it returns the average of the daily temperatures of the warmest month.
#' If `type` is "month", it returns the number of the warmest month (in "MM" format).
#' @export
#'
#' @examples
#' 

tmc <- function(mn, mx, dates, type = 'temp', ...){
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
    
    # Return either the warmest month or the mean temperature of the warmest month
    if(type == 'temp') {
      z <- max(ag_month[, 2])
    } else if(type == 'month') {
      z <- which.max(ag_month[, 2])
    } else {
      stop("Invalid 'type' argument. Use 'temp' or 'month'.")
    }
  }
  return(z)
}