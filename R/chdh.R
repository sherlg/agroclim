#' Freeze-Thaw Cycles
#' 
#' @description Calculates the number of days in each seasonal range where the minimum temperature is less than or equal to a 
#' threshold and the maximum temperature is greater than or equal to another threshold.
#' @param mn A numeric vector of daily minimum temperature values.
#' @param mx A numeric vector of daily maximum temperature values.
#' @param dates A vector of dates corresponding to the daily temperature series.
#' @param iniday Start date of the analysis period, in text format ("mm-dd"). Defaults to `"01-01"`.
#' @param endday End date of the analysis period, in text format ("mm-dd"). Defaults to `"12-31"`.
#' @param thres_mn A numeric value indicating the threshold for the daily minimum temperature (default is -2).
#' @param thres_mx A numeric value indicating the threshold for the daily maximum temperature (default is 2).
#' @return A numeric vector representing the total annual duration (in days) of the freeze-thaw cycles.
#' 
#' @details
#' The condition for a Freeze-Thaw Cycle day is: minimum temperature less than or equal to `thresh_min` AND maximum temperature 
#' greater than or equal to `thresh_max`.
#' Only complete seasons are included in the results. If data for a season is missing at the beginning or end,
#' that season will return NA.
#'
#' @import zoo
#' 

chdh <- function(mn, mx, dates, iniday = '07-01', endday = '06-30', thres_mn = -2, thres_mx = 2) {
  # Check if input lengths are the same
  if (length(mn) != length(mx) || length(mn) != length(dates)) {
    stop("mn, mx, and dates must have the same length.")
  }
  
  # Create a zoo object with temperatures and dates
  temperature_data <- zoo(cbind(min_temp = mn, max_temp = mx), order.by = dates)
  
  # Identify start and end indices of each season
  start_idx <- which(format(time(temperature_data), '%m-%d') == iniday)
  end_idx <- which(format(time(temperature_data), '%m-%d') == endday)
  
  if(length(start_idx) == 0 || length(end_idx) == 0) {
    stop("No seasons found matching the specified start or end day.")
  }
  
  # Adjust end_idx if the first endday is before the iniday
  if(length(end_idx) > 0 && end_idx[1] < start_idx[1]) {
    end_idx <- end_idx[-1]
  }
  
  n_days_cond <- rep(NA, length(start_idx))
  
  for(i in seq_along(start_idx)) {
    st <- start_idx[i]
    if (i == length(start_idx)) {
      en <- nrow(temperature_data)
    } else {
      en <- end_idx[i]
    }
    
    # Subset of data corresponding to the season
    period_data <- temperature_data[st:en, ]
    
    # Identify days with min_temp <= threshold and max_temp >= threshold
    days_cond <- period_data[, "min_temp"] <= thres_mn & period_data[, "max_temp"] >= thres_mx
    
    # Sum season days of the freeze-thaw cycles
    n_days_cond[i] <- sum(days_cond, na.rm = TRUE)
  }
  
  return(n_days_cond)
}

  