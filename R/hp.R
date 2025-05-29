#' Length of the Probable Frost Period (Probable Frost Days)
#' 
#' @description Calculates the number of days where the absolute minimum temperature is under a threshold and the daily 
#' minimum temperature is above a threshold within a predefined period.
#' @param mn A numeric vector of daily minimum temperature series.
#' @param dates A vector of dates corresponding with daily temperature series.
#' @param iniday first day of the growing season, in text format ("mm-dd"). Defaults to `07-01`.
#' @param endday last day of the growing season, in text format ("mm-dd"). Defaults to `06-30`.
#' @param thres A numeric value indicating the daily minimum temperature threshold considered to trigger frost occurrence (0 by default).
#' @param thres_abs A numeric value indicating the absolute minimum temperature threshold considered to trigger frost occurrence (0 by default).
#' 
#' @return A numeric vector with the number of probable frost days (days where the absolute minimum temperature is under a threshold 
#' and the daily minimum temperature is above a threshold) per season is returned.
#' 
#' @details Despite the logical threshold of temperature is 0 ºC to determine frost occurrence, the arguments "thres" and "thres_abs" are open to 
#' change in case of different units of temperature.
#' Only complete seasons are included in the analysis, except for the last one. If the first season does not contain all required 
#' days (based on start and end indices), it is excluded from the results. The last season, however, is included even if it is 
#' not complete, and the data is analyzed up to the last available date provided in the indices.
#' 
#' @import zoo
#' 


hp <- function(mn, dates, iniday = '07-01', endday = '06-30', thres = 0, thres_abs = 0) {
  # Check if input lengths are the same
  if (length(mn) != length(dates)) {
    stop("mn and dates must have the same length.")
  }

  # Create a zoo object with temperatures and dates
  temperature_data <- zoo(cbind(temp = mn), order.by = dates)

  # Extract month and day from the date index
  days <- format(index(temperature_data), "%m-%d")
  
  # Calculate the daily absolute minimum temperatures for the set of years and identify which are under the threshold
  daily_mn_abs <- tapply(coredata(temperature_data), days, function(x) {
    mn_temp <- min(x)
    return(mn_temp < thres_abs) 
  })
  
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
    
    # Extraer 'mm-dd' del periodo
    period_days <- format(index(period_data), "%m-%d")
    
    # Identify days with tmin abs < 0
    days_cond_abs <- daily_mn_abs[period_days]
    
    # Identify days with min temp > thres & tmin abs < 0
    days_cond <- period_data[, "temp"] > thres & days_cond_abs
    
    # Sum days per season
    n_days_cond[i] <- sum(days_cond, na.rm = TRUE)
  }
  return(n_days_cond)

}


