#' Length of the Frost Period (Guaranteed Frost Days)
#' 
#' @description Calculates the length of the periods where the daily mean temperature has been below 0°C over years.
#' @param mn A numeric vector of daily minimum temperature series.
#' @param mx A numeric vector of daily maximum temperature series.
#' @param dates A vector of dates corresponding with daily temperature series.
#' @param thres A numeric value indicating the temperature threshold considered to trigger frost occurrence (0 by default).
#' @param min_duration A numeric value indicating the minimum day length of the frost period to be considered. Periods shorter than this will be excluded. Default to 3.
#' @param period A character string indicating the type of period to calculate. It can be `"all"` for all frost periods where the temperature is below the threshold, `"first"` for the first frost period, or `"longest"` for the first longest frost period. `"all"` by default.
#' @param type type of output. It can be `"number"` for the length of the period or `"date"` for data format (`"dd-mm"`). `"number"` by default.
#' @return Depending on the argument 'type', the output will be:
#' - A numeric vector with the length of each period (if `type == "number"`)
#' - A list with two dates (start and end) for the period in "mm-dd" format (if `type == "date"`).
#' 
#' @import zoo
#' 
#' 
#' 

hs <- function(mn, mx, dates, thres = 0, min_duration = 3, period = "all", type = "number") {
  
  # Check if input lengths are the same
  if (length(mn) != length(mx) || length(mn) != length(dates)) {
    stop("mn, mx, and dates must have the same length.")
  }
  
  # Ensure the minimum duration is at least 1
  if (min_duration < 1) {
    stop("The minimum duration must be at least 1.")
  }
  
  # Calculate the daily average temperature
  mm <- (mn + mx) / 2
  
  # Create a zoo object with average temperatures and dates
  temperature_data <- zoo(mm, dates)
  
  # Extract month and day from the date index
  days <- format(index(temperature_data), "%m-%d")
  
  # Identify days with temperatures under the threshold across all years
  frost_days <- tapply(coredata(temperature_data), days, function(x) all(x < thres))
  
  # Filter for days where all years have temperatures under the threshold
  common_frost_days <- names(frost_days[frost_days == TRUE])  
  
  # Check if any common frost days exist
  if (length(common_frost_days) == 0) {
    return(NA)
  }
  
  # Transform into Date format
  common_frost_days <- as.Date(common_frost_days, format="%m-%d")
  
  # Remove duplicate frost days and keep unique ones and NAs values
  unique_frost_dates <- unique(common_frost_days)
  unique_frost_dates <- na.omit(unique_frost_dates)
  
  # Calculate the differences between consecutive dates
  date_diffs <- diff(unique_frost_dates)
  
  # Initialize list to store frost periods
  frost_periods_list <- list()
  
  # Start with the first frost date
  period_start <- unique_frost_dates[1]
  
  for (i in 2:length(unique_frost_dates)) {
    # If the gap between consecutive dates is greater than 1, start a new period
    if (date_diffs[i - 1] > 1) {
      frost_periods_list <- c(frost_periods_list, list(c(period_start, unique_frost_dates[i - 1])))
      period_start <- unique_frost_dates[i]
    }
  }
  
  # Add the final frost period
  frost_periods_list <- c(frost_periods_list, list(c(period_start, unique_frost_dates[length(unique_frost_dates)])))
  
  # Format the periods as strings and remove duplicates within each period
  consecutive_periods <- lapply(frost_periods_list, function(x) format(unique(x), "%m-%d"))
  
  # Filter the periods that have a length greater than or equal to min_duration
  consecutive_periods_filtered <- consecutive_periods[sapply(consecutive_periods, length) >= min_duration]
  
  # Select the appropriate frost period based on the 'period' parameter
  if (length(consecutive_periods_filtered) == 0) {
    return(NA) # No common frost periods across all years
  } else if (period == "first") {
    result_period <- consecutive_periods_filtered[[1]]
    result_period <- list(result_period)
  } else if (period == "longest") {
    period_lengths <- sapply(consecutive_periods_filtered, length) 
    longest_period_index <- which.max(period_lengths) 
    result_period <- consecutive_periods_filtered[[longest_period_index]] 
    result_period <- list(result_period)
  } else if (period == "all") {
    result_period <- consecutive_periods_filtered
  } else {
    stop("Invalid period type specified.")
  }
  
  # Return results based on the requested output type
  if (type == "number") {
    frost_period_numbers <- sapply(result_period, length)
    return(frost_period_numbers)
#    return(as.list(frost_period_numbers))
  } else if (type == "date") {
    # Return the start and end dates of the frost periods
    frost_period_dates <- list()
    
    for (period in result_period) {
      if (length(period) > 1) {
        # If the group has more than one date, return the first and last date
        frost_period_dates <- append(frost_period_dates, list(c(period[1], period[length(period)])))
      } else {
        frost_period_dates <- append(frost_period_dates, period)
      }
    }
    
    return(frost_period_dates)
  } else {
    stop("Invalid output type specified.")
  }
}