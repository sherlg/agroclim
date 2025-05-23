#' Length of the Non-Frost Period (Frost-Free Days)
#' 
#' @description Calculates the length of the periods where the the daily minimum temperature has been above the critical frost temperature over the years.
#' @param mn A numeric vector of daily minimum temperature series.
#' @param dates A vector of dates corresponding with daily temperature series.
#' @param iniday first day of the growing season, in text format ("mm-dd"). Defaults to `07-01`.
#' @param endday last day of the growing season, in text format ("mm-dd"). Defaults to `06-30`.
#' @param thres A numeric value indicating the temperature threshold considered to trigger frost occurrence (0 by default).
#' @param min_duration A numeric value indicating the minimum day length of the non-frost period to be considered. Periods shorter than this will be excluded. Default to 3.
#' @param period A character string indicating the type of period to calculate. It can be `"all"` for all non-frost periods where the temperature is above the threshold, `"first"` for the first non-frost period, or `"longest"` for the first longest non-frost period. `"all"` by default.
#' @param type type of output. It can be `"number"` for the length of the period or `"date"` for data format (`"dd-mm"`). `"number"` by default.
#' @return Depending on the argument 'type', the output will be:
#' - A list of numeric values with the length of each period (if `type == "number"`)
#' - A list with two dates (start and end) for the period in "mm-dd" format (if `type == "date"`).
#' 
#' @import zoo
#' @import lubridate
#' 
#' 

hn <- function(mn, dates, iniday = '07-01', endday = '06-30', thres = 0, min_duration = 3, period = "all", type = "number") {
  # Check if mn, and dates are of the same length
  if (length(mn) != length(dates)) {
    stop("mn and dates must have the same length.")
  }
  
  # Ensure the minimum duration is at least 1
  if (min_duration < 1) {
    stop("The minimum duration must be at least 1.")
  }
  
  # Check if a date is within the growing season
  within_season <- function(date) {
    md <- format(date, "%m-%d")
    if (iniday <= endday) {
      return(md >= iniday & md <= endday)
    } else {
      return(md >= iniday | md <= endday)
    }
  }
  
  # Create a logical vector with the indices of dates included inside the growing season
  indices_season <- sapply(dates, within_season)
  
  # Create a subset of minimum temperatures and dates for the growing season
  mn_season <- mn[indices_season]
  dates_season <- dates[indices_season]
  
  # Create a zoo object with minimum temperatures and dates
  temperature_data <- zoo(mn_season, dates_season)
  
  # Extract month and day from the date index
  days <- format(index(temperature_data), "%m-%d")
  
  # Identify days with temperatures under the threshold across all years
  non_frost_days <- tapply(coredata(temperature_data), days, function(x) all(x > thres))
    
  # Filter for days where all years have temperatures under the threshold
  common_non_frost_days <- names(non_frost_days[non_frost_days == TRUE])  
  
  # Check if any common non-frost days exist
  if (length(common_non_frost_days) == 0) {
    return(NA)
  }
  
  # Transform into Date format (keeping season order)
  common_non_frost_days_asdate <- as.Date(common_non_frost_days, format="%m-%d") 
  iniday_date <- as.Date(iniday, format="%m-%d")
  common_non_frost_days_season <- lapply(common_non_frost_days_asdate, function(date) {
    if (is.na(date)) {
      return(NA)
    } else if (date < iniday_date) {
      return(date + years(1))
    } else {
      return(date)
    }
  })
  common_non_frost_days_season <- sort(as.Date(unlist(common_non_frost_days_season)))
  
  # Remove duplicate non-frost days and keep unique ones and NAs values
  unique_non_frost_dates <- unique(common_non_frost_days_season)
  unique_non_frost_dates <- na.omit(unique_non_frost_dates)
  
  # Calculate the differences between consecutive dates
  date_diffs <- diff(unique_non_frost_dates)
  
  # Initialize list to store non_frost periods
  non_frost_periods_list <- list()
  
  # Start with the first non-frost date
  period_start <- unique_non_frost_dates[1]
  
  for (i in 2:length(unique_non_frost_dates)) {
    # If the gap between consecutive dates is greater than 1, start a new period
    if (date_diffs[i - 1] > 1) {
      non_frost_periods_list <- c(non_frost_periods_list, list(c(period_start, unique_non_frost_dates[i - 1])))
      period_start <- unique_non_frost_dates[i]
    }
  }
  
  # Add the final non_frost period
  non_frost_periods_list <- c(non_frost_periods_list, list(c(period_start, unique_non_frost_dates[length(unique_non_frost_dates)])))
  
  # Filter the periods that have a length greater than or equal to min_duration
  non_frost_periods_list_filtered <- non_frost_periods_list[
    sapply(non_frost_periods_list, function(d) length(d) > 1 && as.numeric(d[2] - d[1]) + 1 >= min_duration)
  ]
  
  # Select the appropriate non-frost period based on the 'period' parameter
  if (length(non_frost_periods_list_filtered) == 0) {
    return(NA) # No common non-frost periods across all years
  } else if (period == "first") {
    value_first <- non_frost_periods_list_filtered[[1]]
    result_period <- list(value_first)
  } else if (period == "longest") {
    period_lengths <- lapply(non_frost_periods_list_filtered, function(period) {
      as.Date(period[2]) - as.Date(period[1]) + 1
    })
    indice_longest <- which.max(period_lengths)
    value_longest <- non_frost_periods_list_filtered[[indice_longest]]
    result_period <- list(value_longest)
  } else if (period == "all") {
    result_period <- non_frost_periods_list_filtered
  } else {
    stop("Invalid period type specified.")
  }
  
  # Return results based on the requested output type
  if (type == "number") {
    non_frost_period_numbers <- lapply(result_period, function(period) {
      as.numeric(as.Date(period[2]) - as.Date(period[1]) + 1)
    })
    return(non_frost_period_numbers)
  } else if (type == "date") {
    non_frost_period_dates <- list()
    
    for (period in result_period) {
      if (length(period) > 1) {
        non_frost_period_dates <- append(non_frost_period_dates, list(c(period[1], period[length(period)])))
      } else {
        non_frost_period_dates <- append(non_frost_period_dates, period)
      }
    }
    non_frost_period_dates <- lapply(non_frost_period_dates, function(x) format(unique(x), "%m-%d"))
    return(non_frost_period_dates)
  } else {
    stop("Invalid output type specified.")
  }
}