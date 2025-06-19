#' Calculate Extreme Temperature Range (ETR) at different time scales
#'
#' @param mx Numeric vector of daily maximum temperatures.
#' @param mn Numeric vector of daily minimum temperatures.
#' @param dates Vector of class `Date` corresponding to `var`.
#' @param start_day Character string in "mm-dd" format indicating the start of the season. Only used when `time_scale = "season"`. Optional; default is "01-01".
#' @param end_day Character string in "mm-dd" format indicating the end of the season. Only used when `time_scale = "season"`. Optional; default is "12-31".
#' @param time_scale Character string specifying the time aggregation scale. Valid options are `"day"`, `"month"`, and `"season"`. Optional; default is `"day"`.
#
#' @return Numeric vector with ETR values per period based on selected time scale.
#' 
#' @details When `time_scale = "season"`, only complete seasons are included in the analysis, except for the last one. If the first season does not contain 
#' all required days based on `start_day` and `end_day`, it is excluded. The last season is included even if incomplete, with analysis 
#' running up to the last available date.
#' 
#' @examples
#' ETR per month
#' extreme_temperature_range(tmax, tmin, dates, time_scale = "month")
#'
#' @import zoo
#'
extreme_temperature_range <- function(mx, mn, dates, start_day = "01-01", end_day = "12-31", time_scale = "day") {
  if (length(mx) != length(mn) || length(mx) != length(dates)) {
    stop("tmax, tmin, and dates must be of the same length.")
  }
  
  time_scale <- tolower(time_scale)
  
  if (time_scale == "day") { # Return daily ETR (is the daily amplitude: Tmax - Tmin)
    return(mx - mn)
  } else if (time_scale == "month") { # ETR = max(Tmax) - min(Tmin) per month
    max_tmax <- tapply(mx, format(dates, "%Y-%m"), max, na.rm = TRUE)
    min_tmin <- tapply(mn, format(dates, "%Y-%m"), min, na.rm = TRUE)
    return(as.vector(unname(max_tmax - min_tmin)))
  } else if (time_scale == "season") { # ETR = max(Tmax) - min(Tmin) per custom season (e.g., July to June)
    # Find indices for start and end of seasons
    start_idx <- which(format(dates, "%m-%d") == start_day)
    end_idx <- which(format(dates, "%m-%d") == end_day)
    
    if (length(start_idx) == 0 || length(end_idx) == 0) {
      stop("No matching dates found for the specified season start or end day.")
    }
    
    # Adjust end_idx if first end_day is before first start_day
    if (end_idx[1] < start_idx[1]) {
      end_idx <- end_idx[-1]
    }
    
    etr_seasons <- numeric(length(start_idx))
    
    for(i in seq_along(start_idx)) {
      st <- start_idx[i]
      # For last season, go to last available day
      if(i == length(start_idx)) {
        en <- length(mx)
      } else {
        en <- end_idx[i]
      }
      
      tmax_season <- mx[st:en]
      tmin_season <- mn[st:en]
      
      etr_seasons[i] <- max(tmax_season, na.rm = TRUE) - min(tmin_season, na.rm = TRUE)
    }
    
    return(etr_seasons)
    
  } else {
    stop('time_scale must be one of: "day", "month", "season".')
  }
}
