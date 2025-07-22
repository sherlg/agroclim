#' Calculate thermal amplitude at different time scales
#'
#' @param mx Numeric vector of daily maximum temperatures.
#' @param mn Numeric vector of daily minimum temperatures.
#' @param dates Vector of class `Date` corresponding to `mx` and `mn`.
#' @param start_day Character string in "mm-dd" format indicating the start of a custom analysis period. Optional; Only used when both `start_day` and `end_day` are provided. Default is `NULL`.
#' @param end_day Character string in "mm-dd" format indicating the end of a custom analysis period. Optional; Only used when both `start_day` and `end_day` are provided. Default is `NULL`.
#' @param time_scale Character string specifying the time aggregation scale. Valid options are `"day"`, `"month"`, and `"season"`. Optional; default is `"day"`.
#'
#' @return Numeric vector with thermal amplitude values depending on the selected time scale.
#'
#' @details 
#' If `start_day` and `end_day` are both `NULL`, the function computes ETR over the entire dataset.
#' If both are provided, a specific period is defined and the `time_scale` is applied within that period 
#' (daily, monthly, or by complete seasons).
#' 
#' When a custom analysis period is specified, only complete seasons are included in the analysis, except for the last one. If the first season does not contain 
#' all required days based on `start_day` and `end_day`, it is excluded. The last season is included even if incomplete, with analysis 
#' running up to the last available date.
#'
#' @examples
#' Monthly average thermal amplitude
#' thermal_amplitude(mx, mn, dates, time_scale = "month")
#' 
#' Monthly average thermal amplitude in agronomic season
#' thermal_amplitude(mx, mn, dates, start_day = '07-01', end_day = '06-30', time_scale = "month")
#'  
#' @import zoo
#'
#'
#'
thermal_amplitude <- function(mx, mn, dates, start_day = NULL, end_day = NULL, time_scale = "day") {
  if (length(mx) != length(mn) || length(mx) != length(dates)) {
    stop("mx, mn, and dates must be of the same length.")
  }
  
  # Daily thermal amplitude
  amp_daily <- mx - mn

  time_scale <- tolower(time_scale)
  if (!(time_scale %in% c("day", "month", "season"))) {
    stop('time_scale must be one of: "day", "month", "season".')
  }
  
  if(is.null(start_day) && is.null(end_day)){
    
    if (time_scale == "day") {
      # Return daily amplitude
      return(amp_daily)
    } else if (time_scale == "month") {
      # Average daily amplitudes grouped by year-month
      monthly_amp <- tapply(amp_daily, format(dates, "%Y-%m"), mean, na.rm = TRUE)
      return(as.vector(unname(monthly_amp)))
    } else if (time_scale == "season") {
      return(mean(amp_daily, na.rm = TRUE))
    }
    
  } else if (!is.null(start_day) && !is.null(end_day)) {

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
    
    # Si la útima temporada no abarca todos los datos se pone como fin el último dato
    if(length(end_idx) < length(start_idx)){
      end_idx <- c(end_idx, length(dates))
    }
    
    results <- list()

    for(i in seq_along(start_idx)) {
      st <- start_idx[i]
      en <- end_idx[i]
      
      if (time_scale == "day") {
        # Return daily amplitude
        results[[i]] <- amp_daily[st:en]
      } else if (time_scale == "month") {
        # Average daily amplitudes grouped by year-month
        monthly_amp <- tapply(amp_daily[st:en], format(dates[st:en], "%Y-%m"), mean, na.rm = TRUE)
        results[[i]] <- as.vector(unname(monthly_amp))
      } else if (time_scale == "season") {
        results[[i]] <- mean(amp_daily[st:en], na.rm = TRUE)
      }
    }
  return(unlist(results))
  } else {
    stop("To specify a period, both start_day and end_day must be provided.")
  }
}
