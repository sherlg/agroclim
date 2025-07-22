#' Empirical Probability of a Threshold Event on a Specific Time Scale
#'
#' @description Calculates the empirical probability of a variable (e.g., temp, precip) exceeding or falling below a threshold
#' on a specific day, month, or year. 
#' @param any Numeric vector representing the variable to be analyzed (e.g., temperature, precipitation).
#' @param dates Vector of class `Date` corresponding to `any`.
#' @param start_day Character string in "mm-dd" format indicating the start of the season. Optional; used only if time_scale = `"season"`, ignored for all other values. Default: "07-01".
#' @param end_day Character string in "mm-dd" format indicating the end of the season. Optional; used only if time_scale = `"season"`, ignored for all other values. Default: "06-30".
#' @param threshold Numeric value specifying the threshold for condition evaluation. Optional; default is 0.
#' @param direction Character string specifying the condition direction; valid options are `"geq"` (greater than or equal to `threshold`) and `"leq"` (less than or equal to `threshold`). Optional; default is `"geq"`.
#' @param time_scale Character: `"day"`, `"month"`, `"year"`, or `"season"`. Optional; default is `"season"`.
#' @param target_date Character (for day: "mm-dd"; for month: "mm"; for year: "yyyy") specifying the time to compute. Optional; used only if time_scale is `"day"`, `"month"`, or `"year"`, ignored for `"season"`. 
#'
#' @return Numeric probability (between 0 and 1).
#' 
#' @details
#' The `season` time scale only computes the probability if a complete season of data is available.
#' The probability of an event on a given time scale is calculated as:
#'
#' probability = (Number of days where the condition is met, e.g. temp < 0) /
#'               (Total number of days with data for that time scale)
#'
#' For example, to calculate frost probability (temp < 0°C) on a seasonal scale,
#' the function returns the proportion of days with frost within the season.
#' 
#' @examples
#' Probability of frost (temperature < 0°C) on January 15th
#' event_prob_emp(any = temp_min, dates = date_seq, threshold = 0, direction = "leq", time_scale = "day", target_date = "01-15")
#' 
#' @import zoo
#' @export

event_prob_emp <- function(any, dates, start_day = "07-01", end_day = "06-30", threshold = 0, direction = "geq", time_scale = "day", target_date = NULL) {
  # Validate inputs
  if (length(any) != length(dates)) stop("Length of 'any' and 'dates' must be equal.")
  if (!direction %in% c("geq", "leq")) stop("Invalid 'direction'. Use 'geq' or 'leq'.")
  if (!time_scale %in% c("day", "month", "year", "season")) stop("Invalid 'time_scale'. Use 'day', 'month', 'year', or 'season'.")
  
  if (!is.null(target_date) && time_scale %in% c("day", "month", "year")) {
    # Filter based on timescale
    if (time_scale == "day") {
      indices <- which(format(dates, "%m-%d") == target_date)
    } else if (time_scale == "month") {
      indices <- which(format(dates, "%m") == target_date)
    } else if (time_scale == "year") {
      indices <- which(format(dates, "%Y") == target_date)
    }
    if (length(indices) == 0) {
      warning("No data for specified target_date and time_scale.")
      return(NA)
    }
  } else if (is.null(target_date) && !is.null(start_day) && !is.null(end_day) && time_scale == "season") {
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
    
    # Adjust start_idx if there is no endday for the last season
    if(length(start_idx) > 0 && length(start_idx) > length(end_idx)) {
      start_idx <- start_idx[1:length(end_idx)]
    }
    
    indices <- integer(0)
    
    for(i in seq_along(start_idx)) {
      indices_range <- seq(from = start_idx[i], to = end_idx[i])
      indices <- c(indices, indices_range)
    }
  } else {
    stop("Invalid combination of parameters. Use 'target_date' only with time_scale = 'day', 'month', or 'year'. Use 'start_day' and 'end_day' only when time_scale = 'season'.")
  }

  if (direction == "geq") {
    event_days <- sum(any[indices] >= threshold, na.rm = TRUE)
  } else {
    event_days <- sum(any[indices] <= threshold, na.rm = TRUE)
  }

  total_days <- sum(!is.na(any[indices]))

  if (total_days == 0) {
    return(NA)
  }
  
  prob <- event_days / total_days

  return(prob)
}
