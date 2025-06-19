#' Count number of days above or below a threshold within seasons for at least a defined number of consecutive days.
#' 
#' @param var Numeric vector representing the variable to be analyzed (e.g., temperature, precipitation).
#' @param dates Vector of class `Date` corresponding to `var`.
#' @param start_day Character string in "mm-dd" format indicating the start of the season. Optional; default is "07-01".
#' @param end_day Character string in "mm-dd" format indicating the end of the season. Optional; default is "06-30".
#' @param threshold Numeric value specifying the threshold for condition evaluation. Optional; default is 0.
#' @param direction Character string specifying the condition direction; valid options are `"geq"` (greater than or equal to `threshold`) and `"leq"` (less than or equal to `threshold`). Optional; default is `"geq"`.
#' @param min_duration Numeric value indicating the minimum number of consecutive days that must meet the condition. Optional; default is 1.
#' @param min_threshold Numeric value specifying the minimum threshold for condition evaluation. Optional; default is NULL.
#' @param min_direction Character string specifying the condition direction for `min_threshold`; valid options are `"geq"` and `"leq"`. Optional; default is `"geq"`.
#' @param max_threshold Numeric value specifying the maximum threshold for condition evaluation. Optional; default is NULL.
#' @param max_direction Character string specifying the condition direction for `max_threshold`; valid options are `"geq"` and `"leq"`. Optional; default is `"leq"`.
#'
#' @return Numeric vector indicating the number of days meeting the condition per season.
#'
#' @details #' Only complete seasons are included in the analysis, except for the last one. If the first season does not contain 
#' all required days based on `start_day` and `end_day`, it is excluded. The last season is included even if incomplete, with analysis 
#' running up to the last available date.
#'
#' @examples
#' # Count days with temperature >= 15 between July 1 and June 30 seasons
#' ndays_thre_1var(var = temp_data, dates = date_seq, threshold = 15, direction = "geq")
#' 
#' # Count days with temperature between 10 (≥ 10) and 20 (≤ 20) within seasons
#' ndays_thre_1var(var = temp_data, dates = date_seq,
#'                min_threshold = 10, min_direction = "geq",
#'                max_threshold = 20, max_direction = "leq")
#' 
#' @import zoo
#' @export
#' 
ndays_thre_1var <- function(var, dates, start_day = "07-01", end_day = "06-30", 
                            threshold = 0, direction = "geq", min_duration = 1,
                            min_threshold = NULL, min_direction = "geq",
                            max_threshold = NULL, max_direction = "leq") {
  # Validate inputs
  if(length(var) != length(dates)) stop("var and dates must have the same length.")
  if(!direction %in% c("geq", "leq")) stop('direction must be "geq" or "leq".')
  if(!is.null(min_direction) && !min_direction %in% c("geq", "leq")) stop('min_direction must be "geq" or "leq".')
  if(!is.null(max_direction) && !max_direction %in% c("geq", "leq")) stop('max_direction must be "geq" or "leq".')
  
  # Check exclusive use of threshold/direction vs min/max thresholds
  use_range <- !is.null(min_threshold) || !is.null(max_threshold)
  use_single <- is.null(min_threshold) && is.null(max_threshold)
  
  if(!use_range && !use_single) {
    stop("Either provide threshold and direction OR min_threshold and/or max_threshold, not both.")
  }
  
  # Create zoo object
  var_data <- zoo(var, order.by = dates)
  
  # Find indices for start and end of seasons
  start_idx <- which(format(time(var_data), "%m-%d") == start_day)
  end_idx <- which(format(time(var_data), "%m-%d") == end_day)
  
  if(length(start_idx) == 0 || length(end_idx) == 0) {
    stop("No seasons found matching the specified start or end day.")
  }
  
  # Adjust end_idx if first end_day is before first start_day
  if(length(end_idx) > 0 && end_idx[1] < start_idx[1]) {
    end_idx <- end_idx[-1]
  }
  
  results <- numeric(length(start_idx))
  
  for(i in seq_along(start_idx)) {
    st <- start_idx[i]
    # For last season, go to last available day
    if(i == length(start_idx)) {
      en <- length(var_data)
    } else {
      en <- end_idx[i]
    }
    
    period_data <- var_data[st:en]
    
    # Determine condition depending on parameters used
    if(use_single) {
      if(direction == "geq") {
        days_cond <- period_data >= threshold
      } else if(direction == "leq") {
        days_cond <- period_data <= threshold
      }
    } else if(use_range) {
      days_cond_min <- rep(TRUE, length(period_data))
      days_cond_max <- rep(TRUE, length(period_data))
      
      if(!is.null(min_threshold)) {
        if(min_direction == "geq") {
          days_cond_min <- period_data >= min_threshold
        } else if(min_direction == "leq") {
          days_cond_min <- period_data <= min_threshold
        }
      }
      
      if(!is.null(max_threshold)) {
        if(max_direction == "geq") {
          days_cond_max <- period_data >= max_threshold
        } else if(max_direction == "leq") {
          days_cond_max <- period_data <= max_threshold
        }
      }
      
      days_cond <- days_cond_min & days_cond_max
    }
    
    # Convert days_cond to a simple logical vector
    days_cond <- as.logical(days_cond)
    
    # Apply run-length encoding to identify consecutive runs of TRUE/FALSE
    r <- rle(days_cond)
    
    # Keep only runs of TRUE that are at least min_duration long
    r$values <- r$values & r$lengths >= min_duration
    
    # Reconstruct a logical vector marking only those valid runs as TRUE
    valid_days <- inverse.rle(r)     
    
    # Sum days per season
    results[i] <- sum(valid_days, na.rm = TRUE)
  }
  
  return(results)
}
