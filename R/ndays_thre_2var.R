#' Count number of days meeting threshold conditions on two variables within seasons for at least a defined number of consecutive days.
#' 
#' @param var1 Numeric vector representing the first variable to be analyzed (e.g., temperature).
#' @param var2 Numeric vector representing the second variable to be analyzed (e.g., precipitation).
#' @param dates Vector of class `Date` corresponding to both `var1` and `var2`.
#' @param start_day Character string in "mm-dd" format indicating the start of the season. Optional; default is "07-01".
#' @param end_day Character string in "mm-dd" format indicating the end of the season. Optional; default is "06-30".
#' @param threshold1 Numeric value specifying the threshold for the first variable's condition. Optional; default is 0.
#' @param direction1 Character string specifying the condition direction for `threshold1`; valid options are `"geq"` (greater than or equal) and `"leq"` (less than or equal). Optional; default is `"geq"`.
#' @param threshold2 Numeric value specifying the threshold for the second variable's condition. Optional; default is 0.
#' @param direction2 Character string specifying the condition direction for `threshold2`; valid options are `"geq"` and `"leq"`. Optional; default is `"geq"`.
#' @param min_duration Numeric value indicating the minimum number of consecutive days that must meet both conditions simultaneously. Optional; default is 1.
#'
#' @return Numeric vector indicating the number of days per season meeting both conditions for at least `min_duration` consecutive days.
#'
#' @details Only complete seasons are included in the analysis, except for the last one. If the first season does not contain 
#' all required days based on `start_day` and `end_day`, it is excluded. The last season is included even if incomplete, with analysis 
#' running up to the last available date.
#'
#' @examples
#' # Count days with minimum temperature ≥ 5 and maximum temperature ≤ 18 between July 1 and June 30 seasons
#' ndays_thre_2vars(var1 = mn, var2 = mx, dates = date_seq,
#'                 threshold1 = 5, direction1 = "geq",
#'                 threshold2 = 18, direction2 = "leq")
#' 
#' @import zoo
#' @export

ndays_thre_2var <- function(var1, var2, dates, start_day = "07-01", end_day = "06-30",
                            threshold1 = 0, direction1 = "geq",
                            threshold2 = 0, direction2 = "geq",
                            min_duration = 1) {
  # Validate inputs
  if(length(var1) != length(dates) || length(var2) != length(dates)) stop("var1, var2, and dates must have the same length.")
  if(!direction1 %in% c("geq", "leq")) stop('direction1 must be "geq" or "leq".')
  if(!direction2 %in% c("geq", "leq")) stop('direction2 must be "geq" or "leq".')
  
  # Create zoo objects
  var1_data <- zoo(var1, order.by = dates)
  var2_data <- zoo(var2, order.by = dates)
  
  # Find indices for start and end of seasons
  start_idx <- which(format(time(var1_data), "%m-%d") == start_day)
  end_idx <- which(format(time(var1_data), "%m-%d") == end_day)
  
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
      en <- length(var1_data)
    } else {
      en <- end_idx[i]
    }
    
    period_var1 <- var1_data[st:en]
    period_var2 <- var2_data[st:en]
    
    # Condition for var1
    cond1 <- if(direction1 == "geq") {
      period_var1 >= threshold1
    } else {
      period_var1 <= threshold1
    }
    
    # Condition for var2
    cond2 <- if(direction2 == "geq") {
      period_var2 >= threshold2
    } else {
      period_var2 <= threshold2
    }
    
    # Logical vector where BOTH conditions are TRUE
    days_cond <- as.logical(cond1 & cond2)

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
