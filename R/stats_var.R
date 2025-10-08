#' Calculate seasonal summary statistics (max, min, mean, sum) of a numeric climate variable.
#'
#' @param any Numeric vector representing the variable to be analyzed (e.g., temperature, precipitation).
#' @param dates Vector of class `Date` corresponding to `any`.
#' @param start_day Character string in "mm-dd" format indicating the start of the season. Optional; default is "07-01".
#' @param end_day Character string in "mm-dd" format indicating the end of the season. Optional; default is "06-30".
#' @param operator Character string indicating the operation to apply per season. Valid options are `"max"`, `"min"`, `"mean"` and `"sum"`. Optional; default is "mean".
#' @param threshold Numeric value specifying the threshold for condition evaluation. Optional; default is NULL.
#' @param direction Character string specifying the condition direction; valid options are `"geq"` (greater than or equal to `threshold`) and `"leq"` (less than or equal to `threshold`). Optional; default is `"geq"`.
#'
#' @return Numeric vector with the result of the chosen operator per season.
#'
#' @details 
#' Only complete seasons are included in the analysis, except for the last one. If the first season does not contain 
#' all required days based on `start_day` and `end_day`, it is excluded. The last season is included even if incomplete, with analysis 
#' running up to the last available date.
#' 
#' @examples
#' stats_var(any = temp_data, dates = date_seq, operator = "mean")
#' stats_var(any = temp_data, dates = date_seq, operator = "sum", threshold = 7, direction= "leq")
#'
#' @import zoo

stats_var <- function(any, dates, start_day = "07-01", end_day = "06-30", operator = "mean", threshold = NULL, direction = "geq") {
  # Validations
  if (length(any) != length(dates)) stop("temp and dates must have the same length.")
  if (!operator %in% c("max", "min", "mean", "sum")) {
    stop('operator must be one of: "max", "min", "mean", "sum".')
  }
  
  # Create zoo object
  var_data <- zoo(any, order.by = dates)
  
  # Find indices for start and end of seasons
  start_idx <- which(format(time(var_data), "%m-%d") == start_day)
  end_idx <- which(format(time(var_data), "%m-%d") == end_day)
  
  if (length(start_idx) == 0 || length(end_idx) == 0) {
    stop("No seasons found matching the specified start or end day.")
  }
  
  # Adjust end_idx if first end_day is before first start_day
  if(length(end_idx) > 0 && end_idx[1] < start_idx[1]) {
    end_idx <- end_idx[-1]
  }
  
  # If the last period does not cover all the data, the last data point is used as the end.
  if(length(end_idx) < length(start_idx)){
    end_idx <- c(end_idx, length(dates))
  }
  
  results <- numeric(length(start_idx))
  
  for(i in seq_along(start_idx)) {
    # Start and end index for the current period
    st <- start_idx[i]
    en <- end_idx[i]
    
    period_data <- var_data[st:en]
    
    # Filter using threshold parameter
    if(!is.null(threshold)) {
      if(direction == "geq") {
        days_cond <- period_data >= threshold
        period_data <- period_data[days_cond]
      } else if(direction == "leq") {
        days_cond <- period_data <= threshold
        period_data <- period_data[days_cond]
      }
    }
    
    # Compute statistic based on operator
    results[i] <- switch(operator,
                         max = max(period_data, na.rm = TRUE),
                         min = min(period_data, na.rm = TRUE),
                         mean = mean(period_data, na.rm = TRUE),
                         sum = sum(period_data, na.rm = TRUE))
  }
  
  return(results)
}
