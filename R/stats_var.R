#' Calculate seasonal summary statistics (max, min, mean, sum) of a numeric climate variable.
#'
#' @param var Numeric vector representing the variable to be analyzed (e.g., temperature, precipitation).
#' @param dates Vector of class `Date` corresponding to `var`.
#' @param start_day Character string in "mm-dd" format indicating the start of the season. Optional; default is "07-01".
#' @param end_day Character string in "mm-dd" format indicating the end of the season. Optional; default is "06-30".
#' @param operator Character string indicating the operation to apply per season. Valid options are `"max"`, `"min"`, `"mean"` and `"sum"`. Optional; default is "mean".
#'
#' @return Numeric vector with the result of the chosen operator per season.
#'
#' @examples
#' stats_var(var = temp_data, dates = date_seq, operator = "mean")
#'
#' @import zoo

stats_var <- function(var, dates, start_day = "07-01", end_day = "06-30", operator = "mean") {
  # Validations
  if (length(var) != length(dates)) stop("temp and dates must have the same length.")
  if (!operator %in% c("max", "min", "mean", "sum")) {
    stop('operator must be one of: "max", "min", "mean", "sum".')
  }
  
  # Create zoo object
  var_data <- zoo(var, order.by = dates)
  
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
    
    # Compute statistic based on operator
    results[i] <- switch(operator,
                         max = max(period_data, na.rm = TRUE),
                         min = min(period_data, na.rm = TRUE),
                         mean = mean(period_data, na.rm = TRUE),
                         sum = sum(period_data, na.rm = TRUE))
  }
  
  return(results)
}
