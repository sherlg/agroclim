#' Calculate the first or last day in the year when the probability of a climatic event meets a threshold
#'
#' @param var Numeric vector representing the variable to be analyzed (e.g., temperature, precipitation).
#' @param dates Vector of class `Date` corresponding to `var`.
#' @param start_day Character string in "mm-dd" format indicating the start of the season. Optional; default is "07-01".
#' @param end_day Character string in "mm-dd" format indicating the end of the season. Optional; default is "06-30".
#' @param threshold Numeric value specifying the threshold for condition evaluation. Optional; default is 0.
#' @param direction Character string specifying the condition direction; valid options are `"geq"` (greater than or equal to `threshold`) and `"leq"` (less than or equal to `threshold`). Optional; default is `"geq"`.
#' @param event Character string: `"first"` for earliest occurrence or `"last"` for latest. Optional; default is `"first"`.
#' @param prob Numeric value between 0 and 1 indicating a quantile-based threshold. Optional; default is 0.10.
#'
#' @return Numeric vector with the Julian day (day of year) corresponding to the event probability threshold.
#'
#' @import zoo
#' @export
#' 
event_prob_day <- function(var, dates, start_day = "07-01", end_day = "06-30", threshold = 0, direction = "geq", event = "first", prob = 0.10) {
  # Validate inputs
  if (length(var) != length(dates)) stop("Length of 'var' and 'dates' must be equal.")
  if (!direction %in% c("geq", "leq")) stop("Invalid 'direction' argument. Must be either 'geq' or 'leq'.")
  if (!event %in% c("first", "last")) stop("Invalid 'event' argument. Must be either 'first' or 'last'.")
  
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
    if(i == length(start_idx)) {
      en <- length(var_data)
    } else {
      en <- end_idx[i]
    }
    
    period_data <- var_data[st:en]
    period_values <- coredata(period_data)
    period_dates <- time(period_data)
    
    # Determine direction depending on parameters used
    if(direction == "geq") {
      days_cond <- period_data >= threshold
    } else if(direction == "leq") {
      days_cond <- period_data <= threshold
    }
    
    # If no day within the period meets the condition,skip that iteration and move to the next cycle
    if (!any(days_cond, na.rm = TRUE)) {
      next
    }
    
    event_indices <- which(days_cond)
    if (event == "first") {
      chosen_idx <- min(event_indices)
    } else {
      chosen_idx <- max(event_indices)
    }    
    results[i] <- as.numeric(format(period_dates[chosen_idx], "%j"))
    
  }

  # Calculate quantile depending on event type
  if(event == "first") {
    return(unname(round(quantile(results, prob, na.rm = TRUE))))
  } else {
    return(unname(round(quantile(results, 1 - prob, na.rm = TRUE))))
  }

}
