#' Compute coefficients of a non-stationary extreme value model using a
#' peaks-over-threshold approach and a Poisson-General Pareto (P-GP) model.
#'
#' @param var Numeric vector representing the variable to be analyzed (e.g., temperature, precipitation).
#' @param dates Vector of class `Date` corresponding to `var`.
#' @param start_day Character string in "mm-dd" format indicating the start of the season. Default: "07-01".
#' @param end_day Character string in "mm-dd" format indicating the end of the season. Default: "06-30".
#' @param threshold Numeric value specifying the threshold for condition evaluation. Optional; default is 0.
#' @param direction Character string specifying the condition direction; valid options are `"geq"` (greater than or equal to `threshold`) and `"leq"` (less than or equal to `threshold`). Optional; default is `"geq"`.
#' @param gap_length Numeric value specifying the maximum allowed gap inside an event. Optional; default is 0 (no gaps allowed).
#'
#' @description Create a table of events from time series data. It results in
#' a three-columns matrix indicating the starting and ending times of the
#' events and their duration, maximum magnitude and integral magnitude.
#'
#' @return A data.frame.
#'
#' @examples
#' @export
#'
find_events <- function(var,
                       dates = NULL,
                       start_day = "07-01",
                       end_day = "06-30",
                       threshold = 0,
                       direction = "geq",
                       gap_length = 0) {
  
  # Validate inputs
  if (anyNA(var)) stop("NA not allowed in the variable to be analyzed")
  if (length(var) != length(dates)) stop("Length of 'var' and 'dates' must be equal.")
  if (!direction %in% c("geq", "leq")) stop("Invalid 'direction'. Use 'geq' or 'leq'.")

  # Check if a date is within the growing season
  within_season <- function(date) {
    md <- format(date, "%m-%d")
    if (start_day <= end_day) {
      return(md >= start_day & md <= end_day)
    } else {
      return(md >= start_day | md <= end_day)
    }
  }
  
  # Create a logical vector with the indices of dates included inside the growing season
  in_season <- sapply(dates, within_season)  
  
  # Determine events
  n <- length(var)
  starts <- ends <- ints <- ints_med <- mags <- vector(mode = "numeric", length = 0)
  isevent <- FALSE
  
  for (i in 1:n) {
    # Based on direction parameter
    in_condition <- (direction == "geq" && var[i] >= threshold) || (direction == "leq" && var[i] <= threshold)
    out_condition <- (direction == "geq" && var[i] < threshold) || (direction == "leq" && var[i] > threshold)
    
    if (!isevent && in_condition && in_season[i]) {
      # Start event only if it's within the season
      isevent <- TRUE
      inicio <- i
      starts <- append(starts, inicio)
    }
    if (isevent && (out_condition || !in_season[i])) {
      # End event if data drops below threshold or goes out of season
      isevent <- FALSE
      fin <- i - 1
      ends <- append(ends, fin)
    }
  }
  
  # Close event if it ends on the last day
  if (isevent) {
    ends <- append(ends, n)
  }

  # Create an empty dates vector if not provided by user
  if (is.null(dates)) {
    dates <- rep(NA, n)
  }
  
  # Merge events no further than gap_length days away
  n <- length(starts)
  gap <- starts[2:n] - ends[1:{
    n - 1
  }] - 1 <= gap_length
  while (sum(gap) > 0) {
    # Merge first two events
    i <- which(gap)[1]
    ends[i] <- ends[i + 1]
    starts[i + 1] <- ends[i + 1] <- NA
    starts <- na.omit(starts)
    ends <- na.omit(ends)
    n <- length(starts)
    gap <- starts[2:n] - ends[1:{
      n - 1
    }] <= gap_length
  }
  
  # Determine event's intensity and magnitude (integral over the threshold)
  n <- length(starts)
  for (i in c(1:n)) {
    segment <- var[starts[i]:ends[i]]
    ints <- c(ints, max(segment))
    ints_med <- c(ints_med, mean(segment))
    mags <- if (direction == "geq") {
      c(mags, sum(segment - threshold))
    } else {
      c(mags, sum(threshold - segment))
    }
  }
  
  # Return result
  return(
    data.frame(
      pos_start = starts,
      pos_end = ends,
      date_start = dates[starts],
      date_end = dates[ends],
      dur = ends - starts + 1,
      int = ints,
      ints_med = ints_med, # In reference to the tmean
      mag = mags
    )
  )
}
