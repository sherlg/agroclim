#' Calculate Extreme Temperature Range (ETR) at different time scales
#'
#' @param mx Numeric vector of daily maximum temperatures.
#' @param mn Numeric vector of daily minimum temperatures.
#' @param dates Vector of class `Date` corresponding to `mx` and `mn`.
#' @param start_day Character string in "mm-dd" format indicating the start of a custom analysis period. Optional; Only used when both `start_day` and `end_day` are provided. Default is `NULL`.
#' @param end_day Character string in "mm-dd" format indicating the end of a custom analysis period. Optional; Only used when both `start_day` and `end_day` are provided. Default is `NULL`.
#' @param time_scale Character string specifying the time aggregation scale. Valid options are `"day"`, `"month"`, and `"season"`. Default is `"day"`.
#
#' @return Numeric vector with ETR values per period based on the selected time scale.
#' 
#' @details 
#' If `start_day` and `end_day` are both `NULL`, the function computes ETR over the entire dataset.
#' If both are provided, a specific period is defined and the `time_scale` is applied within that period 
#' (daily, monthly, or by complete seasons).
#' 
#' When a custom analysis period is specified, only complete periods are included in the analysis, except for the last one. If the first period does not contain 
#' all required days based on `start_day` and `end_day`, it is excluded. The last period is included even if incomplete, with analysis 
#' running up to the last available date.
#' 
#' @examples
#' ETR per month
#' extreme_temperature_range(mx, mn, dates, time_scale = "month")
#' 
#' ETR per month in agronomic season
#' extreme_temperature_range(mx, mn, dates, start_day = '07-01', end_day = '06-30', time_scale = "month")
#'
#' @import zoo
#'

extreme_temperature_range <- function(mx, mn, dates, start_day = NULL, end_day = NULL, time_scale = "day") {
  if (length(mx) != length(mn) || length(mx) != length(dates)) {
    stop("mx, mn, and dates must be of the same length.")
  }

  time_scale <- tolower(time_scale)
  if (!(time_scale %in% c("day", "month", "season"))) {
    stop('time_scale must be one of: "day", "month", "season".')
  }

  # If no start_day and end_day are specified, the analysis is applied to the full dataset
  if(is.null(start_day) && is.null(end_day)){

    if (time_scale == "day") { # Return daily ETR (is the daily amplitude: Tmax - Tmin)
      etr <- mx - mn
      etr[!is.finite(mx) | !is.finite(mn)] <- NA
      return(etr)

    } else if (time_scale == "month") { # ETR = max(Tmax) - min(Tmin) per month
      max_tmax <- tapply(mx, format(dates, "%Y-%m"), function(x) {
        x <- x[is.finite(x)]
        if (length(x) == 0) NA else max(x)
      })
      min_tmin <- tapply(mn, format(dates, "%Y-%m"), function(x) {
        x <- x[is.finite(x)]
        if (length(x) == 0) NA else min(x)
      })

      etr <- max_tmax - min_tmin
      etr[!is.finite(max_tmax) | !is.finite(min_tmin)] <- NA
      return(as.vector(unname(etr)))

    } else if (time_scale == "season") { # ETR = max(Tmax) - min(Tmin) per complete season
      if (!any(is.finite(mx)) || !any(is.finite(mn))) {
        return(NA)
      }
      return(max(mx, na.rm = TRUE) - min(mn, na.rm = TRUE))
    }

  # If start_day and end_day are specified
  } else if (!is.null(start_day) && !is.null(end_day)) {

    # Find indices for start and end of period
    start_idx <- which(format(dates, "%m-%d") == start_day)
    end_idx <- which(format(dates, "%m-%d") == end_day)

    if (length(start_idx) == 0 || length(end_idx) == 0) {
      stop("No matching dates found for the specified season start or end day.")
    }

    # Adjust end_idx if first end_day is before first start_day
    if (end_idx[1] < start_idx[1]) {
      end_idx <- end_idx[-1]
    }

    # If the last period does not cover all the data, the last data point is used as the end.
    if(length(end_idx) < length(start_idx)){
      end_idx <- c(end_idx, length(dates))
    }

    etr_seasons <- list()

    for (i in seq_along(start_idx)) {

      # Start and end index for the current period
      st <- start_idx[i]
      en <- end_idx[i]

      tmax_season <- mx[st:en]
      tmin_season <- mn[st:en]
      dates_season <- dates[st:en]

      if (time_scale == "day") {
        etr <- tmax_season - tmin_season
        etr[!is.finite(tmax_season) | !is.finite(tmin_season)] <- NA
        etr_seasons[[i]] <- etr

      } else if (time_scale == "month") {
        max_tmax <- tapply(tmax_season, format(dates_season, "%Y-%m"), function(x) {
          x <- x[is.finite(x)]
          if (length(x) == 0) NA else max(x)
        })
        min_tmin <- tapply(tmin_season, format(dates_season, "%Y-%m"), function(x) {
          x <- x[is.finite(x)]
          if (length(x) == 0) NA else min(x)
        })

        etr <- max_tmax - min_tmin
        etr[!is.finite(max_tmax) | !is.finite(min_tmin)] <- NA
        etr_seasons[[i]] <- as.vector(unname(etr))

      } else if (time_scale == "season") {
        if (!any(is.finite(tmax_season)) || !any(is.finite(tmin_season))) {
          etr_seasons[[i]] <- NA
        } else {
          etr_seasons[[i]] <- max(tmax_season, na.rm = TRUE) - min(tmin_season, na.rm = TRUE)
        }
      }
    }
    return(unlist(etr_seasons))
  } else {
    stop("To specify a period, both start_day and end_day must be provided.")
  }
}

