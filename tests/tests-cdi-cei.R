# Examples

# Preamble -----

library(data.table)
library(dplyr)
library(zoo)

source("./ndays_thre_1var.R")
source("./functions.R")

generate_example_climate <- function(
    n_stations = 2,
    start_date = as.Date("2019-06-01"),
    end_date   = as.Date("2021-07-31"),
    seed = 123
) {
  set.seed(seed)
  dates <- seq.Date(start_date, end_date, by = "day")
  n_days <- length(dates)
  
  stations <- paste0("S", seq_len(n_stations))
  DT <- data.table(
    id = rep(stations, each = n_days),
    date = rep(dates, times = n_stations)
  )
  
  # synthetic daily variables with some structure
  DT[, doy := as.integer(format(date, "%j"))]
  
  # Temperature with annual cycle (°C)
  DT[, temp := 15 + 10 * sin(2 * pi * doy / 365) +
       rnorm(.N, sd = 3) +
       ifelse(id == "S2", 1.5, 0)]  # station offset
  
  # Relative humidity (%)
  DT[, humidity := 70 - 20 * sin(2 * pi * doy / 365) + rnorm(.N, sd = 5)]
  
  # Wind speed (m/s)
  DT[, wind := abs(rnorm(.N, mean = 4, sd = 1)) +
       1.5 * cos(2 * pi * doy / 10)]
  
  # Insert some missing values randomly
  DT[sample(.N, 50), temp := NA]
  DT[sample(.N, 50), humidity := NA]
  DT[sample(.N, 50), wind := NA]
  
  # clean up
  DT[, doy := NULL]
  setorder(DT, id, date)
  return(DT)
}


# Example usage -----

# Create example dataset
clim_dt <- generate_example_climate()
head(clim_dt)
summary(clim_dt)
plot(clim_dt)


# ndays_thre_1var() ----

test_days_1var <- ndays_thre_1var(
  any = clim_dt$temp,
  dates = clim_dt$date,
  start_day = "06-01",
  end_day = "07-31",
  threshold = 15,
  direction = "geq",
  min_duration = 3
)

head(test_days_1var)
plot(test_days_1var, type = "p")

# Condition Duration Index (CDI) ----

# Define bounds (temperature and humidity within comfort zone)
bounds <- data.frame(
  var = c("temp", "humidity"),
  lower = c(15, 50),
  upper = c(30, 80),
  inc_lower = TRUE,
  inc_upper = TRUE
)

test_days_dp <- CDI(
  df = clim_dt,
  id = "id",
  start_date = min(clim_dt$date),
  bounds = bounds,
  season_start = "06-01",
  season_end = "07-31",
  combiner = "all",
  min_duration = 3
)

test_days_dp
plot(test_days_dp$cum_days, type = "l")


# Condition Excess Index (CEI) ----

test_excess_dp <- CEI(
  df = clim_dt,
  id = "id",
  x_col = "temp",
  start_date = min(clim_dt$date),
  season_start = "07-01",
  season_end = "06-30",
  lower = 25,
  upper = Inf,
  inc_lower = FALSE,
  min_duration = 2
)

test_excess_dp
plot(test_excess_dp$cum_excess, type = "l")
