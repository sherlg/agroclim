# Examples

# Preamble -----

library(data.table)
library(dplyr)
library(zoo)

source("./R/ndays_thre_1var.R")
source("./R/ndays_thre_2var.R")
source("./R/general_functions.R")

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

# Exportar excel con datos para comprobación manual
#writexl::write_xlsx(clim_dt, path = "./comprobación_functions.xlsx",)

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

# Se comprueban todas las condiciones
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

# con que se cumpla 1 vale
test_days_dp <- CDI(
  df = clim_dt,
  id = "id",
  start_date = min(clim_dt$date),
  bounds = bounds,
  season_start = "06-01",
  season_end = "07-31",
  combiner = "any",
  min_duration = 1
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
  min_duration = 3
)

test_excess_dp
plot(test_excess_dp$cum_excess, type = "l")


# Pruebas CDI -----

##### Pruebas sin cruzar temporadas #####

# Ndays con temp >= 15 (1 variable 1 threshold)
bounds_1thre <- data.frame(
  var = c("temp"),
  lower = c(15),
  inc_lower = TRUE
)

test_days_dp_1thre <- CDI(
  df = clim_dt,
  id = "id",
  start_date = min(clim_dt$date),
  bounds = bounds_1thre,
  season_start = "06-01",
  season_end = "07-31",
  combiner = "all",
  min_duration = 3
)

res <- test_days_dp_1thre %>% 
  group_by(id, season_id) %>% 
  summarise(max_cum_days = max(cum_days, na.rm = TRUE))


test_days_1var <- ndays_thre_1var(
  any = clim_dt$temp[1:792], # Solo permite 1 estación
  dates = clim_dt$date[1:792], # Solo permite 1 estación
  start_day = "06-01",
  end_day = "07-31",
  threshold = 15,
  direction = "geq",
  min_duration = 3
)

test_days_1var==res$max_cum_days[1:3]


# Ndays con temp >= 15 & <25 (1 variable 2 threholds)
bounds_2thre <- data.frame(
  var = c("temp"),
  lower = c(15),
  upper = c(25),
  inc_lower = TRUE,
  inc_upper = FALSE
)

test_days_dp_2thre <- CDI(
  df = clim_dt,
  id = "id",
  start_date = min(clim_dt$date),
  bounds = bounds_2thre,
  season_start = "06-01",
  season_end = "07-31",
  combiner = "all",
  min_duration = 3
)

res_2thre <- test_days_dp_2thre %>% 
  group_by(id, season_id) %>% 
  summarise(max_cum_days = max(cum_days, na.rm = TRUE))

test_days_1var_2thre <- ndays_thre_1var(
  any = clim_dt$temp[1:792], # Solo permite 1 estación
  dates = clim_dt$date[1:792], # Solo permite 1 estación
  start_day = "06-01",
  end_day = "07-31",
  min_threshold = 15,
  min_direction = "geq",
  max_threshold = 25,
  max_direction = "leq", #Sería 24 pero la función toma 24 literal, sin tener en cuenta los decimales
  min_duration = 3
)

test_days_1var_2thre==res_2thre$max_cum_days[1:3]


# Ndays con temp <= 15 y humidity>=60 (2 variables, 1 threshold cada una)
bounds_2var_1thre <- data.frame(
  var = c("temp", "humidity"),
  lower = c(-Inf, 60),
  upper = c(15, Inf),
  inc_lower = TRUE,
  inc_upper = TRUE
)

test_days_dp_2var_1thre <- CDI(
  df = clim_dt,
  id = "id",
  start_date = min(clim_dt$date),
  bounds = bounds_2var_1thre,
  season_start = "06-01",
  season_end = "07-31",
  combiner = "all",
  min_duration = 3
)

res_2var_1thre <- test_days_dp_2var_1thre %>% 
  group_by(id, season_id) %>% 
  summarise(max_cum_days = max(cum_days, na.rm = TRUE))

test_days_2var_1thre <- ndays_thre_2var(
  any = clim_dt$temp[1:792], # Solo permite 1 estación
  any2 = clim_dt$humidity[1:792], # Solo permite 1 estación
  dates = clim_dt$date[1:792], # Solo permite 1 estación
  start_day = "06-01",
  end_day = "07-31",
  threshold1 = 15,
  direction1 = "leq",
  threshold2 = 60,
  direction2 = "geq", 
  min_duration = 3
)

test_days_2var_1thre==res_2var_1thre$max_cum_days[1:3]


# Ndays con temp >= 15 & <=30 y humidity >= 50 & <=80 (2 variables, 2 threshold cada una)
bounds_2var_2thre <- data.frame(
  var = c("temp", "humidity"),
  lower = c(15, 50),
  upper = c(30, 80),
  inc_lower = TRUE,
  inc_upper = TRUE
)

test_days_dp_2var_2thre <- CDI(
  df = clim_dt,
  id = "id",
  start_date = min(clim_dt$date),
  bounds = bounds_2var_2thre,
  season_start = "06-01",
  season_end = "07-31",
  combiner = "all",
  min_duration = 3
)

res_2var_2thre <- test_days_dp_2var_2thre %>% 
  group_by(id, season_id) %>% 
  summarise(max_cum_days = max(cum_days, na.rm = TRUE))
#Comprobación manual OK (S1-1, S1-2)

##### Mismas pruebas pero cruzando años para comprobar temporadas #####

# Ndays con temp >= 15 (1 variable 1 threshold)
bounds_1thre <- data.frame(
  var = c("temp"),
  lower = c(15),
  inc_lower = TRUE
)

test_days_dp_1thre <- CDI(
  df = clim_dt,
  id = "id",
  start_date = min(clim_dt$date),
  bounds = bounds_1thre,
  season_start = "05-01",
  season_end = "07-31",
  combiner = "all",
  min_duration = 3
)

res <- test_days_dp_1thre %>% 
  group_by(id, season_id) %>% 
  summarise(max_cum_days = max(cum_days, na.rm = TRUE))

test_days_1var <- ndays_thre_1var(
  any = clim_dt$temp[c(184:245, 550:611)], # Solo permite 1 estación
  dates = clim_dt$date[c(184:245, 550:611)], # Solo permite 1 estación
  start_day = "12-01",
  end_day = "01-31",
  threshold = 15,
  direction = "geq",
  min_duration = 3
)

test_days_1var==res$max_cum_days[1:2]



# Ndays con temp >= 15 & <25 (1 variable 2 threholds)
bounds_2thre <- data.frame(
  var = c("temp"),
  lower = c(15),
  upper = c(25),
  inc_lower = TRUE,
  inc_upper = FALSE
)

test_days_dp_2thre <- CDI(
  df = clim_dt,
  id = "id",
  start_date = min(clim_dt$date),
  bounds = bounds_2thre,
  season_start = "12-01",
  season_end = "01-31",
  combiner = "all",
  min_duration = 3
)

res_2thre <- test_days_dp_2thre %>% 
  group_by(id, season_id) %>% 
  summarise(max_cum_days = max(cum_days, na.rm = TRUE))

test_days_1var_2thre <- ndays_thre_1var(
  any = clim_dt$temp[c(184:245, 550:611)], # Solo permite 1 estación
  dates = clim_dt$date[c(184:245, 550:611)], # Solo permite 1 estación
  start_day = "12-01",
  end_day = "01-31",
  min_threshold = 15,
  min_direction = "geq",
  max_threshold = 25,
  max_direction = "leq", #Sería 24 pero la función toma 24 literal, sin tener en cuenta los decimales
  min_duration = 3
)

test_days_1var_2thre==res_2thre$max_cum_days[1:2]


# Ndays con temp <= 15 y humidity>=60 (2 variables, 1 threshold cada una)
bounds_2var_1thre <- data.frame(
  var = c("temp", "humidity"),
  lower = c(-Inf, 60),
  upper = c(15, Inf),
  inc_lower = TRUE,
  inc_upper = TRUE
)

test_days_dp_2var_1thre <- CDI(
  df = clim_dt,
  id = "id",
  start_date = min(clim_dt$date),
  bounds = bounds_2var_1thre,
  season_start = "12-01",
  season_end = "01-31",
  combiner = "all",
  min_duration = 3
)

res_2var_1thre <- test_days_dp_2var_1thre %>% 
  group_by(id, season_id) %>% 
  summarise(max_cum_days = max(cum_days, na.rm = TRUE))

test_days_2var_1thre <- ndays_thre_2var(
  any = clim_dt$temp[c(184:245, 550:611)], # Solo permite 1 estación
  any2 = clim_dt$humidity[c(184:245, 550:611)], # Solo permite 1 estación
  dates = clim_dt$date[c(184:245, 550:611)], # Solo permite 1 estación
  start_day = "12-01",
  end_day = "01-31",
  threshold1 = 15,
  direction1 = "leq",
  threshold2 = 60,
  direction2 = "geq", 
  min_duration = 3
)

test_days_2var_1thre==res_2var_1thre$max_cum_days[1:2]


# Ndays con temp >= 15 & <=30 y humidity >= 50 & <=80 (2 variables, 2 threshold cada una)
bounds_2var_2thre <- data.frame(
  var = c("temp", "humidity"),
  lower = c(15, 50),
  upper = c(30, 80),
  inc_lower = TRUE,
  inc_upper = TRUE
)

test_days_dp_2var_2thre <- CDI(
  df = clim_dt,
  id = "id",
  start_date = min(clim_dt$date),
  bounds = bounds_2var_2thre,
  season_start = "12-01",
  season_end = "01-31",
  combiner = "all",
  min_duration = 3
)

res_2var_2thre <- test_days_dp_2var_2thre %>% 
  group_by(id, season_id) %>% 
  summarise(max_cum_days = max(cum_days, na.rm = TRUE))
#Comprobación manual OK (S1-1, S1-2)


##### Pruebas parametro COMBINER -> "any" #####

# Ndays
bounds_2var_2thre <- data.frame(
  var = c("temp", "humidity", "wind"),
  lower = c(15, 50, 2),
  upper = c(30, 80, 3),
  inc_lower = TRUE,
  inc_upper = TRUE
)

test <- CDI(
  df = clim_dt,
  id = "id",
  start_date = min(clim_dt$date),
  bounds = bounds_2var_2thre,
  season_start = "12-01",
  season_end = "01-31",
  combiner = "any",
  min_duration = 3
)

res <- test %>% 
  group_by(id, season_id) %>% 
  summarise(max_cum_days = max(cum_days, na.rm = TRUE))
# Comprobado con 3 -> S1-1 53, S1-2 54
# Comprobado con 2 -> S1-1 52, S1-2 52

##### Pruebas parametro K cruzando temporada #####
bounds_2var_2thre <- data.frame(
  var = c("temp", "humidity", "wind"),
  lower = c(15, 50, 2),
  upper = c(30, 80, 3),
  inc_lower = TRUE,
  inc_upper = TRUE
)

# Ejemplo con k=1
test <- CDI(
  df = clim_dt,
  id = "id",
  start_date = min(clim_dt$date),
  bounds = bounds_2var_2thre,
  season_start = "12-01",
  season_end = "01-31",
  combiner = "k_of_n",
  k = 1,
  min_duration = 3
)

res <- test %>% 
  group_by(id, season_id) %>% 
  summarise(max_cum_days = max(cum_days, na.rm = TRUE))

# k of n = 1 sería igual que el parámetro combiner = "any"
# Comprobado con 3 -> S1-1 53, S1-2 54


# Ejemplo con k=2
test <- CDI(
  df = clim_dt,
  id = "id",
  start_date = min(clim_dt$date),
  bounds = bounds_2var_2thre,
  season_start = "12-01",
  season_end = "01-31",
  combiner = "k_of_n",
  k = 2,
  min_duration = 3
)

res <- test %>% 
  group_by(id, season_id) %>% 
  summarise(max_cum_days = max(cum_days, na.rm = TRUE))
# Comprobado con 3 -> S1-1 22, S1-2 23


# Pruebas CEI ----------

source("./R/stats_var.R")

# 1 variable, 1 threshold, 1 min_duration
test_cei <- CEI(
  df = clim_dt,
  id = "id",
  x_col = "temp",
  start_date = min(clim_dt$date),
  season_start = "07-01",
  season_end = "06-30",
  lower = 10,
  upper = Inf,
  inc_lower = FALSE,
  min_duration = 1
)

res <- test_cei %>% 
  group_by(id, season_id) %>% 
  summarise(cum_excess = max(cum_excess, na.rm = TRUE))


vals <- clim_dt$temp[c(31:396, 397:761)] - 10   # Solo permite 1 estación

test_stats_var <- stats_var(
  any = vals,
  dates = clim_dt$date[c(31:396, 397:761)], # Solo permite 1 estación
  start_day = "07-01",
  end_day = "06-30",
  operator = "sum",
  threshold = 0,
  direction = "geq"
)

round(test_stats_var, 0) == round(res$cum_excess[2:3], 0)



# 2 threshold, 3 min_duration
test_cei_2 <- CEI(
  df = clim_dt,
  id = "id",
  x_col = "temp",
  start_date = min(clim_dt$date),
  season_start = "07-22",
  season_end = "08-10",
  lower = 10,
  upper = 20,
  inc_lower = TRUE,
  inc_upper = TRUE,
  min_duration = 3
)

res <- test_cei_2 %>% 
  group_by(id, season_id) %>% 
  summarise(cum_excess = max(cum_excess, na.rm = TRUE))
res
# Comprobado manual -> S1-1 19.2574218382072, S1-2 13.0857121355528


# Parámetros na_action = "skip_days"
test_cei_na <- CEI(
  df = clim_dt,
  id = "id",
  x_col = "temp",
  start_date = min(clim_dt$date),
  season_start = "07-22",
  season_end = "08-10",
  lower = 10,
  upper = Inf,
  inc_lower = FALSE,
  min_duration = 1,
  na_action = "skip_days" # Elimina del análisis los dias con NA y así no tienen influencia en min_duration
)

res_na <- test_cei_na %>% 
  group_by(id, season_id) %>% 
  summarise(cum_excess = max(cum_excess, na.rm = TRUE))
res_na




# PRUEBAS CON PARAMETRO NA ----
# na_action = c("false", "skip_vars", "skip_days")[1])
bounds_2var_1thre <- data.frame(
  var = c("temp", "humidity"),
  lower = c(-Inf, 60),
  upper = c(15, Inf),
  inc_lower = TRUE,
  inc_upper = TRUE
)

test_days_dp_2var_1thre_na <- CDI(
  df = clim_dt,
  id = "id",
  start_date = min(clim_dt$date),
  bounds = bounds_2var_1thre,
  season_start = "06-01",
  season_end = "07-31",
  combiner = "all",
  min_duration = 3,
  na_action = "skip_days" # Elimina del análisis los días con valores NA
)


test_days_dp_2var_1thre_na2 <- CDI(
  df = clim_dt,
  id = "id",
  start_date = min(clim_dt$date),
  bounds = bounds_2var_1thre,
  season_start = "06-01",
  season_end = "07-31",
  combiner = "all",
  min_duration = 3,
  na_action = "skip_vars" # Ignora del análisis los valores NA de esa columna (no se cuentan ni como verdaderos ni como falsos)
)
