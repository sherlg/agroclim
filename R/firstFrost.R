#' First frost day
#'
#' @description Calculates the first frost day (tmp < 0ºC) of each growing
#' season. Other thresholds can be used.
#' 
#' @param mn vector of daily minimum temperature.
#' @param dates vector of dates corresponding to the mn vector, in Date format.
#' @param iniday first day of the growing season, in text format ("mm-dd").
#' Defaults to `07-01`.
#' @param endday last day of the growing season, in text format ("mm-dd").
#' Defaults to `06-30`.
#' @param type character vector specifying the type of output desired. It can
#' be `doy` for day of the year (julian day), or `date` for character date 
#' format (`mm-dd`). Defaults to `doy`.
#' @param thres temperature threshold to trigger frost (defaults to 0ºC).
#' 
#' @details The function is able to span over consecutive years. If
#' `enddate` is an earlier day than `inidate`, then the considered period will
#' leap over two years.
#' 
#' @return Depending on `type`, the output will be a numeric vector of julian
#' days (of `type` = 'doy') or a vector of characters representing dates
#' (`type` = 'date').
#'
#' @examples
#'
#' firstFrost(mn = daily_tmin,
#'             dates = seq.Date(as.Date('1981-01-01'),
#'                              as.Date('2010-12-31'), by ='day'),
#'             iniday = '07-01',
#'             endday = '06-30',
#'             type = 'doy')
#'
#' @importFrom lubridate yday
#' @export


firstFrost <- function(mn, dates, iniday = '07-01', endday = '06-30', type = 'doy', thres = 0) {

  # determine growing seasons
  season <- data.frame(
    ini = which(as.character(dates, format = '%m-%d') == iniday),
    end = which(as.character(dates, format = '%m-%d') == endday)
  )
  if(season$end[1] < season$ini[1]) {
    season$end <- c(season$end[2:length(season$end)], length(mn))
  }

  # determine first frost of each growing season
  ff <- apply(season, 1, function(x) {
    t <- x['ini']:x['end']
    w <- match(TRUE, mn[t] < thres)
    return(dates[t][w])
  })
  ff <- as.Date(ff, origin='1969-12-31')
  
  # format and return
  if (type == 'date') {
    ff <- as.character(ff, format='%m-%d')
  }
  if (type == 'doy'){
    ff <- yday(ff)
  }
  return(ff)
}
