#' Chilling Hours Quantification Based on Crossa-Raynaud
#'
#' @description Chilling Hours (CH) based on Crossa-Raynaud formula. This method
#' is used to estimate the number of chilling hours accumulated during the winter
#' season, which is essential for many plants to break dormancy and resume growth.
#' 
#' @param mx vector of daily maximum temperature.
#' @param mn vector of daily minimum temperature.
#' @param dates vector of dates corresponding to the mx and mn vectors, in Date
#' format.
#' @param iniday first day of the chilling season, in text format ("mm-dd").
#' If "lati" is defined, this is automatically set (see details).
#' @param endday last day of the chilling season, in text format ("mm-dd").
#' If "lati" is defined, this is automatically set (see details).
#' @param lati (optional) numeric value indicating the latitude of location,
#' with positive values indicating northern hemisphere and negative southern.
#' @param tbase (optional) numeric value indicating the threshold temperature
#' for chilling hours calculation. Defaults to 7.2 ºC.
#' 
#' @return The sum of chilling hours accumulated during each chilling season, as a
#' numeric value.
#' 
#' @details The function computes, for each chilling season in the time series,
#' the amount of CH accumulated. CHs are computed as:
#' 
#' HF = 7.2 - (Tmax / (Tmax - Tmin)) * 24
#' 
#' Where:
#' 
#' - If Tmax <= 7.2, then CH = 24 hours (all day is considered chilling).
#' - If Tmin >= 7.2, then CH = 0 hours (no chilling accumulation).
#' - Otherwise, the formula computes chilling hours based on the temperature difference.
#' 
#' If `lati` is set, the function detects the hemisphere and considers the
#' chilling season to go from November 1st to the end of February (northern hemisphere),
#' or from 1st May to 31st August (southern hemisphere).
#' 
#' @references Crossa-Raynaud, P., 1955. Effets des hivers doux sur le comportement des arbres fruitiers à
#' feuilles caduques : Observations faites en Tunisie à la suite de l’hiver 1954-1955. Annale de
#' Service Botanique et Agronomique, Tunisie, 28: 1–22.
#' 
#' @examples
#'
#' @export

fdd <- function(mx, mn, dates, iniday = NULL, endday = NULL, lati = NULL, tbase = 7.2) {
  
  if ((is.null(iniday) | is.null(endday)) & is.null(lati)) {
    stop('Either `lati` or the pair `iniday` and `endday` must be provided.')
  }
  
  if (sum(is.na(mx) | sum(is.na(mn)) > 0)) {
    stop("`mx` and `mn` must not contain NA values.")
  }
  
  if(!is.null(lati)) {
    if(!is.numeric(lati)){
      stop("`lati` must be a numeric value indicating latitude.")
    }
    
    # definition of growing dates depending on hemisphere
    if(lati >= 0){  # northern: nov to feb
      iniday <- '11-01'
      endday <- '02-28'
    } else {  # southern: may to ag
      iniday <- '05-01'
      endday <- '08-31'
    }
  }
  
  # determine chilling seasons
  season <- data.frame(
    ini = which(format(dates, '%m-%d') == iniday),
    end = which(format(dates, '%m-%d') == endday)
  )
  
  if(season$end[1] < season$ini[1]) {
    season$end <- c(season$end[2:length(season$end)], length(mn))
    warning('Last chilling season did not reach the end day.')
  }
  
  # determine chilling hours quantification of each chilling season using Crossa-Raynaud formula
  ch <- apply(season, 1, function(x) {
    t <- x['ini']:x['end']

    tmp <- sapply(1:length(t), function(i) {
      tmax <- mx[t[i]]
      tmin <- mn[t[i]]

      if (tmax <= tbase) {
        return(24)
      } else if (tmin >= tbase) {
        return(0)
      } else {
        chours <- ((7.2 - tmin) / (tmax - tmin)) * 24
        return(chours)
      }
    })

    return(sum(tmp, na.rm = TRUE))
  })
  
  return(ch)
}
