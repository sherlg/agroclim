#' Growing Degree Days
#'
#' @description Growing Degree-Day (GDD). GDDs are a measure of heat
#' accumulation used by agronomists and farmers to predict plant (and animal)
#' development rates. Critical development times such as emerging, blooming,
#' or maturity, have been succesfully related to GDD for a variety of crops
#' and animal species.
#' 
#' @param mx vector of daily maximum temperature.
#' @param mn vector of daily minimum temperature.
#' @param dates vector of dates corresponding to the mx and mn vectors.
#' @param iniday first day of the growing season, in text format ("mm-dd").
#' If "lati" is defined, this is automatically set (see details).
#' @param endday last day of the growing season, in text format ("mm-dd").
#' If "lati" is defined, this is automatically set (see details).
#' @param lati (optional) numeric value indicating the latitude of location,
#' with positive values indicating northern hemisphere and negative southern.
#' @param tbase (optional) numeric value indicating the lower temperature
#' threshold for the crop to grow. Defaults to 10 ºC.
#' @param tceil (optional) numeric value indicating the upper temperature
#' threshold for the crop to grow. Defaults to Inf, in which case there is no
#' upper threshold.
#' @param variant (optional) character value indicating the computation
#' variant to use, one of ('A', 'B'). Defaults to 'A'.
#' 
#' @return The sum of growing degree-days during each growing season, as a
#' numeric value.
#' 
#' @details The function computes, for each growing season in the time series,
#' the amount of GDD accumulated. GDDs are computed as:
#' 
#' GDD = (Tmx - Tmn) / 2 - tbase,  for (Tmx - Tmn) / 2 > tbase, and zero
#' otherwise.
#' 
#' This is the default option when `variant` is set to A. If it set to B, the
#' following modification is applied:
#' 
#' Tmn = tbase,  for Tmn < tbase.
#' 
#' Also, if `tceil` is provided then there is no GDD accumulation above that
#' threshold, so:
#' 
#' Tmx = tceil,  for Tmx > tbase.
#' 
#' If `lati` is set, the function detects the hemisphere and considers the
#' growing season to go from April 1st to October 31st (northern hemisphere),
#' or from 1st October to 30rd April (southern hemisphere). This corresponds
#' to the normal cicle of a grape crop, and is maintained as a legacy.
#' 
#' @references Winkler AJ, Cook JA, Kliwer WM, Lider LA (1974) General
#' viticulture. University of California Press, Berkeley, CA.
#' 
#' @examples
#'
#'  dates <- seq.Date(as.Date('1981-01-01'), as.Date('2010-12-31'), by ='day')
#'  gdd(mx = daily_tmax,
#'       mn = daily_tmin,
#'       dates = dates,
#'       lati = 42)
#'  gdd(mx = daily_tmax,
#'       mn = daily_tmin,
#'       dates = dates,
#'       iniday = '04-01',
#'       endday = '10-31',
#'       tceil = 35)
#'
#' @importFrom lubridate yday
#' @export

gdd <- function(mx, mn, dates, iniday = NULL, endday = NULL, lati = NULL,
                tbase = 10, tceil = Inf, variant = 'A') {

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
    if(lati >= 0){ # northern: apr to sept
      iniday <- '04-01'
      endday <- '10-31'} else{ # southern: oct to mar
        iniday <- '10-01'
        endday <- '04-30'
      }
  }
  
  # transform to Julian days since start of year
  iniday <- yday(paste0('1970-', iniday))
  endday <- yday(paste0('1970-', endday))
  julians <- yday(dates)

  # variants
  mx[mx > tceil] <- tceil
  if (variant == 'B') {
    mn[!is.na(mn) & mn < tbase] <- tbase
  }

  # nullify out of season values
  mx[julians < iniday] <- mn[julians < iniday] <- tbase
  mx[julians > endday] <- mn[julians > endday] <- tbase
  
  # compute average temperature
  tmp <- (mx + mn) / 2
  
  # compute average temp. above base temperature
  tmp <- sapply(tmp, function(x) max(0, x - tbase))

  # aggregate for each growing season - FOR NOW, ONLY BY YEARS
  gdd <- aggregate(tmp, by=list(year(dates)), FUN=sum)

  return(gdd$x)
}
