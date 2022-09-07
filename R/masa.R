# ---------------------------------------------------------------------------- #
#' masa
#'
#' @description Masa for a given place and time
#'
#' @param jd Julian day number
#' @param place Vector containing latitude, longitude and timezone
#'
#' @return Masa number and whether it is adhika masa or not
#'
#' @examples
#' masa(2459778,c(15.34, 75.13, +5.5))
#' masa(swephR::swe_julday(2022,7,14,0,swephR::SE$GREG_CAL),c(15.34, 75.13, +5.5))
masa <- function(jd,place){
  #Masa as -> 1 = Chaitra, 2 = Vaisakha, ..., 12 = Phalguna
  ti = tithi(jd,place)[1]
  critical = sunrise(jd,place)[1]
  last_new_moon = new_moon(critical,ti,-1)
  next_new_moon = new_moon(critical,ti,+1)
  this_solar_month = lagna(last_new_moon)
  next_solar_month = lagna(next_new_moon)
  is_leap_month = (this_solar_month == next_solar_month)
  maasa = this_solar_month + 1
  if(maasa > 12){
    maasa = maasa %% 12
  }
  return (c(as.integer(maasa),is_leap_month))
}
# ---------------------------------------------------------------------------- #
