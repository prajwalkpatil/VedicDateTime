# ---------------------------------------------------------------------------- #
#' sunset
#' @description Sunset for a given date and place
#'
#' @param jd Julian day number
#' @param place Vector containing latitude, longitude and timezone
#'
#' @return Sunset as Julian day number
#'
#' @examples
#' sunset(2459778,c(15.34, 75.13, +5.5))
sunset <- function(jd,place){
  lat = place[1]
  lon = place[2]
  tz = place[3]
  result <- swephR::swe_rise_trans_true_hor(jd - (tz/24.0),swephR::SE$SUN,"",swephR::SE$FLG_SWIEPH,swephR::SE$BIT_DISC_CENTER + swephR::SE$CALC_SET,c(lon,lat,0),0,0,0)
  setting <- result$tret #Julian day number
  #Convert to the given timezone
  return (c(setting + (tz)/24,to_dms((setting - jd) * 24 + tz)))
}
# ---------------------------------------------------------------------------- #
