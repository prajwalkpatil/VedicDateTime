# ---------------------------------------------------------------------------- #
#' sunrise
#'
#' @description Sunrise for a given date and place
#'
#' @param jd Julian day number
#' @param place Vector containing latitude, longitude and timezone
#'
#' @return Sunrise as Julian day number
#'
#' @examples
#' sunrise(2459778,c(15.34, 75.13, +5.5))
sunrise <- function(jd,place){
  lat = place[1]
  lon = place[2]
  tz = place[3]
  result <- swephR::swe_rise_trans_true_hor(jd - (tz/24.0),swephR::SE$SUN,"",swephR::SE$FLG_SWIEPH,swephR::SE$BIT_DISC_CENTER + swephR::SE$CALC_RISE,c(lon,lat,0),0,0,0)
  rise <- result$tret
  return (c(rise + (tz)/24.0,to_dms((rise - jd) * 24 + tz)))
}
# ---------------------------------------------------------------------------- #
