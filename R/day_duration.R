# ---------------------------------------------------------------------------- #
#' day_duration
#'
#' @description Duration of the day for a given place and time
#'
#' @param jd Julian day number
#' @param place Vector containing latitude, longitude and timezone
#'
#' @return Vector containing the length of the day & in dms
#'
#' @examples
#' day_duration(2459778,c(15.34, 75.13, +5.5))
#' day_duration(swephR::swe_julday(2022,7,14,0,swephR::SE$GREG_CAL),c(15.34, 75.13, +5.5))
day_duration <- function(jd,place){
  srise = sunrise(jd,place)[1]
  sset = sunrise(jd,place)[1]
  diff = (sset - srise) * 24
  return (c(diff,to_dms(diff)))
}
# ---------------------------------------------------------------------------- #
