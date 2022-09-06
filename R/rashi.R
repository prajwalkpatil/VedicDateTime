# ---------------------------------------------------------------------------- #
#' Rashi
#'
#' @description Rashi for a given Julian day number
#'
#' @param jd Julian day number
#'
#' @return Rashi as an integer
#'
#' @examples
#' rashi(2459778)
#' rashi(gregorian_to_jd(30,8,2022))
rashi <- function(jd){
  swephR::swe_set_sid_mode(swephR::SE$SIDM_LAHIRI,0,0)
  s = moon_longitude(jd)
  lunar_nirayana = (moon_longitude(jd) - swephR::swe_get_ayanamsa_ex_ut(jd,swephR::SE$FLG_SWIEPH + swephR::SE$FLG_NONUT)$daya) %% 360
  return (ceiling(lunar_nirayana / 30))
}
# ---------------------------------------------------------------------------- #
