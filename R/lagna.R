# ---------------------------------------------------------------------------- #
#' Lagna
#'
#' @description Lagna for a given Julian day number
#'
#' @param jd Julian day number
#'
#' @return Lagna as an integer
#'
#' @examples
#' lagna(2459778)
#' lagna(gregorian_to_jd(30,8,2022))
lagna <- function(jd){
  swephR::swe_set_sid_mode(swephR::SE$SIDM_LAHIRI,0,0)
  s = sun_longitude(jd)
  solar_nirayana = (sun_longitude(jd) - swephR::swe_get_ayanamsa_ex_ut(jd,swephR::SE$FLG_SWIEPH + swephR::SE$FLG_NONUT)$daya) %% 360
  return (ceiling(solar_nirayana / 30))
}
# ---------------------------------------------------------------------------- #
