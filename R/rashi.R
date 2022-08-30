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
  swe_set_sid_mode(SE$SIDM_LAHIRI,0,0)
  s = moon_longitude(jd)
  lunar_nirayana = (moon_longitude(jd) - swe_get_ayanamsa_ex_ut(jd,SE$FLG_SWIEPH + SE$FLG_NONUT)$daya) %% 360
  return (ceiling(lunar_nirayana / 30))
}
# ---------------------------------------------------------------------------- #
