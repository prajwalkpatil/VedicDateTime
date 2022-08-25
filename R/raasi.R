# ---------------------------------------------------------------------------- #
#' raasi
#'
#' @description Raasi(Sun-sign) for a given Julian day number
#'
#' @param jd Julian day number
#'
#' @return Raasi as an integer
#'
#' @examples
#' raasi(2459778)
#' raasi(swe_julday(2022,7,14,0,SE$GREG_CAL))
raasi <- function(jd){
  swe_set_sid_mode(SE$SIDM_LAHIRI,0,0)
  s = sun_longitude(jd)
  solar_nirayana = (sun_longitude(jd) - swe_get_ayanamsa_ex_ut(jd,SE$FLG_SWIEPH + SE$FLG_NONUT)$daya) %% 360
  return (ceiling(solar_nirayana / 30))
}
# ---------------------------------------------------------------------------- #