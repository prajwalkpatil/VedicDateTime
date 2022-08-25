# ---------------------------------------------------------------------------- #
#' moonsign
#'
#' @description Moon-sign for a given Julian day number
#'
#' @param jd Julian day number
#'
#' @return Moonsign as an integer
#'
#' @examples
#' moonsign(2459778)
#' moonsign(swe_julday(2022,7,14,0,SE$GREG_CAL))
moonsign <- function(jd){
  swe_set_sid_mode(SE$SIDM_LAHIRI,0,0)
  s = moon_longitude(jd)
  lunar_nirayana = (moon_longitude(jd) - swe_get_ayanamsa_ex_ut(jd,SE$FLG_SWIEPH + SE$FLG_NONUT)$daya) %% 360
  return (ceiling(lunar_nirayana / 30))
}
# ---------------------------------------------------------------------------- #