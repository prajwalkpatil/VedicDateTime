# ---------------------------------------------------------------------------- #
#' lunar_phase
#'
#' @description Lunar phase for a given Julian day number
#'
#' @param jd Julian day number
#'
#' @return Lunar phase
#'
#' @examples
#' lunar_phase(2459778)
lunar_phase <- function(jd){
  sl = sun_longitude(jd)
  ll = moon_longitude(jd)
  moon_phase <- ((ll-sl) %% 360)
  return (moon_phase)
}
# ---------------------------------------------------------------------------- #

