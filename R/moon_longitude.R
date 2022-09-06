# ---------------------------------------------------------------------------- #
#' moon_longitude
#'
#' @description Get Lunar longitude for a given Julian day number.
#'
#' @param jd Julian day
#'
#' @return Lunar longitude for \code{jd}
#'
#' @examples
#' moon_longitude(2459778)
#' moon_longitude(2459500)
moon_longitude <- function(jd){
  return (swephR::swe_calc_ut(jd, swephR::SE$MOON, swephR::SE$FLG_SWIEPH)$xx[1])
}
# ---------------------------------------------------------------------------- #
