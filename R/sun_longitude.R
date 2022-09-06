# ---------------------------------------------------------------------------- #
#' sun_longitude
#'
#' @description Get Solar longitude for a given Julian day number.
#'
#' @param jd Julian day
#'
#' @return Solar longitude for \code{jd}
#'
#' @examples
#' sun_longitude(2459778)
#' sun_longitude(2459500)
sun_longitude <- function(jd){
  return (swephR::swe_calc_ut(jd, swephR::SE$SUN, swephR::SE$FLG_SWIEPH)$xx[1])
}
# ---------------------------------------------------------------------------- #
