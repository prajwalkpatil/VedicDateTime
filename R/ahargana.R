# ---------------------------------------------------------------------------- #
#' ahargana
#'
#' @param jd Julian day number
#'
#' @return Ahargana
#'
#' @examples
#' ahargana(2459778)
#' ahargana(swe_julday(2022,7,14,0,SE$GREG_CAL))
ahargana <- function(jd){
  return (jd - 588465.5)
}
# ---------------------------------------------------------------------------- #