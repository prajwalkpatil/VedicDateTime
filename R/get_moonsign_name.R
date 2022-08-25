# ---------------------------------------------------------------------------- #
#' get_moonsign_name
#' @description Get name of the moonsign for given Julian day number.
#'
#' @param jd Julian day number
#'
#' @return Name of the Moonsign.
#'
#' @examples
#' get_moonsign_name(2459778)
#' get_moonsign_name(swe_julday(2022,7,14,0,SE$GREG_CAL))
get_moonsign_name <- function(jd){
  return(raasis[moonsign(jd)])
}
# ---------------------------------------------------------------------------- #