# ---------------------------------------------------------------------------- #
#' get_vaara_name
#' @description Get name of the Vaara for given Julian day number.
#'
#' @param jd Julian day number
#'
#' @return Name of the Vaara.
#'
#' @examples
#' get_vaara_name(2459778)
#' get_vaara_name(swephR::swe_julday(2022,7,14,0,swephR::SE$GREG_CAL))
get_vaara_name <- function(jd){
  return(vaaras[vaara(jd)])
}
# ---------------------------------------------------------------------------- #
