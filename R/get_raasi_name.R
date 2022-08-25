# ---------------------------------------------------------------------------- #
#' get_raasi_name
#'
#' @description Get name of the Raasi for given Julian day number.
#'
#' @param jd Julian day number
#'
#' @return Name of the Raasi.
#'
#' @examples
#' get_raasi_name(2459778)
#' get_raasi_name(swe_julday(2022,7,14,0,SE$GREG_CAL))
get_raasi_name <- function(jd){
  return(raasis[raasi(jd)])
}
# ---------------------------------------------------------------------------- #