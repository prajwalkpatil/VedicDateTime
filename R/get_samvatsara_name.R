# ---------------------------------------------------------------------------- #
#' get_samvatsara_name
#'
#' @description Name of the Shaka Samvatsar for a given Julian day number and maasa number.
#' @param jd Julian day number
#' @param maasa_num Maasa number
#'
#' @return Shaka Samvatsar
#'
#' @examples
#' get_samvatsara_name(2459778,2)
get_samvatsara_name <- function(jd,maasa_num){
  return (samvatsars[samvatsara(jd,maasa_num)])
}
# ---------------------------------------------------------------------------- #