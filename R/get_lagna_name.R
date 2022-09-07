# ---------------------------------------------------------------------------- #
#' get_lagna_name
#' @description Get name of the Lagna for given Julian day number.
#'
#' @param jd Julian day number
#'
#' @return Name of the lagna.
#'
#' @examples
#' get_lagna_name(2459778)
#' get_lagna_name(gregorian_to_jd(30,8,2022))
get_lagna_name <- function(jd){
  return(rashis[lagna(jd)])
}
# ---------------------------------------------------------------------------- #
