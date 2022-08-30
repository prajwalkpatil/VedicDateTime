# ---------------------------------------------------------------------------- #
#' get_rashi_name
#'
#' @description Get name of the Rashi for given Julian day number.
#'
#' @param jd Julian day number
#'
#' @return Name of the Rashi.
#'
#' @examples
#' get_rashi_name(2459778)
#' get_rashi_name(gregorian_to_jd(30,8,2022))
get_rashi_name <- function(jd){
  return(rashis[rashi(jd)])
}
# ---------------------------------------------------------------------------- #
