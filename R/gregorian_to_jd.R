# ---------------------------------------------------------------------------- #
#' gregorian_to_jd
#'
#' @description Convert Gregorian date to Julian day number at 00:00 UTC
#'
#' @param day Day number
#' @param month Month number
#' @param year Year number
#'
#' @return Julian day number
#'
#' @examples
#' gregorian_to_jd(18,7,2022)
gregorian_to_jd <- function(day,month,year){
  return (swephR::swe_julday(year, month, day, 0.0,swephR::SE$GREG_CAL))
}
# ---------------------------------------------------------------------------- #
