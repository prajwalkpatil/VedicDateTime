# ---------------------------------------------------------------------------- #
#' get_karana_name
#' @description Get name(s) of the Karana for given Julian day number and place.
#'
#' @param jd Julian day number
#' @param place Vector containing latitude, longitude and timezone
#'
#' @return Name(s) of the Karana.
#'
#' @examples
#' get_karana_name(2459778,c(15.34, 75.13, +5.5))
#' get_karana_name(swephR::swe_julday(2022,7,14,0,swephR::SE$GREG_CAL),c(15.34, 75.13, +5.5))
get_karana_name <- function(jd,place){
  karana_ = karana(jd,place)
  karana_name = paste(karanas[karana_[1]],karanas[karana_[2]],sep = "-")
  return(karana_name)
}
# ---------------------------------------------------------------------------- #
