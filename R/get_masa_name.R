# ---------------------------------------------------------------------------- #
#' get_masa_name
#' @description Get name of the Masa for given Julian day number and place.
#'
#' @param jd Julian day number
#' @param place Vector containing latitude, longitude and timezone
#'
#' @return Name of the Masa
#'
#' @examples
#' get_masa_name(2459778,c(15.34, 75.13, +5.5))
#' get_masa_name(swephR::swe_julday(2022,7,14,0,swephR::SE$GREG_CAL),c(15.34, 75.13, +5.5))
get_masa_name <- function(jd,place){
  masa_ = masa(jd,place)
  masa_name = ""
  if(masa_[2] == 1){
    masa_name = "Adhika "
  }
  masa_name <- paste(masa_name,masas[masa_[1]],sep = "")
  return (masa_name)
}
# ---------------------------------------------------------------------------- #
