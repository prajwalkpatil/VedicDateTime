# ---------------------------------------------------------------------------- #
#' karana
#'
#' @description Karana for a given place and time
#'
#' @param jd Julian day number
#' @param place Vector containing latitude, longitude and timezone
#'
#' @return Two karanas
#'
#' @examples
#' karana(2459778,c(15.34, 75.13, +5.5))
#' karana(gregorian_to_jd(17,6,2022),c(15.34, 75.13, +5.5))
karana <- function(jd,place){
  tithi_ = tithi(jd,place)
  answer <- c((tithi_[1] * 2) - 1,tithi_[1] * 2)
  return(answer)
}
# ---------------------------------------------------------------------------- #