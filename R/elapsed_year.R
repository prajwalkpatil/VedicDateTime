# ---------------------------------------------------------------------------- #
#' elapsed_year
#'
#' @param jd Julian Day number
#' @param maasa_num Number indicating the Maasa
#'
#' @return A vector containing Kali, Saka, and Vikram Samvat
#'
#' @examples
#' elapsed_year(2459778,2)
elapsed_year <- function(jd,maasa_num){
  sidereal_year = 365.25636
  ahar = ahargana(jd)
  kali = as.integer((ahar + (4 - maasa_num) * 30) / sidereal_year)
  saka = kali - 3179
  vikrama = saka + 135
  return (c(kali, saka, vikrama))
}
# ---------------------------------------------------------------------------- #