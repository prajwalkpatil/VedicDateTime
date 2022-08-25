# ---------------------------------------------------------------------------- #
#' to_dms
#'
#' @description Convert decimal degrees to degrees, minutes, and seconds
#'
#' @param deg Degrees as a decimal number
#'
#' @return A vector containing degrees, minutes and seconds
#'
#' @examples
#' to_dms(30.263888889)
to_dms <- function(deg){
  d = as.integer(deg)
  mins = (deg - d) * 60
  m = as.integer(mins)
  s = as.integer(round((mins - m) * 60))
  return (c(d, m, s))
}
# ---------------------------------------------------------------------------- #
