# ---------------------------------------------------------------------------- #
#' from_dms
#'
#' @description Convert degrees, minutes, and seconds to decimal degrees
#'
#' @param degs Degrees
#' @param mins Minutes
#' @param secs Seconds
#'
#' @return Degrees as a decimal number
#'
#' @examples
#' from_dms(30,15,50)
from_dms <- function(degs,mins,secs){
  return (degs + mins/60 + secs/3600)
}
# ---------------------------------------------------------------------------- #
