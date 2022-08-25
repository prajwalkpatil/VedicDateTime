# ---------------------------------------------------------------------------- #
#' jd_to_gregorian
#'
#' @description Convert Julian day number to Gregorian date
#'
#' @param jd Julian day number
#'
#' @return Gregorian date
#'
#' @examples
#' jd_to_gregorian(2459778)
jd_to_gregorian <- function(jd){
  return (swe_revjul(jd, SE$GREG_CAL))
}
# ---------------------------------------------------------------------------- #
