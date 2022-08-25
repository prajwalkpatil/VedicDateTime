# ---------------------------------------------------------------------------- #
#' vaara
#'
#' @description Vaara for a given Julian day number
#' @param jd Julian day number
#'
#' @return Vaara as an integer
#'
#' @examples
#' vaara(2459778)
vaara <- function(jd){
  return (as.integer(ceiling(jd + 1) %% 7) + 1)
}
# ---------------------------------------------------------------------------- #