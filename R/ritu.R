# ---------------------------------------------------------------------------- #
#' ritu
#'
#' @param masa_num Number associated with a Masa
#'
#' @return Number associated with the Ritu
#'
#' @examples
#' ritu(2)
ritu <- function(masa_num){
  return (((masa_num[1] - 1) %/% 2) + 1)
}
# ---------------------------------------------------------------------------- #
