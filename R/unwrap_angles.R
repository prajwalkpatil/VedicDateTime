# ---------------------------------------------------------------------------- #
#' unwrap_angles
#'
#' @description Add 360 degs an element in the input vector if elements are not sorted in ascending order.
#'
#' @param angles Vector containing angles
#'
#' @return angles in ascending order
#'
unwrap_angles <- function(angles){
  angles_unwraped = angles
  for(i in 2:length(angles)){
    if(angles_unwraped[i] < angles_unwraped[i-1]){
      angles_unwraped[i] <- angles_unwraped[i] + 360
    }
  }
  if(is.unsorted(angles_unwraped)){
    warning("unwrap_angles : angles are not sorted!")
  }
  return (angles_unwraped)
}
# ---------------------------------------------------------------------------- #
