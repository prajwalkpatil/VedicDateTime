# ---------------------------------------------------------------------------- #
#' new_moon
#'
#' @description Julian day representing the new moon day for a given Julian day number and tithi
#'
#' @param jd Julian day number
#' @param tithi_ Number associated with the tithi
#' @param opt Option to select next new moon day(\code{opt} = 1) or previous new moon day (\code{opt} = -1), Default \code{opt} = -1 .
#'
#' @return New moon day as a Julian day number
#'
#' @examples
#' new_moon(2459778,2)
#' new_moon(2459778,tithi(2459778,c(15.34, 75.13, +5.5)))
new_moon <- function(jd,tithi_,opt = -1){
  if(opt == -1){
    start = jd - tithi_
  }
  if(opt == +1){
    start = jd + (30 - tithi_)
  }
  x = c()
  y = c()
  for(i in 0:16){
    x <- append(x,(-2 + i/4))
  }
  for(j in 1:length(x)){
    y <- append(y,lunar_phase(start + x[j]))
  }
  y = unwrap_angles(y)
  y0 = inverse_lagrange(x,y,360)
  return (start + y0)
}
# ---------------------------------------------------------------------------- #
