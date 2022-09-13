# ---------------------------------------------------------------------------- #
#' inverse_lagrange
#'
#' @description Given two vectors x and y, find the value of x = xa when y = ya, i.e., f(xa) = ya
#'
#' @param x Vector x
#' @param y Vector y
#' @param ya Double ya
#'
#' @return Value of xa
#'
inverse_lagrange <- function(x,y,ya){
  if(length(x) != length(y)){
    warning("Lengths are not equal!")
  }
  total <- 0
  for(i in 1:length(x)){
    numerator = 1
    denominator = 1
    for(j in 1:length(x)){
      if(j != i){
        numerator = numerator * (ya - y[j])
        denominator = denominator * (y[i] - y[j])
      }
    }
    total <- total + ((numerator * x[i]) / denominator)
  }
  return (total)
}
# ---------------------------------------------------------------------------- #
