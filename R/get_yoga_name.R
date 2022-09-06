# ---------------------------------------------------------------------------- #
#' get_yoga_name
#' @description Get name(s) of the Yoga for given Julian day number and place.
#'
#' @param jd Julian day number
#' @param place Vector containing latitude, longitude and timezone
#'
#' @return Name(s) of the Yoga and its ending time.
#'
#' @examples
#' get_yoga_name(2459778,c(15.34, 75.13, +5.5))
#' get_yoga_name(swephR::swe_julday(2022,7,14,0,swephR::SE$GREG_CAL),c(15.34, 75.13, +5.5))
get_yoga_name <- function(jd,place){
  yoga_ = yoga(jd,place)
  size = length(yoga_)
  size = size / 4
  j <- 1
  yoga_name <- ""
  for(i in 1:size){
    yoga_name <- paste(yoga_name,yogas[yoga_[j]]," till",sep = "")
    yoga_name <- paste(yoga_name,paste(yoga_[j+1], yoga_[j+2], yoga_[j+3], sep = ":"))
    if(size > 1 && i == 1){
      yoga_name <- paste(yoga_name,"& ")
    }
    j <- 5
  }
  return (yoga_name)
}
