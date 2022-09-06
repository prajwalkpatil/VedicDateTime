# ---------------------------------------------------------------------------- #
#' get_nakshatra_name
#' @description Get name(s) of the Nakshatra for given Julian day number and place.
#'
#' @param jd Julian day number
#' @param place Vector containing latitude, longitude and timezone
#'
#' @return Name(s) of the Nakshatra and its ending time.
#'
#' @examples
#' get_nakshatra_name(2459778,c(15.34, 75.13, +5.5))
#' get_nakshatra_name(swephR::swe_julday(2022,7,14,0,swephR::SE$GREG_CAL),c(15.34, 75.13, +5.5))
get_nakshatra_name <- function(jd,place){
  nakshatra_ = nakshatra(jd,place)
  size = length(nakshatra_)
  size = size / 4
  j <- 1
  nakshatra_name <- ""
  for(i in 1:size){
    nakshatra_name <- paste(nakshatra_name,nakshatras[nakshatra_[j]]," till",sep = "")
    nakshatra_name <- paste(nakshatra_name,paste(nakshatra_[j+1], nakshatra_[j+2], nakshatra_[j+3], sep = ":"))
    if(size > 1 && i == 1){
      nakshatra_name <- paste(nakshatra_name,"& ")
    }
    j <- 5
  }
  return(nakshatra_name)
}
# ---------------------------------------------------------------------------- #
