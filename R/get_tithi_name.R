# ---------------------------------------------------------------------------- #
#' get_tithi_name
#' @description Get name(s) of the Tithi for given Julian day number and place.
#'
#' @param jd Julian day number
#' @param place Vector containing latitude, longitude and timezone
#'
#' @return Name(s) of the Tithi and its ending time.
#'
#' @examples
#' get_tithi_name(2459778,c(15.34, 75.13, +5.5))
#' get_tithi_name(swephR::swe_julday(2022,7,14,0,swephR::SE$GREG_CAL),c(15.34, 75.13, +5.5))
get_tithi_name <- function(jd,place){
  tithi_ = tithi(jd,place)
  size = length(tithi_)
  size = size / 4
  j <- 1
  tithi_name <- ""
  for(i in 1:size){
    tithi_name <- paste(tithi_name,tithis[tithi_[j]]," till",sep = "")
    tithi_name <- paste(tithi_name,paste(tithi_[j+1], tithi_[j+2], tithi_[j+3], sep = ":"))
    if(size > 1 && i == 1){
      tithi_name <- paste(tithi_name,"& ")
    }
    j <- 5
  }
  return (tithi_name)
}
# ---------------------------------------------------------------------------- #
