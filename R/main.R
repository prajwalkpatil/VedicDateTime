# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

install.packages("swephR")
library(swephR)
data(SE)


moon_longitude <- function(jd){
  return (swe_calc_ut(jd, SE$MOON, SE$FLG_SWIEPH)$xx[1]);
}

sun_longitude <- function(jd){
  return (swe_calc_ut(jd, SE$SUN, SE$FLG_SWIEPH)$xx[1]);
}

from_dms <- function(degs,mins,secs){
  return (degs + mins/60 + secs/3600);
}

to_dms <- function(deg){
  d = as.integer(deg)
  mins = (deg - d) * 60
  m = as.integer(mins)
  s = as.integer(round((mins - m) * 60))
  return (c(d, m, s));
}


