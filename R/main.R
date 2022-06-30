install.packages("swephR")
library(swephR)
data(SE)
options(digits=22)

moon_longitude <- function(jd){
  return (swe_calc_ut(jd, SE$MOON, SE$FLG_SWIEPH)$xx[1])
}

sun_longitude <- function(jd){
  return (swe_calc_ut(jd, SE$SUN, SE$FLG_SWIEPH)$xx[1])
}

from_dms <- function(degs,mins,secs){
  return (degs + mins/60 + secs/3600)
}

to_dms <- function(deg){
  d = as.integer(deg)
  mins = (deg - d) * 60
  m = as.integer(mins)
  s = as.integer(round((mins - m) * 60))
  return (c(d, m, s))
}

unwrap_angles <- function(angles){
  angles_unwraped = angles
  for(i in 2:length(angles)){
    if(angles_unwraped[i] < angles_unwraped[i-1]){
      angles_unwraped[i] <- angles_unwraped[i] + 360
    }
  }
  if(is.unsorted(angles_unwraped)){
    print("unwrap_angles : angles are not sorted!")
  }
  return (angles_unwraped)
}


inverse_lagrange <- function(x,y,ya){
  if(length(x) != length(y)){
    print("Lengths are not equal!")
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

gregorian_to_jd <- function(day,month,year){
  return (swe_julday(year, month, day, 0.0,SE$GREG_CAL))
}

jd_to_gregorian <- function(jd){
  return (swe_revjul(jd, SE$GREG_CAL))
}

sunrise <- function(jd,place){
  lat = place[1]
  lon = place[2]
  tz = place[3]
  result <- swe_rise_trans_true_hor(jd - (tz/24.0)+0.5,SE$SUN,"",SE$FLG_SWIEPH,SE$BIT_DISC_CENTER + SE$CALC_RISE,c(lon,lat,0),0,0,0)
  print(jd - (tz/24.0))
  rise <- as.double(result$tret)
  return (c(rise + (tz)/24,to_dms((rise - jd) * 24 + tz)))
}

sunrise(sunrise(swe_julday(2022, 6, 30, 13.0,SE$GREG_CAL),c(17.329731,76.834297,+5.5)))
