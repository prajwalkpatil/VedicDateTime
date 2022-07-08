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
  result <- swe_rise_trans_true_hor(jd - (tz/24.0),SE$SUN,"",SE$FLG_SWIEPH,SE$BIT_DISC_CENTER + SE$CALC_RISE,c(lon,lat,0),0,0,0)
  rise <- result$tret
  return (c(rise + (tz)/24,to_dms((rise - jd) * 24 + tz)))
}

sunset <- function(jd,place){
  lat = place[1]
  lon = place[2]
  tz = place[3]
  result <- swe_rise_trans_true_hor(jd - (tz/24.0),SE$SUN,"",SE$FLG_SWIEPH,SE$BIT_DISC_CENTER + SE$CALC_SET,c(lon,lat,0),0,0,0)
  setting <- result$tret
  return (c(setting + (tz)/24,to_dms((setting - jd) * 24 + tz)))
}

moonrise <- function(jd,place){
  lat = place[1]
  lon = place[2]
  tz = place[3]
  result <- swe_rise_trans_true_hor(jd - (tz/24.0),SE$MOON,"",SE$FLG_SWIEPH,SE$BIT_DISC_CENTER + SE$CALC_RISE,c(lon,lat,0),0,0,0)
  rise <- result$tret
  return (to_dms((rise - jd) * 24 + tz))
}

moonset <- function(jd,place){
  lat = place[1]
  lon = place[2]
  tz = place[3]
  result <- swe_rise_trans_true_hor(jd - (tz/24.0),SE$MOON,"",SE$FLG_SWIEPH,SE$BIT_DISC_CENTER + SE$CALC_SET,c(lon,lat,0),0,0,0)
  setting <- result$tret
  return (to_dms((setting - jd) * 24 + tz))
}

lunar_phase <- function(jd){
  sl = sun_longitude(jd)
  ll = moon_longitude(jd)
  moon_phase <- ((ll-sl) %% 360)
  return (moon_phase)
}

tithi<-function(jd,place){
  tz = place[3]
  rise = sunrise(jd,place)[1] - (tz/24)
  moon_phase = lunar_phase(rise)
  today = ceiling(moon_phase/12)
  degrees_left = today * 12 - moon_phase


  offsets = c(0.25,0.5,0.75,1.0)
  lunar_logitude_diff = c()
  solar_logitude_diff = c()
  relative_motion = c()
  for(i in 1:length(offsets)){
    lunar_logitude_diff <- append(lunar_logitude_diff,((moon_longitude(rise + offsets[i]) - moon_longitude(rise)) %% 360))
    solar_logitude_diff <- append(solar_logitude_diff,((sun_longitude(rise + offsets[i]) - sun_longitude(rise)) %% 360))
    relative_motion <- append(relative_motion,(lunar_logitude_diff[i]- solar_logitude_diff[i]))
  }

  y = relative_motion
  x = offsets

  approx_end = inverse_lagrange(x,y,degrees_left)
  ends = (rise + approx_end - jd) * 24 + tz
  answer = c(as.integer(today),to_dms(ends))

  moon_phase_tom = lunar_phase(rise + 1)
  tomorrow =ceiling(moon_phase_tom/12)
  if(((tomorrow-today) %% 30) > 1){
    leap_tithi = today + 1
    degrees_left = leap_tithi * 12 -moon_phase
    approx_end = inverse_lagrange(x,y,degrees_left)
    ends = (rise + approx_end - jd) * 24 + place[3]
    answer <- append(answer,c(as.integer(leap_tithi),to_dms(ends)))
  }
  return (answer)
}


tithi(swe_julday(2022,6,17,0,SE$GREG_CAL),c(15.34, 75.13, +5.5))

nakshatra <- function(jd,place){
  swe_set_sid_mode(SE$SIDM_LAHIRI,0,0)
  lat = place[1]
  lon = place[2]
  tz = place[3]
  rise = sunrise(jd,place)[1]-(tz/24)

  offsets = c(0.0,0.25,0.5,0.75,1.0)
  longitudes = c()
  for(i in 1:length(offsets)){
    longitudes <- append(longitudes,((moon_longitude(rise + offsets[i]) - swe_get_ayanamsa_ex_ut(rise,SE$FLG_SWIEPH + SE$FLG_NONUT)$daya) %% 360))
  }
  nak = ceiling(longitudes[1] * 27 / 360)
  y = unwrap_angles(longitudes)
  x = offsets
  approx_end = inverse_lagrange(x,y,nak * 360/27)
  ends = (rise - jd + approx_end) * 24 + tz
  answer = c(as.integer(nak),to_dms(ends))

  nak_tmrw = ceiling(longitudes[length(longitudes)-1] * 27 / 360)
  if(((nak_tmrw - nak) %% 27) > 1){
    leap_nak = nak + 1
    approx_end = inverse_lagrange(offsets,longitudes,leap_nak*360/27)
    ends = (rise - jd + approx_end) * 24 + tz
    answer <- append(answer,c(as.integer(leap_nak),to_dms(ends)))
  }
  return (answer)
}

nakshatra(swe_julday(2022,6,30,0,SE$GREG_CAL),c(15.34, 75.13, +5.5))

yoga <- function(jd,place){
  swe_set_sid_mode(SE$SIDM_LAHIRI,0,0)
  lat = place[1]
  lon = place[2]
  tz = place[3]
  rise = sunrise(jd,place)[1]-(tz/24)
  lunar_long = (moon_longitude(rise) - swe_get_ayanamsa_ex_ut(rise,SE$FLG_SWIEPH + SE$FLG_NONUT)$daya) %% 360
  solar_long = (sun_longitude(rise) - swe_get_ayanamsa_ex_ut(rise,SE$FLG_SWIEPH + SE$FLG_NONUT)$daya) %% 360
  total = (lunar_long + solar_long) %% 360
  yog = ceiling(total * 27 / 360)
  degrees_left = yog * (360 / 27) - total

  offsets = c(0.25,0.5,0.75,1.0)
  lunar_logitude_diff = c()
  solar_logitude_diff = c()
  total_motion = c()

  for(i in 1:length(offsets)){
    lunar_logitude_diff <- append(lunar_logitude_diff,((moon_longitude(rise + offsets[i]) - moon_longitude(rise)) %% 360))
    solar_logitude_diff <- append(solar_logitude_diff,((sun_longitude(rise + offsets[i]) - sun_longitude(rise)) %% 360))
    total_motion <- append(total_motion,(lunar_logitude_diff[i] + solar_logitude_diff[i]))
  }

  y = total_motion
  x = offsets

  approx_end = inverse_lagrange(x, y, degrees_left)
  ends = (rise + approx_end - jd) * 24 + tz
  answer = c(as.integer(yog),to_dms(ends))

  lunar_long_tmrw = (moon_longitude(rise + 1) - swe_get_ayanamsa_ex_ut(rise + 1,SE$FLG_SWIEPH + SE$FLG_NONUT)$daya) %% 360
  solar_long_tmrw = (sun_longitude(rise + 1) - swe_get_ayanamsa_ex_ut(rise + 1,SE$FLG_SWIEPH + SE$FLG_NONUT)$daya) %% 360
  total_tmrw = (lunar_long_tmrw + solar_long_tmrw) %% 360
  tomorrow = ceiling(total_tmrw * 27 / 360)
  if(((tomorrow - yog) %% 27) > 1){
    leap_yog = yog + 1
    degrees_left = leap_yog * (360 / 27) - total
    approx_end = inverse_lagrange(x, y, degrees_left)
    ends = (rise + approx_end - jd) * 24 + tz
    answer <- append(answer,c(as.integer(leap_yog),to_dms(ends)))
  }
  return (answer)
}


yoga(swe_julday(2022,6,17,0,SE$GREG_CAL),c(15.34, 75.13, +5.5))


karana <- function(jd,place){
  rise = sunrise(jd,place)[1]

  solar_long = sun_longitude(rise)
  lunar_long = moon_longitude(rise)
  moon_phase = (lunar_long - solar_long) %% 360
  today = ceiling(moon_phase / 6)
  degrees_left = today * 6 - moon_phase

  return (c(as.integer(today)))
}

karana(swe_julday(2022,6,17,0,SE$GREG_CAL),c(15.34, 75.13, +5.5))

vaara <- function(jd){
  return (as.integer(ceiling(jd + 1) %% 7))
}

raasi <- function(jd){
  swe_set_sid_mode(SE$SIDM_LAHIRI,0,0)
  s = sun_longitude(jd)
  solar_nirayana = (sun_longitude(jd) - swe_get_ayanamsa_ex_ut(jd,SE$FLG_SWIEPH + SE$FLG_NONUT)$daya) %% 360
  return (ceiling(solar_nirayana / 30))
}

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

masa <- function(jd,place){
  ti = tithi(jd,place)[1]
  critical = sunrise(jd,place)[1]
  last_new_moon = new_moon(critical,ti,-1)
  next_new_moon = new_moon(critical,ti,+1)
  this_solar_month = raasi(last_new_moon)
  next_solar_month = raasi(next_new_moon)
  is_leap_month = (this_solar_month == next_solar_month)
  maasa = this_solar_month + 1
  if(maasa > 12){
    maasa = maasa %% 12
  }
  return (c(as.integer(maasa),is_leap_month))
}

masa(swe_julday(2022,6,17,0,SE$GREG_CAL),c(15.34, 75.13, +5.5))

ahargana <- function(jd){
  return (jd - 588465.5)
}

elapsed_year <- function(jd,maasa_num){
  sidereal_year = 365.25636
  ahar = ahargana(jd)
  kali = as.integer((ahar + (4 - maasa_num) * 30) / sidereal_year)
  saka = kali - 3179
  vikrama = saka + 135
  return (c(kali, saka, vikrama))
}

samvatsara <- function(jd,maasa_num){
  kali = elapsed_year(jd,maasa_num)[1]
  if(kali >= 4009){
    kali = (kali - 14) %% 60
  }
  samvat = (kali + 27 + as.integer((kali * 211 - 108)/18000)) %% 60
  return (samvat)
}

ritu <- function(masa_num){
  return ((masa_num - 1) %/% 2)
}

day_duration <- function(jd,place){
  srise = sunrise(jd,place)[1]
  sset = sunrise(jd,place)[1]
  diff = (sset - srise) * 24
  return (c(diff,to_dms(diff)))
}

get_masa_name <- function(jd,place){
  masa_ = masa(jd,place)
  masa_name = ""
  if(masa_[2] == 1){
    masa_name = "Adhika "
  }
  masa_name <- paste(masa_name,masas[masa_[1]],sep = "")
  return (masa_name)
}

get_masa_name(swe_julday(2022,7,29,0,SE$GREG_CAL),c(15.34, 75.13, +5.5))

get_tithi_name <- function(jd,place){
  tithi_ = tithi(jd,place)
  size = length(tithi_)
  size = size / 4
  j <- 1
  for(i in 1:size){
    cat(tithis[tithi_[j]])
    cat(" till ")
    cat(tithi_[j+1], tithi_[j+2], tithi_[j+3], sep = ":")
    if(size > 1 && i == 1){
      cat(" & ")
    }
    j <- 5
  }
}

get_tithi_name(swe_julday(2022,7,8,0,SE$GREG_CAL),c(15.34, 75.13, +5.5))

get_nakshatra_name <- function(jd,place){
  nakshatra_ = nakshatra(jd,place)
  size = length(nakshatra_)
  size = size / 4
  j <- 1
  for(i in 1:size){
    cat(nakshatras[nakshatra_[j]])
    cat(" till ")
    cat(nakshatra_[j+1], nakshatra_[j+2], nakshatra_[j+3], sep = ":")
    if(size > 1 && i == 1){
      cat(" & ")
    }
    j <- 5
  }
}

get_nakshatra_name(swe_julday(2022,7,18,0,SE$GREG_CAL),c(15.34, 75.13, +5.5))
