## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  tidy.opts = list(width.cutoff = 60), tidy = TRUE,
  cache.rebuild = TRUE
)

## ---- include = FALSE---------------------------------------------------------
library(swephR)
data(SE)
options(digits = 5)

## ----setup--------------------------------------------------------------------
library(VedicDateTime)

## ----Tithi, width="50"--------------------------------------------------------
tithi<-function(jd,place){
  # Tithi as -> 1 = Shukla paksha prathama, 2 = Shukla paksha dvitiya,...

  tz = place[3] #Timezone of the place

  #1. Find time of sunrise at a given place
  rise = sunrise(jd,place)[1] - (tz/24)

  #2. Find tithi on this Julian day number at a given place
  moon_phase = lunar_phase(rise)
  today = ceiling(moon_phase/12)
  degrees_left = today * 12 - moon_phase

  # 3. Compute longitudinal differences at intervals of 0.25 days from sunrise
  offsets = c(0.25,0.5,0.75,1.0)
  lunar_longitude_diff = c()
  solar_longitude_diff = c()
  relative_motion = c()
  for(i in 1:length(offsets)){
    lunar_longitude_diff <- append(lunar_longitude_diff,((moon_longitude(rise + offsets[i]) - moon_longitude(rise)) %% 360));
    solar_longitude_diff <- append(solar_longitude_diff,((sun_longitude(rise + offsets[i]) - sun_longitude(rise)) %% 360));
    relative_motion <- append(relative_motion,(lunar_longitude_diff[i]- solar_longitude_diff[i]))
  }
  # 4. Find end time by 4-point inverse Lagrange interpolation
  y = relative_motion
  x = offsets
  # Compute fraction of day (after sunrise) needed to traverse 'degrees_left'
  approx_end = inverse_lagrange(x,y,degrees_left)
  ends = (rise + approx_end - jd) * 24 + tz
  answer = c(as.integer(today),to_dms(ends))

  # 5. Check for skipped tithi
  moon_phase_tom = lunar_phase(rise + 1)
  tomorrow = ceiling(moon_phase_tom/12)
  if(((tomorrow-today) %% 30) > 1){
    # interpolate again with same (x,y)
    leap_tithi = today + 1
    degrees_left = leap_tithi * 12 - moon_phase
    approx_end = inverse_lagrange(x,y,degrees_left)
    ends = (rise + approx_end - jd) * 24 + tz
    answer <- append(answer,c(as.integer(leap_tithi),to_dms(ends)))
  }
  return (answer)
}

## -----------------------------------------------------------------------------
jd <- 2459778 #Julian day number
place <- c(15.34, 75.13, +5.5) #Latitude, Longitude and timezone of the location
tithi(jd,place)

## -----------------------------------------------------------------------------
tithi(gregorian_to_jd(17,6,2022),c(15.34, 75.13, +5.5))

## ----echo=FALSE---------------------------------------------------------------
for (i in 1:length(tithis)){
  print(paste(i," - ",tithis[i]))
}

## -----------------------------------------------------------------------------
jd <- 2459778 #Julian day number
place <- c(15.34, 75.13, +5.5)  #Latitude, Longitude and timezone of the location
get_tithi_name(jd,place)

## -----------------------------------------------------------------------------
get_tithi_name(gregorian_to_jd(17,6,2022),c(15.34, 75.13, +5.5))

## -----------------------------------------------------------------------------
vaara <- function(jd){
  return (as.integer(ceiling(jd + 1) %% 7) + 1)
}

## -----------------------------------------------------------------------------
vaara(2459778)

## -----------------------------------------------------------------------------
vaara(gregorian_to_jd(6,8,2022))

## ----echo=FALSE---------------------------------------------------------------
vaaras <- c(
  "Ravivar",
  "Somvar",
  "Mangalwar",
  "Budhwar",
  "Guruwar",
  "Shukrawar",
  "Shaniwar"
)
for (i in 1:length(vaaras)){
  print(paste(i," - ",vaaras[i]))
}

## -----------------------------------------------------------------------------
get_vaara_name(2459778)

## -----------------------------------------------------------------------------
get_vaara_name(gregorian_to_jd(6,8,2022))

## -----------------------------------------------------------------------------
nakshatra <- function(jd,place){
  #Nakshatra as -> 1 = Ashwini, 2 = Bharani, ..., 27 = Revati

  #Set Lahiri ayanamsa
  swe_set_sid_mode(SE$SIDM_LAHIRI,0,0)

  # 1. Find time of sunrise
  lat = place[1]
  lon = place[2]
  tz = place[3]
  rise = sunrise(jd,place)[1]-(tz/24)

  # Swiss Ephemeris always gives Sayana. So subtract ayanamsa to get Nirayana
  offsets = c(0.0,0.25,0.5,0.75,1.0)
  longitudes = c()
  for(i in 1:length(offsets)){
    longitudes <- append(longitudes,((moon_longitude(rise + offsets[i]) - swe_get_ayanamsa_ex_ut(rise,SE$FLG_SWIEPH + SE$FLG_NONUT)$daya) %% 360))
  }
  # 2. Today's nakshatra is when offset = 0
  # There are 27 Nakshatras spanning 360 degrees
  nak = ceiling(longitudes[1] * 27 / 360)

  # 3. Find end time by 5-point inverse Lagrange interpolation
  y = unwrap_angles(longitudes)
  x = offsets
  approx_end = inverse_lagrange(x,y,nak * 360/27)
  ends = (rise - jd + approx_end) * 24 + tz
  answer = c(as.integer(nak),to_dms(ends))

  # 4. Check for skipped nakshatra
  nak_tmrw = ceiling(longitudes[length(longitudes)-1] * 27 / 360)
  if(((nak_tmrw - nak) %% 27) > 1){
    leap_nak = nak + 1
    approx_end = inverse_lagrange(offsets,longitudes,leap_nak*360/27)
    ends = (rise - jd + approx_end) * 24 + tz
    answer <- append(answer,c(as.integer(leap_nak),to_dms(ends)))
  }
  return (answer)
}


## -----------------------------------------------------------------------------
jd <- 2459778 #Julian day number
place <- c(15.34, 75.13, +5.5)  #Latitude, Longitude and timezone of the location
nakshatra(jd,place)

## -----------------------------------------------------------------------------
nakshatra(gregorian_to_jd(17,6,2022),c(15.34, 75.13, +5.5))

## ----echo=FALSE---------------------------------------------------------------
for (i in 1:length(nakshatras)){
  print(paste(i," - ",nakshatras[i]))
}

## -----------------------------------------------------------------------------
jd <- 2459778 #Julian day number
place <- c(15.34, 75.13, +5.5)  #Latitude, Longitude and timezone of the location
get_nakshatra_name(jd,place)

## -----------------------------------------------------------------------------
get_nakshatra_name(gregorian_to_jd(17,6,2022),c(15.34, 75.13, +5.5))

## -----------------------------------------------------------------------------
yoga <- function(jd,place){
  #Yoga as -> 1 = Vishkambha, 2 = Priti, ..., 27 = Vaidhrti
  swe_set_sid_mode(SE$SIDM_LAHIRI,0,0)

  # 1. Find time of sunrise
  lat = place[1]
  lon = place[2]
  tz = place[3]
  rise = sunrise(jd,place)[1]-(tz/24)

  # 2. Find the Nirayana longitudes and add them
  lunar_long = (moon_longitude(rise) - swe_get_ayanamsa_ex_ut(rise,SE$FLG_SWIEPH + SE$FLG_NONUT)$daya) %% 360
  solar_long = (sun_longitude(rise) - swe_get_ayanamsa_ex_ut(rise,SE$FLG_SWIEPH + SE$FLG_NONUT)$daya) %% 360
  total = (lunar_long + solar_long) %% 360

  # There are 27 Yogas spanning 360 degrees
  yog = ceiling(total * 27 / 360)

  # 3. Find how many longitudes is there left to be swept
  degrees_left = yog * (360 / 27) - total

  # 4. Compute longitudinal sums at intervals of 0.25 days from sunrise
  offsets = c(0.25,0.5,0.75,1.0)
  lunar_longitude_diff = c()
  solar_longitude_diff = c()
  total_motion = c()

  for(i in 1:length(offsets)){
    lunar_longitude_diff <- append(lunar_longitude_diff,((moon_longitude(rise + offsets[i]) - moon_longitude(rise)) %% 360))
    solar_longitude_diff <- append(solar_longitude_diff,((sun_longitude(rise + offsets[i]) - sun_longitude(rise)) %% 360))
    total_motion <- append(total_motion,(lunar_longitude_diff[i] + solar_longitude_diff[i]))
  }
  # 5. Find end time by 4-point inverse Lagrange interpolation
  y = total_motion
  x = offsets
  # compute fraction of day (after sunrise) needed to traverse 'degrees_left'
  approx_end = inverse_lagrange(x, y, degrees_left)
  ends = (rise + approx_end - jd) * 24 + tz
  answer = c(as.integer(yog),to_dms(ends))

  # 5. Check for skipped yoga
  lunar_long_tmrw = (moon_longitude(rise + 1) - swe_get_ayanamsa_ex_ut(rise + 1,SE$FLG_SWIEPH + SE$FLG_NONUT)$daya) %% 360
  solar_long_tmrw = (sun_longitude(rise + 1) - swe_get_ayanamsa_ex_ut(rise + 1,SE$FLG_SWIEPH + SE$FLG_NONUT)$daya) %% 360
  total_tmrw = (lunar_long_tmrw + solar_long_tmrw) %% 360
  tomorrow = ceiling(total_tmrw * 27 / 360)
  if(((tomorrow - yog) %% 27) > 1){
    # interpolate again with same (x,y)
    leap_yog = yog + 1
    degrees_left = leap_yog * (360 / 27) - total
    approx_end = inverse_lagrange(x, y, degrees_left)
    ends = (rise + approx_end - jd) * 24 + tz
    answer <- append(answer,c(as.integer(leap_yog),to_dms(ends)))
  }
  return (answer)
}

## -----------------------------------------------------------------------------
jd <- 2459778 #Julian day number
place <- c(15.34, 75.13, +5.5)  #Latitude, Longitude and timezone of the location
yoga(jd,place)

## -----------------------------------------------------------------------------
yoga(gregorian_to_jd(17,6,2022),c(15.34, 75.13, +5.5))

## ----echo=FALSE---------------------------------------------------------------
for (i in 1:length(yogas)){
  print(paste(i," - ",yogas[i]))
}

## -----------------------------------------------------------------------------
jd <- 2459778 #Julian day number
place <- c(15.34, 75.13, +5.5) #Latitude, Longitude and timezone of the location
get_yoga_name(jd,place)

## -----------------------------------------------------------------------------
get_yoga_name(gregorian_to_jd(17,6,2022),c(15.34, 75.13, +5.5))

## -----------------------------------------------------------------------------
karana <- function(jd,place){
  tithi_ = tithi(jd,place)
  answer <- c((tithi_[1] * 2) - 1,tithi_[1] * 2)
  return(answer)
}

## -----------------------------------------------------------------------------
jd <- 2459778 #Julian day number
place <- c(15.34, 75.13, +5.5) #Latitude, Longitude and timezone of the location
karana(jd,place)

## -----------------------------------------------------------------------------
karana(gregorian_to_jd(17,6,2022),c(15.34, 75.13, +5.5))

## -----------------------------------------------------------------------------
jd <- 2459778 #Julian day number
place <- c(15.34, 75.13, +5.5) #Latitude, Longitude and timezone of the location
get_karana_name(jd,place)

## -----------------------------------------------------------------------------
get_karana_name(gregorian_to_jd(17,6,2022),c(15.34, 75.13, +5.5))

## -----------------------------------------------------------------------------
rashi <- function(jd){
  swe_set_sid_mode(SE$SIDM_LAHIRI,0,0)
  s = moon_longitude(jd)
  lunar_nirayana = (moon_longitude(jd) - swe_get_ayanamsa_ex_ut(jd,SE$FLG_SWIEPH + SE$FLG_NONUT)$daya) %% 360
  return (ceiling(lunar_nirayana / 30))
}

## -----------------------------------------------------------------------------
jd <- gregorian_to_jd(17,6,2022) #Julian day number
rashi(jd)

## ----echo=FALSE---------------------------------------------------------------
for (i in 1:length(rashis)){
  print(paste(i," - ",rashis[i]))
}

## -----------------------------------------------------------------------------
jd <- gregorian_to_jd(17,6,2022) #Julian day number
get_rashi_name(jd)

## -----------------------------------------------------------------------------
lagna <- function(jd){
  swe_set_sid_mode(SE$SIDM_LAHIRI,0,0)
  s = sun_longitude(jd)
  solar_nirayana = (sun_longitude(jd) - swe_get_ayanamsa_ex_ut(jd,SE$FLG_SWIEPH + SE$FLG_NONUT)$daya) %% 360
  return (ceiling(solar_nirayana / 30))
}

## -----------------------------------------------------------------------------
jd <- gregorian_to_jd(17,6,2022) #Julian day number
lagna(jd)

## -----------------------------------------------------------------------------
jd <- gregorian_to_jd(17,6,2022) #Julian day number
get_lagna_name(jd)

## -----------------------------------------------------------------------------
masa <- function(jd,place){
  #Masa as -> 1 = Chaitra, 2 = Vaisakha, ..., 12 = Phalguna
  ti = tithi(jd,place)[1]
  critical = sunrise(jd,place)[1]
  last_new_moon = new_moon(critical,ti,-1)
  next_new_moon = new_moon(critical,ti,+1)
  this_solar_month = lagna(last_new_moon)
  next_solar_month = lagna(next_new_moon)
  is_leap_month = (this_solar_month == next_solar_month)
  maasa = this_solar_month + 1
  if(maasa > 12){
    maasa = maasa %% 12
  }
  return (c(as.integer(maasa),is_leap_month))
}

## -----------------------------------------------------------------------------
jd <- 2459778 #Julian day number
place <- c(15.34, 75.13, +5.5) #Latitude, Longitude and timezone of the location
masa(jd,place)

## -----------------------------------------------------------------------------
masa(gregorian_to_jd(17,6,2022),c(15.34, 75.13, +5.5))

## ----echo=FALSE---------------------------------------------------------------
for (i in 1:length(masas)){
  print(paste(i," - ",masas[i]))
}

## -----------------------------------------------------------------------------
jd <- 2459778 #Julian day number
place <- c(15.34, 75.13, +5.5) #Latitude, Longitude and timezone of the location
get_masa_name(jd,place)

## -----------------------------------------------------------------------------
get_masa_name(gregorian_to_jd(17,6,2022),c(15.34, 75.13, +5.5))

## -----------------------------------------------------------------------------
ritu <- function(masa_num){
  return (((masa_num - 1) %/% 2) + 1)
}

## -----------------------------------------------------------------------------
masa_num <- masa(gregorian_to_jd(17,6,2022),c(15.34, 75.13, +5.5))[1]
ritu(masa_num)

## ----echo=FALSE---------------------------------------------------------------
for (i in 1:length(ritus)){
  print(paste(i," - ",ritus[i]))
}

## -----------------------------------------------------------------------------
masa_num <- masa(gregorian_to_jd(17,6,2022),c(15.34, 75.13, +5.5))[1]
get_ritu_name(masa_num)

## -----------------------------------------------------------------------------
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

## -----------------------------------------------------------------------------
jd <- gregorian_to_jd(17,6,2022) #Julian day number

#Number associated with the masa
masa_num <- masa(gregorian_to_jd(17,6,2022),c(15.34, 75.13, +5.5))[1] 

samvatsara(jd,masa_num)

## ----echo=FALSE---------------------------------------------------------------
for (i in 1:length(samvatsars)){
  print(paste(i," - ",samvatsars[i]))
}

## -----------------------------------------------------------------------------
jd <- gregorian_to_jd(17,6,2022) #Julian day number

#Number associated with the masa
masa_num <- masa(gregorian_to_jd(17,6,2022),c(15.34, 75.13, +5.5))[1]

get_samvatsara_name(jd,masa_num)

## -----------------------------------------------------------------------------
gregorian_to_jd(17,6,2022) #In dd,mm,yyyy

## -----------------------------------------------------------------------------
jd_to_gregorian(2459778)

## -----------------------------------------------------------------------------
sun_longitude(2459778)

## -----------------------------------------------------------------------------
sun_longitude(gregorian_to_jd(17,6,2022))

## -----------------------------------------------------------------------------
moon_longitude(2459778)

## -----------------------------------------------------------------------------
moon_longitude(gregorian_to_jd(17,6,2022))

## -----------------------------------------------------------------------------
jd <- 2459778 #Julian day number
place <- c(15.34, 75.13, +5.5) #Latitude, Longitude and timezone of the location
sunrise(jd,place)

## -----------------------------------------------------------------------------
jd <- gregorian_to_jd(17,6,2022) #Julian day number
place <- c(15.34, 75.13, +5.5) #Latitude, Longitude and timezone of the location
sunrise(jd,place)

## -----------------------------------------------------------------------------
jd <- 2459778 #Julian day number
place <- c(15.34, 75.13, +5.5) #Latitude, Longitude and timezone of the location
sunset(jd,place)

## -----------------------------------------------------------------------------
jd <- gregorian_to_jd(17,6,2022) #Julian day number
place <- c(15.34, 75.13, +5.5) #Latitude, Longitude and timezone of the location
sunset(jd,place)

## -----------------------------------------------------------------------------
jd <- 2459778 #Julian day number
place <- c(15.34, 75.13, +5.5) #Latitude, Longitude and timezone of the location
moonrise(jd,place)

## -----------------------------------------------------------------------------
jd <- gregorian_to_jd(17,6,2022) #Julian day number
place <- c(15.34, 75.13, +5.5) #Latitude, Longitude and timezone of the location
moonrise(jd,place)

## -----------------------------------------------------------------------------
jd <- 2459778 #Julian day number
place <- c(15.34, 75.13, +5.5) #Latitude, Longitude and timezone of the location
moonset(jd,place)

## -----------------------------------------------------------------------------
jd <- gregorian_to_jd(17,6,2022) #Julian day number
place <- c(15.34, 75.13, +5.5) #Latitude, Longitude and timezone of the location
moonset(jd,place)

