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

