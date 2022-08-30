# ---------------------------------------------------------------------------- #
#                             VedicDateTime package                            #
# ---------------------------------------------------------------------------- #

#Use Swiss ephemeris package for the calculations
install.packages("swephR",repos = "https://CRAN.R-project.org/package=swephR")
library(swephR)
# Load the SE data set from Swiss Ephemeris
data(SE)
# Used to print values with 22 significant digits
options(digits = 22)


# ---------------------------------------------------------------------------- #
#' moon_longitude
#'
#' @description Get Lunar longitude for a given Julian day number.
#'
#' @param jd Julian day
#'
#' @return Lunar longitude for \code{jd}
#'
#' @examples
#' moon_longitude(2459778)
#' moon_longitude(2459500)
moon_longitude <- function(jd){
  return (swe_calc_ut(jd, SE$MOON, SE$FLG_SWIEPH)$xx[1])
}
# ---------------------------------------------------------------------------- #


# ---------------------------------------------------------------------------- #
#' sun_longitude
#'
#' @description Get Solar longitude for a given Julian day number.
#'
#' @param jd Julian day
#'
#' @return Solar longitude for \code{jd}
#'
#' @examples
#' sun_longitude(2459778)
#' sun_longitude(2459500)
sun_longitude <- function(jd){
  return (swe_calc_ut(jd, SE$SUN, SE$FLG_SWIEPH)$xx[1])
}
# ---------------------------------------------------------------------------- #


# ---------------------------------------------------------------------------- #
#' from_dms
#'
#' @description Convert degrees, minutes, and seconds to decimal degrees
#'
#' @param degs Degrees
#' @param mins Minutes
#' @param secs Seconds
#'
#' @return Degrees as a decimal number
#'
#' @examples
#' from_dms(30,15,50)
from_dms <- function(degs,mins,secs){
  return (degs + mins/60 + secs/3600)
}
# ---------------------------------------------------------------------------- #


# ---------------------------------------------------------------------------- #
#' to_dms
#'
#' @description Convert decimal degrees to degrees, minutes, and seconds
#'
#' @param deg Degrees as a decimal number
#'
#' @return A vector containing degrees, minutes and seconds
#'
#' @examples
#' to_dms(30.263888889)
to_dms <- function(deg){
  d = as.integer(deg)
  mins = (deg - d) * 60
  m = as.integer(mins)
  s = as.integer(round((mins - m) * 60))
  return (c(d, m, s))
}
# ---------------------------------------------------------------------------- #


# ---------------------------------------------------------------------------- #
#' unwrap_angles
#'
#' @description Add 360 degs an element in the input vector if elements are not sorted in ascending order.
#'
#' @param angles Vector containing angles
#'
#' @return angles in ascending order
#'
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
# ---------------------------------------------------------------------------- #


# ---------------------------------------------------------------------------- #
#' inverse_lagrange
#'
#' @description Given two vectors x and y, find the value of x = xa when y = ya, i.e., f(xa) = ya
#'
#' @param x Vector x
#' @param y Vector y
#' @param ya
#'
#' @return Value of xa
#'
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
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
#' gregorian_to_jd
#'
#' @description Convert Gregorian date to Julian day number at 00:00 UTC
#'
#' @param day Day number
#' @param month Month number
#' @param year Year number
#'
#' @return Julian day number
#'
#' @examples
#' gregorian_to_jd(18,7,2022)
gregorian_to_jd <- function(day,month,year){
  return (swe_julday(year, month, day, 0.0,SE$GREG_CAL))
}
# ---------------------------------------------------------------------------- #


# ---------------------------------------------------------------------------- #
#' jd_to_gregorian
#'
#' @description Convert Julian day number to Gregorian date
#'
#' @param jd Julian day number
#'
#' @return Gregorian date
#'
#' @examples
#' jd_to_gregorian(2459778)
jd_to_gregorian <- function(jd){
  return (swe_revjul(jd, SE$GREG_CAL))
}
# ---------------------------------------------------------------------------- #


# ---------------------------------------------------------------------------- #
#' sunrise
#'
#' @description Sunrise for a given date and place
#'
#' @param jd Julian day number
#' @param place Vector containing latitude, longitude and timezone
#'
#' @return Sunrise as Julian day number
#'
#' @examples
#' sunrise(2459778,c(15.34, 75.13, +5.5))
sunrise <- function(jd,place){
  lat = place[1]
  lon = place[2]
  tz = place[3]
  result <- swe_rise_trans_true_hor(jd - (tz/24.0),SE$SUN,"",SE$FLG_SWIEPH,SE$BIT_DISC_CENTER + SE$CALC_RISE,c(lon,lat,0),0,0,0)
  rise <- result$tret
  return (c(rise + (tz)/24.0,to_dms((rise - jd) * 24 + tz)))
}
# ---------------------------------------------------------------------------- #


# ---------------------------------------------------------------------------- #
#' sunset
#' @description Sunset for a given date and place
#'
#' @param jd Julian day number
#' @param place Vector containing latitude, longitude and timezone
#'
#' @return Sunset as Julian day number
#'
#' @examples
#' sunset(2459778,c(15.34, 75.13, +5.5))
sunset <- function(jd,place){
  lat = place[1]
  lon = place[2]
  tz = place[3]
  result <- swe_rise_trans_true_hor(jd - (tz/24.0),SE$SUN,"",SE$FLG_SWIEPH,SE$BIT_DISC_CENTER + SE$CALC_SET,c(lon,lat,0),0,0,0)
  setting <- result$tret #Julian day number
  #Convert to the given timezone
  return (c(setting + (tz)/24,to_dms((setting - jd) * 24 + tz)))
}
# ---------------------------------------------------------------------------- #


# ---------------------------------------------------------------------------- #
#' moonrise
#'
#' @description Moonrise for a given date and place
#'
#' @param jd Julian day number
#' @param place Vector containing latitude, longitude and timezone
#'
#' @return Moonrise as Julian day number
#'
#' @examples
#' moonrise(2459778,c(15.34, 75.13, +5.5))
moonrise <- function(jd,place){
  lat = place[1]
  lon = place[2]
  tz = place[3]
  result <- swe_rise_trans_true_hor(jd - (tz/24.0),SE$MOON,"",SE$FLG_SWIEPH,SE$BIT_DISC_CENTER + SE$CALC_RISE,c(lon,lat,0),0,0,0)
  rise <- result$tret #Julian day number
  #Convert to the given timezone
  return (to_dms((rise - jd) * 24 + tz))
}
# ---------------------------------------------------------------------------- #


# ---------------------------------------------------------------------------- #
#' moonset
#'
#' @description Moonset for a given date and place
#'
#' @param jd Julian day number
#' @param place Vector containing latitude, longitude and timezone
#'
#' @return Moonset as Julian day number
#'
#' @examples
#' moonset(2459778,c(15.34, 75.13, +5.5))
moonset <- function(jd,place){
  lat = place[1]
  lon = place[2]
  tz = place[3]
  result <- swe_rise_trans_true_hor(jd - (tz/24.0),SE$MOON,"",SE$FLG_SWIEPH,SE$BIT_DISC_CENTER + SE$CALC_SET,c(lon,lat,0),0,0,0)
  setting <- result$tret #Julian day number
  #Convert to the given timezone
  return (to_dms((setting - jd) * 24 + tz))
}
# ---------------------------------------------------------------------------- #


# ---------------------------------------------------------------------------- #
#' lunar_phase
#'
#' @description Lunar phase for a given Julian day number
#'
#' @param jd Julian day number
#'
#' @return Lunar phase
#'
#' @examples
#' lunar_phase(2459778)
lunar_phase <- function(jd){
  sl = sun_longitude(jd)
  ll = moon_longitude(jd)
  moon_phase <- ((ll-sl) %% 360)
  return (moon_phase)
}
# ---------------------------------------------------------------------------- #


# ---------------------------------------------------------------------------- #
#' tithi
#'
#' @description Tithi for a given place and time
#'
#' @param jd Julian day number
#' @param place Vector containing latitude, longitude and timezone
#'
#' @return Tithi and it's ending time
#'
#' @examples
#' tithi(2459778,c(15.34, 75.13, +5.5))
#' tithi(gregorian_to_jd(17,6,2022),c(15.34, 75.13, +5.5))
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
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
#' nakshatra
#'
#' @description Nakshatra for a given place and time
#'
#' @param jd Julian day number
#' @param place Vector containing latitude, longitude and timezone
#'
#' @return Nakshatra and it's ending time
#'
#' @examples
#' nakshatra(2459778,c(15.34, 75.13, +5.5))
#' nakshatra(gregorian_to_jd(17,6,2022),c(15.34, 75.13, +5.5))
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
# ---------------------------------------------------------------------------- #

nakshatra(swe_julday(2022,6,30,0,SE$GREG_CAL),c(15.34, 75.13, +5.5))

# ---------------------------------------------------------------------------- #
#' yoga
#'
#' @description Yoga for a given place and time
#'
#' @param jd Julian day number
#' @param place Vector containing latitude, longitude and timezone
#'
#' @return Yoga and it's ending time
#'
#' @examples
#' yoga(2459778,c(15.34, 75.13, +5.5))
#' yoga(gregorian_to_jd(17,6,2022),c(15.34, 75.13, +5.5))
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
# ---------------------------------------------------------------------------- #


yoga(swe_julday(2022,6,17,0,SE$GREG_CAL),c(15.34, 75.13, +5.5))


# ---------------------------------------------------------------------------- #
#' karana
#'
#' @description Karana for a given place and time
#'
#' @param jd Julian day number
#' @param place Vector containing latitude, longitude and timezone
#'
#' @return Two karanas
#'
#' @examples
#' karana(2459778,c(15.34, 75.13, +5.5))
#' karana(gregorian_to_jd(17,6,2022),c(15.34, 75.13, +5.5))
karana <- function(jd,place){
  tithi_ = tithi(jd,place)
  answer <- c((tithi_[1] * 2) - 1,tithi_[1] * 2)
  return(answer)
}
# ---------------------------------------------------------------------------- #

karana(swe_julday(2022,7,14,0,SE$GREG_CAL),c(15.34, 75.13, +5.5))

# ---------------------------------------------------------------------------- #
#' vaara
#'
#' @description Vaara for a given Julian day number
#' @param jd Julian day number
#'
#' @return Vaara as an integer
#'
#' @examples
#' vaara(2459778)
vaara <- function(jd){
  return (as.integer(ceiling(jd + 1) %% 7) + 1)
}
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
#' Lagna
#'
#' @description Lagna for a given Julian day number
#'
#' @param jd Julian day number
#'
#' @return Lagna as an integer
#'
#' @examples
#' lagna(2459778)
#' lagna(gregorian_to_jd(30,8,2022))
lagna <- function(jd){
  swe_set_sid_mode(SE$SIDM_LAHIRI,0,0)
  s = sun_longitude(jd)
  solar_nirayana = (sun_longitude(jd) - swe_get_ayanamsa_ex_ut(jd,SE$FLG_SWIEPH + SE$FLG_NONUT)$daya) %% 360
  return (ceiling(solar_nirayana / 30))
}
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
#' Rashi
#'
#' @description Rashi for a given Julian day number
#'
#' @param jd Julian day number
#'
#' @return Rashi as an integer
#'
#' @examples
#' rashi(2459778)
#' rashi(gregorian_to_jd(30,8,2022))
rashi <- function(jd){
  swe_set_sid_mode(SE$SIDM_LAHIRI,0,0)
  s = moon_longitude(jd)
  lunar_nirayana = (moon_longitude(jd) - swe_get_ayanamsa_ex_ut(jd,SE$FLG_SWIEPH + SE$FLG_NONUT)$daya) %% 360
  return (ceiling(lunar_nirayana / 30))
}
# ---------------------------------------------------------------------------- #


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


# ---------------------------------------------------------------------------- #
#' masa
#'
#' @description Masa for a given place and time
#'
#' @param jd Julian day number
#' @param place Vector containing latitude, longitude and timezone
#'
#' @return Masa number and whether it is adhika masa or not
#'
#' @examples
#' masa(2459778,c(15.34, 75.13, +5.5))
#' masa(swe_julday(2022,7,14,0,SE$GREG_CAL),c(15.34, 75.13, +5.5))
masa <- function(jd,place){
  #Masa as -> 1 = Chaitra, 2 = Vaisakha, ..., 12 = Phalguna
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
# ---------------------------------------------------------------------------- #

masa(swe_julday(2022,6,17,0,SE$GREG_CAL),c(15.34, 75.13, +5.5))

# ---------------------------------------------------------------------------- #
#' ahargana
#'
#' @param jd Julian day number
#'
#' @return Ahargana
#'
#' @examples
#' ahargana(2459778)
#' ahargana(gregorian_to_jd(30,8,2022))
ahargana <- function(jd){
  return (jd - 588465.5)
}
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
#' elapsed_year
#'
#' @param jd Julian Day number
#' @param maasa_num Number indicating the Maasa
#'
#' @return A vector containing Kali, Saka, and Vikram Samvat
#'
#' @examples
#' elapsed_year(2459778,2)
elapsed_year <- function(jd,maasa_num){
  sidereal_year = 365.25636
  ahar = ahargana(jd)
  kali = as.integer((ahar + (4 - maasa_num) * 30) / sidereal_year)
  saka = kali - 3179
  vikrama = saka + 135
  return (c(kali, saka, vikrama))
}
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
#' samvatsara
#'
#' @description Shaka Samvatsar for a given Julian day number and maasa number.
#' @param jd Julian day number
#' @param maasa_num Maasa number
#'
#' @return Number associated with the Shaka Samvatsar
#'
#' @examples
#' samvatsara(2459778,2)
samvatsara <- function(jd,maasa_num){
  kali = elapsed_year(jd,maasa_num)[1]
  if(kali >= 4009){
    kali = (kali - 14) %% 60
  }
  samvat = (kali + 27 + as.integer((kali * 211 - 108)/18000)) %% 60
  return (samvat)
}
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
#' get_samvatsara_name
#'
#' @description Name of the Shaka Samvatsar for a given Julian day number and maasa number.
#' @param jd Julian day number
#' @param maasa_num Maasa number
#'
#' @return Shaka Samvatsar
#'
#' @examples
#' get_samvatsara_name(2459778,2)
get_samvatsara_name <- function(jd,maasa_num){
  return (samvatsars[samvatsara(jd,maasa_num)])
}
# ---------------------------------------------------------------------------- #


# ---------------------------------------------------------------------------- #
#' ritu
#'
#' @param masa_num Number associated with a Masa
#'
#' @return Number associated with the Ritu
#'
#' @examples
#' ritu(2)
ritu <- function(masa_num){
  return (((masa_num - 1) %/% 2) + 1)
}
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
#' get_ritu_name
#'
#' @param masa_num Number associated with a Masa
#'
#' @return Ritu's name
#'
#' @examples
#' ritu(2)
get_ritu_name <- function(masa_num){
  return (ritus[ritu(masa_num)])
}
# ---------------------------------------------------------------------------- #


# ---------------------------------------------------------------------------- #
#' day_duration
#'
#' @description Duration of the day for a given place and time
#'
#' @param jd Julian day number
#' @param place Vector containing latitude, longitude and timezone
#'
#' @return Vector containing the length of the day & in dms
#'
#' @examples
#' day_duration(2459778,c(15.34, 75.13, +5.5))
#' day_duration(gregorian_to_jd(30,8,2022),c(15.34, 75.13, +5.5))
day_duration <- function(jd,place){
  srise = sunrise(jd,place)[1]
  sset = sunrise(jd,place)[1]
  diff = (sset - srise) * 24
  return (c(diff,to_dms(diff)))
}
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
#' get_masa_name
#' @description Get name of the Masa for given Julian day number and place.
#'
#' @param jd Julian day number
#' @param place Vector containing latitude, longitude and timezone
#'
#' @return Name of the Masa
#'
#' @examples
#' get_masa_name(2459778,c(15.34, 75.13, +5.5))
#' get_masa_name(gregorian_to_jd(30,8,2022),c(15.34, 75.13, +5.5))
get_masa_name <- function(jd,place){
  masa_ = masa(jd,place)
  masa_name = ""
  if(masa_[2] == 1){
    masa_name = "Adhika "
  }
  masa_name <- paste(masa_name,masas[masa_[1]],sep = "")
  return (masa_name)
}
# ---------------------------------------------------------------------------- #

get_masa_name(swe_julday(2022,7,29,0,SE$GREG_CAL),c(15.34, 75.13, +5.5))

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
#' get_tithi_name(gregorian_to_jd(30,8,2022),c(15.34, 75.13, +5.5))
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
#' get_nakshatra_name(gregorian_to_jd(30,8,2022),c(15.34, 75.13, +5.5))
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

# ---------------------------------------------------------------------------- #
#' get_karana_name
#' @description Get name(s) of the Karana for given Julian day number and place.
#'
#' @param jd Julian day number
#' @param place Vector containing latitude, longitude and timezone
#'
#' @return Name(s) of the Karana.
#'
#' @examples
#' get_karana_name(2459778,c(15.34, 75.13, +5.5))
#' get_karana_name(gregorian_to_jd(30,8,2022),c(15.34, 75.13, +5.5))
get_karana_name <- function(jd,place){
  karana_ = karana(jd,place)
  karana_name = paste(karanas[karana_[1]],karanas[karana_[2]],sep = "-")
  return(karana_name)
}
# ---------------------------------------------------------------------------- #


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
#' get_yoga_name(gregorian_to_jd(30,8,2022),c(15.34, 75.13, +5.5))
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


# ---------------------------------------------------------------------------- #
#' get_rashi_name
#'
#' @description Get name of the Rashi for given Julian day number.
#'
#' @param jd Julian day number
#'
#' @return Name of the Rashi.
#'
#' @examples
#' get_rashi_name(2459778)
#' get_rashi_name(gregorian_to_jd(30,8,2022))
get_rashi_name <- function(jd){
  return(rashis[rashi(jd)])
}
# ---------------------------------------------------------------------------- #


# ---------------------------------------------------------------------------- #
#' get_lagna_name
#' @description Get name of the Lagna for given Julian day number.
#'
#' @param jd Julian day number
#'
#' @return Name of the lagna.
#'
#' @examples
#' get_lagna_name(2459778)
#' get_lagna_name(gregorian_to_jd(30,8,2022))
get_lagna_name <- function(jd){
  return(raasis[lagna(jd)])
}
# ---------------------------------------------------------------------------- #


# ---------------------------------------------------------------------------- #
#' get_vaara_name
#' @description Get name of the Vaara for given Julian day number.
#'
#' @param jd Julian day number
#'
#' @return Name of the Vaara.
#'
#' @examples
#' get_vaara_name(2459778)
#' get_vaara_name(gregorian_to_jd(30,8,2022))
get_vaara_name <- function(jd){
  return(varas[vaara(jd)])
}
# ---------------------------------------------------------------------------- #


# ---------------------------------------------------------------------------- #
# ------------------------------- Tests -------------------------------------- #
# ---------------------------------------------------------------------------- #
jul21 <- swe_julday(2022,7,21,0,SE$GREG_CAL) #Julian day number
gulbarga <- c(17.320486,76.839752,+5.5) #Place with lat, long and tz

get_tithi_name(jul21,gulbarga)
get_nakshatra_name(jul21,gulbarga)
get_yoga_name(jul21,gulbarga)
get_karana_name(jul21,gulbarga)
get_raasi_name(jul21)
get_vaara_name(jul21)

masas[masa(jul21,gulbarga)]
ritus[ritu(masa(jul21,gulbarga))]

samvatsars[samvatsara(jul21,masa(jul21,gulbarga))]


# Can be verified from ->
# https://www.drikpanchang.com/panchang/month-panchang.html?geoname-id=1270752&date=21/07/2022
# ---------------------------------------------------------------------------- #
