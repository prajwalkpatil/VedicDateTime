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
    if(leap_tithi >= 31){
      leap_tithi = (leap_tithi %% 31) + 1
    }
    answer <- append(answer,c(as.integer(leap_tithi),to_dms(ends)))
  }
  return (answer)
}
# ---------------------------------------------------------------------------- #
