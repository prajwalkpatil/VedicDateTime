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
  swephR::swe_set_sid_mode(swephR::SE$SIDM_LAHIRI,0,0)

  # 1. Find time of sunrise
  lat = place[1]
  lon = place[2]
  tz = place[3]
  rise = sunrise(jd,place)[1]-(tz/24)

  # 2. Find the Nirayana longitudes and add them
  lunar_long = (moon_longitude(rise) - swephR::swe_get_ayanamsa_ex_ut(rise,swephR::SE$FLG_SWIEPH + swephR::SE$FLG_NONUT)$daya) %% 360
  solar_long = (sun_longitude(rise) - swephR::swe_get_ayanamsa_ex_ut(rise,swephR::SE$FLG_SWIEPH + swephR::SE$FLG_NONUT)$daya) %% 360
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
  lunar_long_tmrw = (moon_longitude(rise + 1) - swephR::swe_get_ayanamsa_ex_ut(rise + 1,swephR::SE$FLG_SWIEPH + swephR::SE$FLG_NONUT)$daya) %% 360
  solar_long_tmrw = (sun_longitude(rise + 1) - swephR::swe_get_ayanamsa_ex_ut(rise + 1,swephR::SE$FLG_SWIEPH + swephR::SE$FLG_NONUT)$daya) %% 360
  total_tmrw = (lunar_long_tmrw + solar_long_tmrw) %% 360
  tomorrow = ceiling(total_tmrw * 27 / 360)
  if(((tomorrow - yog) %% 27) > 1){
    # interpolate again with same (x,y)
    leap_yog = (yog + 1)
    degrees_left = leap_yog * (360 / 27) - total
    approx_end = inverse_lagrange(x, y, degrees_left)
    ends = (rise + approx_end - jd) * 24 + tz
    if(leap_yog >= 28){
      leap_yog = (leap_yog %% 28) + 1
    }
    answer <- append(answer,c(as.integer(leap_yog),to_dms(ends)))
  }
  return (answer)
}
# ---------------------------------------------------------------------------- #
