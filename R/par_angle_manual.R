#' Calculate parallactic angles from manually entered data
#'
#' @return Parallactic angle in degrees from manually entered data.
#' @examples
#' par_angle_manual()
#' @export
par_angle_manual <- function() {

  # BTA_latitude_rad <- 0.761894396137 # +43 deg 39' 12''
  BTA_latitude_rad <- 43.6467 * pi / 180 # Dyachenko values
  Alpha <- readline(prompt = 'Enter Alpha (hh:mm:ss): ')
  Alpha_rad <- celestial::hms2deg(Alpha) * pi / 180

  Delta <- readline(prompt = 'Enter Delta (dd:mm:ss): ')
  Delta_rad <- celestial::dms2deg(Delta) * pi / 180

  Stime <- readline(prompt = 'Enter Stime (hh:mm:ss): ')
  Stime_rad <- celestial::hms2deg(Stime) * pi / 180

  Hour_angle_rad = Stime_rad - Alpha_rad

  Q_degr = atan(sin(Hour_angle_rad) / (tan(BTA_latitude_rad) * cos(Delta_rad) -
                                     sin(Delta_rad) * cos(Hour_angle_rad))) * 180 / pi

  return(Q_degr)
}
