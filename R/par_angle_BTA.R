#' Parallactic angle
#'
#' Returns parallactic angle calculated from Alpha, Delta and Stime for BTA location
#'
#' @param Alpha a character string (hh:mm:ss)
#' @param Delta a character string (dd:mm:ss)
#' @param Stime a character string (hh:mm:ss)
#' @return Parallactic angle in degrees.
#' @examples
#' par_angle_BTA(Alpha = '00:33:32.7', Delta = '55:18:59.5', Stime = '06:53:00')
#' \dontrun{
#' par_angle_BTA()
#' }
#' @export
par_angle_BTA <- function(Alpha = NULL, Delta = NULL, Stime = NULL) {

  BTA_latitude_rad <- 43.6467 * pi / 180 # BTA latitude in rad

  if(is.null(Alpha)) {
    Alpha <- readline(prompt = 'Enter Alpha (hh:mm:ss): ')
  }
  Alpha_rad <- celestial::hms2deg(Alpha) * pi / 180

  if(is.null(Delta)) {
    Delta <- readline(prompt = 'Enter Delta (dd:mm:ss): ')
  }
  Delta_rad <- celestial::dms2deg(Delta) * pi / 180

  if(is.null(Stime)) {
    Stime <- readline(prompt = 'Enter Stime (hh:mm:ss): ')
  }
  Stime_rad <- celestial::hms2deg(Stime) * pi / 180

  Hour_angle_rad <- Stime_rad - Alpha_rad

  Q_degr <- atan(sin(Hour_angle_rad) / (tan(BTA_latitude_rad) * cos(Delta_rad) -
    sin(Delta_rad) * cos(Hour_angle_rad))) * 180 / pi

  return(Q_degr)
}
