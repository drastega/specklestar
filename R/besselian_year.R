#' Besselian year
#'
#' Returns besselian year calculated from date and time
#'
#' @param date a character string (dd-Mon-year)
#' @param time a character string (hh:mm:ss)
#' @return Besselian year.
#' @examples
#' besselian_year('12-Dec-2009', '02:53:10')
#' @export
besselian_year <- function(date, time) {
  date_time <- lubridate::dmy_hms(paste(date, time, sep = ' '))

  hour <- lubridate::hour(date_time) + lubridate::minute(date_time) / 60 + lubridate::second(date_time) / 3600

  JD_TT <- 367 * lubridate::year(date_time) - (7 * (lubridate::year(date_time) + (lubridate::month(date_time) + 9) %/% 12)) %/% 4 +
    (275 * lubridate::month(date_time)) %/% 9 + lubridate::day(date_time) + 1721013.5 + hour / 24

  BY <- 1900.0 + (JD_TT - 2415020.31352) / 365.242198781
  return(BY)
}
